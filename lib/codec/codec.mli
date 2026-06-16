(** Format-agnostic type description for serialization.

    [Codec] provides a {{!type:t} GADT} that describes the runtime structure
    of OCaml types. A value of type ['a t] is a {e type representation}: it
    carries no payload data, only structural information (field names,
    getters, constructors, etc.). It is allocated once per type and shared
    across all encode/decode operations.

    {b Drivers} (e.g. [Codec_yojson]) traverse OCaml values directly,
    guided by the type representation. There is no intermediate copy: the
    cost of an encode is proportional to the value, not doubled by a
    staging buffer.

    {2 Quick start}

    {[
      (* 1. Describe the type structure *)
      type user = { name : string; age : int }

      let user : user Codec.t =
        Codec.record "user" (fun name age -> { name; age })
        |> Codec.field "name" Codec.string (fun u -> u.name)
        |> Codec.field "age" Codec.int (fun u -> u.age)
        |> Codec.seal

      (* 2. Use any driver *)
      let json  = Codec_yojson.encode user my_user
      (* let yaml  = Codec_yaml.encode user my_user *)
    ]}

    {2 Design overview}

    {ul
    {- The GADT is {{!type:t} private}: drivers can pattern-match on it
       but only [Codec] can construct values (via {!val:int}, {!val:record},
       {!val:seal}, etc.).}
    {- {{!section:records} Records} use a type-safe applicative pipeline
       ([record |> field |> field |> seal]) where the compiler checks
       that every constructor argument is supplied.}
    {- {{!section:variants} Variants} are described by listing
       {{!type:case} cases}, each carrying a destructor (for encoding)
       and a constructor (for decoding).}
    {- {{!section:collections} Collections} (List, Array, Hashtbl,
       Set.Make, …) are all described by a single [Collection]
       constructor that exposes [iter] and a streaming [builder] —
       drivers walk the container in place during encoding and feed
       elements into the builder during decoding, with no list
       intermediate.}
    {- {{!section:driver} Drivers} implement {!module-type:DRIVER} by
       recursively matching on the GADT. The type indices guarantee that
       when a driver matches e.g. [Int], the value is statically known
       to be [int].}} *)

(** {1:errors Error handling} *)

(** Structured encoding/decoding errors with location tracking. *)
module Error : sig

  (** Location of an error within a nested structure.

      Each element is a field name, variant case name, or list index.
      For example [["user"; "address"; "city"]] points to
      [user.address.city]. *)
  type path = string list

  (** An encoding or decoding error. The type is abstract; use {!val:make}
      to construct and {!val:pp} or {!val:to_string} to display. *)
  type t

  (** [make ?expected ?got path msg] builds an error.

      @param expected  What was expected (e.g. ["int"]).
      @param got       What was found (e.g. ["string"]).
      @param path      Location within the value being processed.
      @param msg       Human-readable description. *)
  val make : ?expected:string -> ?got:string -> path -> string -> t

  (** Pretty-print an error. *)
  val pp : t Fmt.t

  (** [to_string e] is [Format.asprintf "%a" pp e]. *)
  val to_string : t -> string

  (** [with_path prefix result] prepends [prefix] to the error path if
      [result] is [Error _]. Useful in drivers to build nested error
      locations. *)
  val with_path : string -> ('a, t) result -> ('a, t) result

  (** [prepend_path prefix err] prepends [prefix] to [err]'s path.
      The bare-error analogue of {!val:with_path}; useful inside
      exception-based encoders/decoders. *)
  val prepend_path : string -> t -> t

  (** Raised by the [_exn] convenience functions in drivers. *)
  exception Codec_error of t
end

(** Alias for {!Error.t}. *)
type error = Error.t

(** {1:repr Type representation}

    A value of type ['a t] is a structural description of the OCaml type
    ['a]. It is {e private}: client code (including drivers) can
    pattern-match on the constructors to inspect the structure but cannot
    build values directly. Construction goes through the functions in
    {{!section:prims} Primitives}, {{!section:combinators} Combinators},
    {{!section:collections} Collections}, {{!section:records} Records},
    and {{!section:variants} Variants}. *)

type _ t = private
  | Unit : unit t
  | Bool : bool t
  | Int : int t
  | Int32 : int32 t
  | Int64 : int64 t
  | Float : float t
  | Char : char t
  | String : string t
  | Option : 'a t -> 'a option t
  | Tuple2 : 'a t * 'b t -> ('a * 'b) t
  | Tuple3 : 'a t * 'b t * 'c t -> ('a * 'b * 'c) t
  | Tuple4 : 'a t * 'b t * 'c t * 'd t -> ('a * 'b * 'c * 'd) t
  | Tuple5 : 'a t * 'b t * 'c t * 'd t * 'e t -> ('a * 'b * 'c * 'd * 'e) t
  | Tuple6 : 'a t * 'b t * 'c t * 'd t * 'e t * 'f t
      -> ('a * 'b * 'c * 'd * 'e * 'f) t
  | Collection : ('container, 'elem) collection_desc -> 'container t
      (** See {{!section:collections} Collections}. *)
  | Record : ('r, 'r) fields -> 'r t
      (** See {{!section:records} Records}. *)
  | Variant : 'v variant_desc -> 'v t
      (** See {{!section:variants} Variants}. *)
  | Map : ('a, 'b) map_desc -> 'b t
      (** See {!val:map}. *)
  | Lazy : 'a t lazy_t -> 'a t
      (** See {!val:lazy_}. *)

(** {2:collection_payload Collection description}

    A value of type [('container, 'elem) collection_desc] describes a
    homogeneous, ordered collection [‘container] of elements of type
    [‘elem]. It exposes two traversal primitives — one for encoding
    ([iter]), one for decoding ([builder]) — and the type representation
    of the elements.

    {ul
    {- {b Encoding.} The driver calls [iter f container]; [f] is invoked
       on each element in turn, and the driver encodes each as
       [element_codec] dictates. No intermediate list is materialized.}
    {- {b Decoding.} The driver calls [builder ()] to obtain a fresh
       pair [(sink, finalize)]. It feeds each decoded element into
       [sink], then calls [finalize ()] to obtain the reconstructed
       container.}}

    This design is uniform: [list], [array], [Hashtbl.t], [Queue.t],
    [Set.Make(_).t], … are all instances. *)
and ('container, 'elem) collection_desc = private {
  iter : ('elem -> unit) -> 'container -> unit;
      (** Push-style traversal used during encoding. *)
  builder : unit -> ('elem -> unit) * (unit -> 'container);
      (** Fresh accumulator factory used during decoding. Returns a
          [(sink, finalize)] pair: [sink] receives each decoded element
          in order, [finalize ()] produces the final container. *)
  element_codec : 'elem t;
      (** Type representation of each element. *)
}

(** {2:record_fields Record fields}

    A value of type [('f, 'r) fields] describes the fields of a record
    of type ['r]. The first parameter ['f] tracks the remaining arguments
    of the constructor function: it starts as the full constructor type
    and is peeled off one argument per {!val:field} call until it equals
    ['r], at which point {!val:seal} can close the description.

    Drivers walk this structure to encode and decode records:
    {ul
    {- {b Encoding.} For each [Field], call [get] on the record value
       and recursively encode the result according to [repr].}
    {- {b Decoding.} Read each field from the serialized form, decode it
       according to [repr], then apply the constructor from [F0]
       one argument at a time. If the field is absent and [default] is
       [Some v], use [v]; if [default] is [None], report an error.}} *)
and (_, _) fields = private
  | F0 : {
      record_name : string;  (** Name used in error messages. *)
      constructor : 'f;      (** Constructor function, fully or partially applied. *)
    } -> ('f, 'r) fields
  | Field : {
      rest : ('a -> 'f, 'r) fields;  (** Remaining fields. *)
      name : string;                  (** Serialized field name. *)
      repr : 'a t;                    (** Type representation of the field value. *)
      get : 'r -> 'a;                 (** Accessor: extracts this field from a record. *)
      default : 'a option;            (** [None]: required. [Some v]: default value. *)
    } -> ('f, 'r) fields

(** {2:variant_cases Variant cases}

    Each case carries enough information for a driver to encode and decode
    one constructor of a variant type.

    {ul
    {- [Case]: a constructor that carries a payload of type ['a].
       The driver uses [destruct] to try to extract the payload during
       encoding and [construct] to rebuild the variant during decoding.}
    {- [Case0]: a constant constructor with no payload. The driver uses
       [match_] to test equality during encoding and [value] to produce
       the constant during decoding.}} *)
and 'v case = private
  | Case : {
      name : string;              (** Serialized case name. *)
      repr : 'a t;                (** Payload type representation. *)
      destruct : 'v -> 'a option; (** [Some payload] if the value matches. *)
      construct : 'a -> 'v;       (** Rebuild the variant from a decoded payload. *)
    } -> 'v case
  | Case0 : {
      name : string;        (** Serialized case name. *)
      value : 'v;            (** The constant variant value. *)
      match_ : 'v -> bool;  (** [true] if the value matches this case. *)
    } -> 'v case

(** Description of a variant type: a name (for error messages) and an
    ordered list of {!type:case}s. During encoding the cases are tried in
    order; during decoding the driver looks up by name. *)
and 'v variant_desc = private {
  vname : string;
  cases : 'v case list;
}

(** Payload of the {!constructor:Map} constructor. See {!val:map}. *)
and ('a, 'b) map_desc = private {
  repr : 'a t;          (** Underlying type representation. *)
  forward : 'a -> 'b;   (** Conversion applied after decoding. *)
  backward : 'b -> 'a;  (** Conversion applied before encoding. *)
}

(** Alias for {!type:t}. Useful in contexts where [t] is shadowed
    (e.g. inside a {!module-type:DRIVER} implementation). *)
type 'a codec = 'a t

(** {1:prims Primitives}

    One representation per OCaml base type. These are the leaves of the
    GADT: a driver that handles every primitive and every structural
    constructor covers all possible types. *)

val unit : unit t
val bool : bool t
val int : int t
val int32 : int32 t
val int64 : int64 t
val float : float t
val char : char t
val string : string t

(** {1:combinators Combinators} *)

(** [option r] represents ['a option] given a representation [r] of ['a].

    Drivers typically encode [None] as null and [Some v] by encoding [v]
    directly. This means [option (option r)] cannot distinguish
    [Some None] from [None] in formats without a native option type
    (e.g. JSON). *)
val option : 'a t -> 'a option t

(** {2 Tuples} *)

val tuple2 : 'a t -> 'b t -> ('a * 'b) t
val tuple3 : 'a t -> 'b t -> 'c t -> ('a * 'b * 'c) t
val tuple4 : 'a t -> 'b t -> 'c t -> 'd t -> ('a * 'b * 'c * 'd) t

val tuple5 :
  'a t -> 'b t -> 'c t -> 'd t -> 'e t ->
  ('a * 'b * 'c * 'd * 'e) t

val tuple6 :
  'a t -> 'b t -> 'c t -> 'd t -> 'e t -> 'f t ->
  ('a * 'b * 'c * 'd * 'e * 'f) t

(** {2 Transformations} *)

(** [map forward backward r] builds a representation for ['b] out of a
    representation [r] for ['a].

    {ul
    {- {b Encoding.} [backward] converts ['b] to ['a], then [r] is used
       to encode the result.}
    {- {b Decoding.} [r] is used to decode an ['a], then [forward]
       converts it to ['b].}}

    Typical use: newtype wrappers.
    {[
      type email = Email of string

      let email : email Codec.t =
        Codec.map (fun s -> Email s) (fun (Email s) -> s) Codec.string
    ]} *)
val map : ('a -> 'b) -> ('b -> 'a) -> 'a t -> 'b t

(** [lazy_ lr] forces [lr] on each use. This breaks recursion cycles
    that arise with recursive types.

    {[
      type tree = Leaf | Node of tree * int * tree

      let rec tree_codec : tree Codec.t Lazy.t =
        lazy (Codec.variant "tree" [
          Codec.case0 "Leaf" Leaf;
          Codec.case "Node" Codec.(tuple3 (lazy_ tree_codec) int (lazy_ tree_codec))
            (function Node (l, v, r) -> Some (l, v, r) | _ -> None)
            (fun (l, v, r) -> Node (l, v, r));
        ])

      let tree_codec = Codec.lazy_ tree_codec
    ]} *)
val lazy_ : 'a t lazy_t -> 'a t

(** {1:collections Collections}

    Homogeneous ordered containers are all described by a single
    {!constructor:Collection} constructor (see
    {!type:collection_desc}). [list], [array], [Hashtbl.t], [Queue.t],
    [Set.Make(K).t], [Map.Make(K).t], [Seq.t], … are all instances.

    A driver matches [Collection { iter; builder; element_codec }] once
    and dispatches to the right behavior via the two callbacks.
    Encoding never materializes an intermediate list; decoding streams
    elements into the builder. *)

(** [list r] represents ['a list]. *)
val list : 'a t -> 'a list t

(** [array r] represents ['a array]. *)
val array : 'a t -> 'a array t

(** [seq r] represents ['a Seq.t]. Note that an encoded [Seq.t] is
    forced (consumed) by the driver; decoding produces a fresh
    finite [Seq.t]. *)
val seq : 'a t -> 'a Seq.t t

(** [queue r] represents ['a Queue.t]. Iteration order is FIFO, which
    is preserved across encode/decode. *)
val queue : 'a t -> 'a Queue.t t

(** [hashtbl k v] represents [('k, 'v) Hashtbl.t] as a sequence of
    [(key, value)] pairs. Iteration order is unspecified (Hashtbl
    doesn't guarantee order), so roundtripping a Hashtbl is preserved
    only up to multiset equality. *)
val hashtbl : 'k t -> 'v t -> ('k, 'v) Hashtbl.t t

(** [collection ~iter ~builder element_codec] is the low-level escape
    hatch for describing a custom container. Users typically reach for
    {!val:list}, {!val:array}, {!val:hashtbl}, …; this is here for
    third-party container types.

    @param iter        Push-style traversal of the container. Must
      yield every element exactly once.
    @param builder     [builder ()] returns a fresh [(sink, finalize)]
      pair. The driver feeds decoded elements into [sink] (in the
      same order the encoder wrote them) then calls [finalize ()].
    @param element_codec  Type representation of each element. *)
val collection :
  iter:(('elem -> unit) -> 'container -> unit) ->
  builder:(unit -> ('elem -> unit) * (unit -> 'container)) ->
  'elem t ->
  'container t

(** {2 Functorial wrappers for [Set.Make] / [Map.Make]} *)

(** Codec construction for functorial map types ([Map.Make(K).t] and
    look-alikes).

    {[
      module Smap = Map.Make(String)
      module Smap_codec = Codec.Map.Make(Smap)

      let counts : int Smap.t Codec.t =
        Smap_codec.codec Codec.string Codec.int
    ]} *)
module Map : sig
  (** Minimal subset of [Map.Make]'s output signature needed to build a
      codec. Real instances of [Map.Make(K)] satisfy this directly. *)
  module type S = sig
    type key
    type +!'a t
    val empty : 'a t
    val add : key -> 'a -> 'a t -> 'a t
    val iter : (key -> 'a -> unit) -> 'a t -> unit
  end

  (** [Make(M)] provides a codec for the abstract map type [M.t]. The
      caller supplies the codecs for [M.key] and for the value type at
      use site.

      {[
        module Smap = Map.Make (String)
        module Smap_codec = Codec.Map.Make (Smap)

        let counts : int Smap.t Codec.t =
          Smap_codec.codec Codec.string Codec.int

        (* Or, attached to [Smap] so the ppx picks it up via the
           [Module.t_codec] convention: *)
        module Smap_with_codec = struct
          include Smap
          let t_codec v = Smap_codec.codec Codec.string v
        end
      ]} *)
  module Make (M : S) : sig
    val codec : M.key codec -> 'a codec -> 'a M.t codec
  end
end

(** Codec construction for functorial set types ([Set.Make(K).t] and
    look-alikes).

    {[
      module Sset = Set.Make(String)
      module Sset_codec = Codec.Set.Make(Sset)

      let names : Sset.t Codec.t =
        Sset_codec.codec Codec.string
    ]} *)
module Set : sig
  (** Minimal subset of [Set.Make]'s output signature needed to build a
      codec. Real instances of [Set.Make(K)] satisfy this directly. *)
  module type S = sig
    type elt
    type t
    val empty : t
    val add : elt -> t -> t
    val iter : (elt -> unit) -> t -> unit
  end

  (** [Make(S)] provides a codec for the abstract set type [S.t]. The
      caller supplies the codec for [S.elt] at use site.

      {[
        module Sset = Set.Make (String)
        module Sset_codec = Codec.Set.Make (Sset)

        let names : Sset.t Codec.t = Sset_codec.codec Codec.string
      ]} *)
  module Make (S : S) : sig
    val codec : S.elt codec -> S.t codec
  end
end

(** {1:records Records}

    Record representations are built with a type-safe applicative pipeline.
    The compiler ensures that the number and types of {!val:field} calls
    match the constructor function given to {!val:record}.

    {[
      type user = { name : string; age : int }

      let user : user Codec.t =
        Codec.record "user" (fun name age -> { name; age })
        |> Codec.field "name" Codec.string (fun u -> u.name)
        |> Codec.field "age" Codec.int (fun u -> u.age)
        |> Codec.seal
    ]}

    Fields with defaults:
    {[
      type config = { host : string; port : int; debug : bool }

      let config : config Codec.t =
        Codec.record "config" (fun host port debug -> { host; port; debug })
        |> Codec.field "host" Codec.string (fun c -> c.host)
        |> Codec.field ~default:8080 "port" Codec.int (fun c -> c.port)
        |> Codec.field ~default:false "debug" Codec.bool (fun c -> c.debug)
        |> Codec.seal
    ]} *)

(** [record name ctor] starts a record description.

    [name] is used in error messages. [ctor] is the function that builds
    the record value from its fields; it must take one argument per
    subsequent {!val:field} call. *)
val record : string -> 'f -> ('f, 'r) fields

(** [field ?default name repr get fields] adds a field to the description.

    @param name  Serialized name of the field.
    @param repr  Type representation of the field value.
    @param get   Extracts this field from a record value (used for encoding).
    @param default  If provided, used when the field is absent during
      decoding. Without [~default] the field is required and a missing
      field is a decoding error. *)
val field :
  ?default:'a -> string -> 'a t -> ('r -> 'a) ->
  ('a -> 'f, 'r) fields -> ('f, 'r) fields

(** [field_opt name repr get fields] adds an optional field whose type
    is ['a option]. A missing or null field decodes as [None].

    This is equivalent to
    [field ~default:None name (option repr) get fields]. *)
val field_opt :
  string -> 'a t -> ('r -> 'a option) ->
  ('a option -> 'f, 'r) fields -> ('f, 'r) fields

(** [seal fields] closes the record description.

    Only compiles when every constructor argument has been supplied
    by a {!val:field} call, i.e. when the type of [fields] is
    [('r, 'r) fields]. *)
val seal : ('r, 'r) fields -> 'r t

(** {1:variants Variants}

    Variant representations are described by listing each constructor
    as a {!type:case}. Constant constructors use {!val:case0}; constructors
    with a payload use {!val:case}.

    {[
      type color = Red | Green | Custom of int * int * int

      let color : color Codec.t =
        Codec.variant "color" [
          Codec.case0 "Red" Red;
          Codec.case0 "Green" Green;
          Codec.case "Custom" Codec.(tuple3 int int int)
            (function Custom (r, g, b) -> Some (r, g, b) | _ -> None)
            (fun (r, g, b) -> Custom (r, g, b));
        ]
    ]} *)

(** [case name repr destruct construct] describes a constructor with
    a payload.

    @param name       Serialized name.
    @param repr       Payload type representation.
    @param destruct   Returns [Some payload] when the variant value matches
      this constructor, [None] otherwise. Used during encoding: cases are
      tried in order until one returns [Some].
    @param construct  Rebuilds the variant from a decoded payload. *)
val case : string -> 'a t -> ('v -> 'a option) -> ('a -> 'v) -> 'v case

(** [case0 name value] describes a constant constructor (no payload).

    @param name   Serialized name.
    @param value  The constant variant value. *)
val case0 : string -> 'v -> 'v case

(** [variant name cases] builds a variant representation.

    @param name   Used in error messages.
    @param cases  Ordered list of constructors. During encoding, the first
      matching case wins. *)
val variant : string -> 'v case list -> 'v t

(** {1:convert Value conversion}

    The Codec library converts between OCaml values and *other values*
    (a [Yojson.Safe.t] AST, an S-expression, an [Sexp.t], …). It is
    {b pure}: encode produces a fresh value, decode consumes one. There
    is no sink, no side effect.

    For sink-based serialization (writing tokens to a [Buffer.t] or
    [out_channel] without materializing an intermediate value), use
    the separate {!Marshal} library — same {!type:t} GADT, different
    semantics.

    {2 Writer / Reader interfaces}

    A {!module:Writer.S} is a module of {b constructors} that build
    values of some target type [t]: [null : t], [int : int -> t],
    [list : t list -> t], etc. A {!module:Reader.S} is the symmetric
    module of {b extractors} that pull primitive values out of a [t]
    with [(_, error) result] return.

    Each format provides one [Writer.S] and one [Reader.S] for its
    representation (e.g. [Codec_yojson.Writer] / [.Reader] both
    instantiated with [type t = Yojson.Safe.t]). *)

(** Constructors that build a target value of type [t]. *)
module Writer : sig
  module type S = sig
    type t
    val null   : t
    val bool   : bool -> t
    val int    : int -> t
    val int32  : int32 -> t
    val int64  : int64 -> t
    val float  : float -> t
    val char   : char -> t
    val string : string -> t
    val list   : t list -> t
    val record : (string * t) list -> t

    (** Constant variant constructor (no payload), e.g. [Red] encoded
        as ["Red"] in most formats. *)
    val variant_constant : string -> t

    (** Variant constructor with a payload, e.g. [Circle 1.5] encoded
        as [["Circle", 1.5]] in JSON. The writer is in control of the
        surrounding syntax. *)
    val variant_payload : string -> t -> t
  end
end

(** Extractors that pull primitive values out of [t]. *)
module Reader : sig
  module type S = sig
    type t
    val null    : t -> (unit,   error) result
    val bool    : t -> (bool,   error) result
    val int     : t -> (int,    error) result
    val int32   : t -> (int32,  error) result
    val int64   : t -> (int64,  error) result
    val float   : t -> (float,  error) result
    val char    : t -> (char,   error) result
    val string  : t -> (string, error) result
    val list    : t -> (t list, error) result
    val record  : t -> ((string * t) list, error) result
  end
end

(** {2 Top-level encode / decode}

    {[
      (* Get a Yojson AST from a value: *)
      let ast =
        Codec.encode user_codec my_user
          ~writer:(module Codec_yojson.Writer)
        |> Result.get_ok

      (* Read a value back: *)
      let u =
        Codec.decode user_codec ~reader:(module Codec_yojson.Reader) ast
        |> Result.get_ok
    ]}

    Curry-style: partial-apply on [(codec, value)] to get a function
    you can call with several writers, or pre-apply a writer to get a
    specialized encoder.

    {[
      let s = Codec.encode user_codec my_user in
      let ast  = s ~writer:(module Codec_yojson.Writer) |> Result.get_ok
      let sexp = s ~writer:(module Codec_sexp.Writer)   |> Result.get_ok
    ]} *)

val encode :
  'a codec -> 'a ->
  writer:(module Writer.S with type t = 'b) ->
  ('b, error) result

val decode :
  'a codec ->
  reader:(module Reader.S with type t = 'b) ->
  'b ->
  ('a, error) result
