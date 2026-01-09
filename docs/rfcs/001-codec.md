# RFC 001: Codec - Generic Type Serialization Library

| Metadata     | Value                     |
|--------------|---------------------------|
| Status       | Draft                     |
| Created      | 2025-12-19                |
| Authors      | OCamlPro                  |

## Summary

This RFC proposes the inclusion of a `codec` library in `ocaml-sdk` that provides a
minimal, format-agnostic abstraction for encoding and decoding OCaml types.

**Core principles:**

1. **Lightweight**: Minimal dependencies (only `fmt` for error formatting), small
   runtime footprint
2. **Format-agnostic**: The core library knows nothing about JSON, YAML, or any
   specific format. It only provides the abstraction and combinators.
3. **Extensible**: Users can easily define their own drivers for custom formats
4. **Composable**: Streaming and other advanced patterns emerge naturally from
   codec composition rather than special APIs

The library consists of:

1. A minimal runtime library defining the `Codec.t` type and combinators
2. A PPX deriver (`[@@deriving codec]`) for automatic codec generation
3. A driver interface that users implement for their target formats
4. Optional companion packages for standard formats (JSON, YAML, etc.)

## Motivation

### The Problem

When building OCaml applications, developers frequently need to serialize and
deserialize data structures for various purposes:

- Configuration files (JSON, YAML, TOML)
- Network protocols (JSON-RPC, binary protocols)
- Database storage
- Inter-process communication
- File formats

Currently, each serialization format requires its own PPX or manual encoding:

```ocaml
type config = {
  host: string;
  port: int;
}
[@@deriving yojson]  (* For JSON *)
[@@deriving yaml]    (* For YAML - separate PPX *)
[@@deriving sexp]    (* For S-expressions - yet another PPX *)
```

This approach has several drawbacks:

1. **Inconsistent APIs**: Different libraries have different conventions
2. **No format abstraction**: Code is coupled to a specific format
3. **Testing complexity**: Each format needs separate test coverage

### The Solution

A unified codec abstraction where one derives a single codec that works with
any format through a driver system:

```ocaml
type config = {
  host: string;
  port: int;
}
[@@deriving codec ~driver:(module Json)]

(* Later, switch to YAML with minimal changes *)
[@@deriving codec ~driver:(module Yaml)]
```

Or even better, derive format-agnostic codecs and choose the driver at runtime:

```ocaml
(* Derive once *)
type config = { ... }
[@@deriving codec ~driver:(module Driver)]

(* Use with any driver *)
let json_str = Codec.encode config_codec value |> Json.to_string
let yaml_str = Codec.encode config_codec value |> Yaml.to_string
```

## State of the Art

### Existing OCaml Serialization Libraries

#### 1. ppx_deriving_yojson

[ppx_deriving_yojson](https://github.com/ocaml-ppx/ppx_deriving_yojson) generates
JSON codecs using the Yojson library.

**Strengths:**
- Mature and widely used
- Good error messages with location information
- Supports records, variants, polymorphic variants
- Customizable field names via `[@key]`

**Limitations:**
- JSON-only: cannot target other formats
- Generates two separate functions (`to_yojson`, `of_yojson`) rather than a
  unified codec value
- No driver abstraction

#### 2. ppx_protocol_conv

[ppx_protocol_conv](https://github.com/andersfugmann/ppx_protocol_conv) is the
closest to our approach with its driver-based architecture. It was a major
inspiration for `codec`.

**Strengths:**
- Driver-based: supports JSON (Yojson), YAML, MessagePack, XML
- Single annotation for multiple formats
- Customizable via `[@key]`, `[@default]`, `[@name]`
- Separate driver packages (`ppx_protocol_conv_json`, `ppx_protocol_conv_yaml`, etc.)

**Limitations:**

- **Binary size overhead**: In our experience, using `ppx_protocol_conv` resulted
  in significantly larger binaries. The runtime library and generated code add
  considerable weight to the final executable. This is a common issue with PPX
  libraries that pull in heavy dependencies.

- **Verbose API**: The generated functions follow the pattern:
  ```ocaml
  val record_to_json : record -> Json.t
  val record_of_json_exn : Json.t -> record
  val record_of_json : Json.t -> (record, error) result
  ```
  This leads to three functions per type per driver, rather than a single
  composable codec value.

- **Driver coupling**: Each driver package (`ppx_protocol_conv_json`, etc.)
  brings its own dependencies. The PPX itself is coupled to the driver
  ecosystem rather than being truly format-agnostic.

- **No first-class codec values**: Cannot pass codecs as values, compose them,
  or store them in data structures.

- **No codec combinators**: No built-in way to transform or compose codecs
  (e.g., `Codec.map`, `Codec.compose`).

- **Runtime structure**: The runtime uses a relatively heavy intermediate
  representation that may not be optimal for all use cases.

- **Does not support GADTs or extensible types**

#### 3. data-encoding (Tezos)

[data-encoding](https://octez.tezos.com/docs/developer/data_encoding.html) is
a GADT-based library used in Tezos for type-safe serialization.

**Strengths:**
- Type-safe encoding with GADTs
- Supports both binary and JSON
- Rich combinator library
- Designed for security-critical applications

**Limitations:**
- Encodings are written manually (no PPX)
- Verbose for simple types
- Tightly coupled to Tezos ecosystem
- Complex API with steep learning curve

#### 4. Repr (Irmin/MirageOS)

[Repr](https://mirage.github.io/repr/repr/Repr/index.html) provides runtime
type representations with serialization capabilities.

**Strengths:**
- Rich type representation system
- Supports JSON and binary
- Efficient binary encoding with size optimization
- PPX available (`ppx_repr`)

**Limitations:**
- Primarily designed for Irmin's needs
- Heavy dependency for simple use cases
- Less focus on human-readable formats

#### 5. ppx_sexp_conv / ppx_bin_prot (Jane Street)

Jane Street's serialization PPXs for S-expressions and binary protocols.

**Strengths:**
- Battle-tested in production
- Excellent performance (bin_prot)
- Good integration with Core ecosystem

**Limitations:**
- Tied to specific formats (sexp or binary)
- Core/Base ecosystem dependency
- No driver abstraction

### API Comparison: ppx_protocol_conv vs codec

To illustrate the difference in ergonomics, here's how the same task looks
with both libraries:

**ppx_protocol_conv:**
```ocaml
open Protocol_conv_json

type user = { name: string; age: int }
[@@deriving protocol ~driver:(module Json)]

(* Generated: 3 functions *)
val user_to_json : user -> Json.t
val user_of_json : Json.t -> (user, error) result
val user_of_json_exn : Json.t -> user

(* Usage: functions, not values *)
let json = user_to_json { name = "Alice"; age = 30 }
let user = user_of_json_exn json

(* Cannot easily compose or pass around *)
let serialize_list users = List.map user_to_json users
```

**codec (proposed):**
```ocaml
open Codec

type user = { name: string; age: int }
[@@deriving codec ~driver:(module Json)]

(* Generated: 1 codec value *)
val user_codec : (user, Json.t) Codec.t

(* Usage: first-class value *)
let json = Codec.encode user_codec { name = "Alice"; age = 30 }
let user = Codec.decode user_codec json

(* Compose naturally: build a codec for user list -> JSON array *)
let users_codec : (user list, Json.t) Codec.t =
  Codec.list Json.list user_codec
let serialize_list users = Codec.encode users_codec users

(* Pass as argument *)
let save_to_file : ('a, Json.t) Codec.t -> 'a -> string -> unit =
  fun codec value path ->
    let json_str = Codec.encode codec value |> Json.to_string in
    Out_channel.with_open_text path (fun oc -> Out_channel.output_string oc json_str)
```

### Comparison Table

| Feature                    | yojson | protocol_conv | data-encoding | repr | codec (ours) |
|----------------------------|--------|---------------|---------------|------|--------------|
| Driver-based               | No     | Yes           | Partial       | No   | Yes          |
| First-class codec values   | No     | No            | Yes           | Yes  | Yes          |
| PPX support                | Yes    | Yes           | No            | Yes  | Yes          |
| Codec combinators          | No     | No            | Yes           | Yes  | Yes          |
| Runtime driver selection   | N/A    | No            | N/A           | No   | Yes          |
| Lightweight core           | Yojson | Heavy         | Heavy         | repr | fmt only     |
| Binary size impact         | Medium | High          | High          | High | Low          |
| Streaming (via composition)| No     | No            | Partial       | No   | Yes          |
| Easy custom drivers        | N/A    | Medium        | Hard          | Hard | Yes          |
| Ergonomic API              | Medium | Low           | Low           | Medium | High       |
| Custom field names         | Yes    | Yes           | Manual        | Yes  | Yes          |
| Default values             | Yes    | Yes           | Manual        | No   | Yes          |
| Recursive types            | Yes    | Yes           | Yes           | Yes  | Yes          |
| Parametric types           | Yes    | Yes           | Yes           | Yes  | Yes          |

### Why a New Library?

None of the existing solutions fully satisfies our requirements:

1. **Truly lightweight**: Most libraries pull in heavy dependencies. We want a
   core that only depends on `fmt` for error formatting. No Yojson, no Core, no
   large framework.

2. **Format-agnostic by design**: The core library should not mention JSON, YAML,
   or any specific format. It provides the abstraction; users bring their own
   formats. This is different from `ppx_protocol_conv` which, while driver-based,
   still couples the PPX to specific driver packages.

3. **First-class codec values**: We want `('a, 'driver) Codec.t` as a value that
   can be passed around, composed, and stored, not just generated functions.

4. **Easy custom drivers**: Implementing a driver for a custom format (proprietary
   binary protocol, database rows, etc.) should be straightforward and well-documented.

5. **Composability**: Codecs should compose naturally with combinators like
   `option`, `list`, `compose`.

6. **Bidirectionality**: A single codec value handles both encoding and decoding,
   ensuring they stay in sync.

7. **Streaming via composition**: For large data, streaming emerges naturally
   by composing codecs with lazy types (`Seq.t`) rather than requiring special APIs.

## Current Implementation

The existing `ocamlpro-codec` library provides a foundation with:

### Core Types

```ocaml
type ('a, 'b) t  (* A codec from 'a to 'b *)

val make : encode:('a -> 'b) -> decode:('b -> 'a) -> ('a, 'b) t
val encode : ('a, 'b) t -> 'a -> 'b
val decode : ('a, 'b) t -> 'b -> 'a  (* Currently raises on error, see Proposed Improvements *)
```

### Combinators

```ocaml
val identity : unit -> ('a, 'a) t
val swap : ('a, 'b) t -> ('b, 'a) t
val compose : ('a, 'b) t -> ('b, 'c) t -> ('a, 'c) t
val option : ('b option, 'b) t -> ('a, 'b) t -> ('a option, 'b) t
val list : ('b list, 'b) t -> ('a, 'b) t -> ('a list, 'b) t
val array : ('b array, 'b) t -> ('a, 'b) t -> ('a array, 'b) t
```

### Driver Protocol

```ocaml
module type DRIVER = sig
  type t
  val unit : (unit, t) codec
  val bool : (bool, t) codec
  val int : (int, t) codec
  val float : (float, t) codec
  val char : (char, t) codec
  val string : (string, t) codec
  val option : (t option, t) codec
  val list : (t list, t) codec
  val array : (t array, t) codec
  val tuple : (t list, t) codec
  val dict : ((string * t) list, t) codec
end
```

### PPX Deriver

```ocaml
type color =
  | Red
  | Green
  | Blue [@name "Bleu"]
  | Custom of int * int * int
  | RGB of { r: int; g: int; b: int }
[@@deriving codec ~driver:(module Json)]

(* Generates: val color_codec : (color, Json.t) Codec.t *)
```

**Supported attributes:**
- `[@name "..."]` - Rename field or constructor in serialized form
- `[@default expr]` - Default value for missing fields
- `~omit_defaults` - Omit fields equal to their default value
- `~suffix "..."` - Add suffix to generated codec name

## Proposed Architecture

### Package Structure

The library should be split into minimal, focused packages:

```
codec/                    # Core library (no format dependencies)
├── codec.opam           # Only depends on: fmt
├── src/
│   ├── codec.ml         # Core types and combinators
│   └── codec.mli
│
ppx_codec/               # PPX deriver (separate package)
├── ppx_codec.opam       # Depends on: codec, ppxlib
└── src/
    ├── ppx_codec.ml     # Deriver registration and entry point
    ├── gen_encode.ml    # Encoder generation logic
    ├── gen_decode.ml    # Decoder generation logic
    └── utils.ml         # Shared helpers (attribute parsing, etc.)
│
codec-json/              # Optional: JSON driver (separate package)
├── codec-json.opam      # Depends on: codec, yojson
└── src/
    └── codec_json.ml
│
codec-yaml/              # Optional: YAML driver (separate package)
└── ...
```

Users only install what they need. The core `codec` package has **zero format
dependencies**.

### Driver Interface

The driver interface should be minimal and easy to implement:

```ocaml
module type DRIVER = sig
  (** The target type of serialization (e.g., Yojson.t, bytes, etc.) *)
  type t

  (** Primitive codecs *)
  val unit : (unit, t) Codec.t
  val bool : (bool, t) Codec.t
  val int : (int, t) Codec.t
  val int32 : (int32, t) Codec.t
  val int64 : (int64, t) Codec.t
  val float : (float, t) Codec.t
  val char : (char, t) Codec.t
  val string : (string, t) Codec.t

  (** Structural codecs *)
  val option : (t option, t) Codec.t
  val list : (t list, t) Codec.t
  val array : (t array, t) Codec.t
  val tuple : (t list, t) Codec.t
  val dict : ((string * t) list, t) Codec.t
end
```

A custom driver for, say, a proprietary binary format would look like
(simplified, without error handling):

```ocaml
module My_binary_driver : Codec.DRIVER = struct
  type t = bytes

  let int = Codec.make
    ~encode:(fun i ->
      let b = Bytes.create 4 in
      Bytes.set_int32_be b 0 (Int32.of_int i);
      b)
    ~decode:(fun b ->
      Bytes.get_int32_be b 0 |> Int32.to_int)

  (* ... other primitives ... *)
end
```

### Streaming via Composition

Streaming is not a special API - it's naturally handled through codec composition.
The key insight is that codecs compose, so streaming is just another codec in the chain.

For example, to stream XML using Daniel Bünzli's Xmlm library:

```ocaml
(* Xmlm provides a streaming signal type *)
type xml_signal = [ `El_start of ... | `El_end | `Data of string ]

(* A codec between our domain type and a sequence of XML signals *)
let foo_xml_signals : (foo, xml_signal Seq.t) Codec.t = ...

(* Compose with I/O: the Seq.t is consumed/produced lazily *)
let write_foo : foo -> out_channel -> unit = fun value oc ->
  let signals = Codec.encode foo_xml_signals value in
  let output = Xmlm.make_output (`Channel oc) in
  Seq.iter (Xmlm.output output) signals  (* lazy: no buffering *)

let read_foo : in_channel -> foo = fun ic ->
  let input = Xmlm.make_input (`Channel ic) in
  let signals = Seq.of_dispenser (fun () ->
    match Xmlm.input input with
    | signal -> Some signal
    | exception End_of_file -> None
  ) in
  Codec.decode foo_xml_signals signals  (* lazy: processes as it reads *)
```

This approach has several advantages:

1. **No API bloat**: The core `Codec.t` type stays simple
2. **True streaming**: Using `Seq.t` or similar lazy types avoids buffering
3. **Flexibility**: Users choose their streaming strategy
4. **Composition**: Chain codecs naturally (`foo <-> xml_signals Seq.t <-> channel`)
5. **Library agnostic**: Works with any streaming library (Xmlm, Jsonm, Angstrom, etc.)

The same pattern works for JSON with Jsonm, binary with Angstrom/Faraday, etc.

## Proposed Improvements

### 1. Better Error Handling

**Current state:** Errors use `failwith` or a simple `Error` exception.

**Proposed:**
```ocaml
type error = {
  path: string list;      (* e.g., ["config"; "server"; "port"] *)
  expected: string;       (* e.g., "int" *)
  got: string;           (* e.g., "string \"abc\"" *)
  message: string option; (* Additional context *)
}

type 'a result = ('a, error) Result.t

val decode : ('a, 'b) t -> 'b -> 'a result
val decode_exn : ('a, 'b) t -> 'b -> 'a  (* For convenience *)
```

Error messages should include the path to the failing field:
```
Error decoding config.server.port: expected int, got string "abc"
```

### 2. Improved PPX Diagnostics

**Current state:** PPX errors can be cryptic.

**Proposed:**
- Clear error messages for unsupported type constructs
- Suggestions for fixing common mistakes
- Location-accurate error reporting

### 3. Additional Combinators

```ocaml
(* Result type support *)
val result : ('ok, 'b) t -> ('err, 'b) t -> (('ok, 'err) result, 'b) t

(* Map over codecs *)
val map : ('a -> 'b) -> ('b -> 'a) -> ('a, 'c) t -> ('b, 'c) t

(* Lazy codecs for recursive types *)
val lazy_ : ('a, 'b) t Lazy.t -> ('a, 'b) t

(* Validation *)
val validate : ('a -> bool) -> string -> ('a, 'b) t -> ('a, 'b) t
```

### 4. Documentation

- Comprehensive API documentation with examples
- Tutorial for common use cases
- Guide for implementing custom drivers

### 5. Test Suite

- Unit tests for all combinators
- Property-based tests for encode/decode roundtrips
- PPX output tests for generated code
- Example drivers with integration tests

### 6. Example Drivers (Separate Packages)

Provide example drivers as **separate optional packages**:

- `codec-json` - JSON driver using Yojson (example, not required)
- `codec-yaml` - YAML driver using ocaml-yaml (example, not required)

These serve as:
1. Ready-to-use drivers for common formats
2. Reference implementations for custom driver authors
3. Test fixtures for the core library

**Important**: The core `codec` library does NOT depend on these. They are
purely optional.

## Use Cases

### 1. Simple In-Memory Format

A developer wants to serialize OCaml values to a simple tagged format for
debugging or logging:

```ocaml
(* Define a trivial driver in a few dozen lines *)
module Debug_driver : Codec.DRIVER = struct
  type t = string

  let unit = Codec.make ~encode:(fun () -> "()") ~decode:(fun _ -> ())
  let bool = Codec.make ~encode:string_of_bool ~decode:bool_of_string
  let int = Codec.make ~encode:string_of_int ~decode:int_of_string
  (* ... *)
end

(* Use it *)
type point = { x: int; y: int }
[@@deriving codec ~driver:(module Debug_driver)]

let () = print_endline (Codec.encode point_codec { x = 1; y = 2 })
(* Output: {x=1, y=2} *)
```

### 2. Custom Binary Protocol

A game developer needs to serialize game state over the network:

```ocaml
module Game_protocol : Codec.DRIVER = struct
  type t = Bytes.t
  (* Compact binary encoding optimized for network *)
  let int = Codec.make
    ~encode:(fun i -> (* varint encoding *) ...)
    ~decode:(fun b -> (* varint decoding *) ...)
  (* ... *)
end
```

### 3. Database Row Mapping

Map OCaml records to database rows:

```ocaml
module Postgres_driver : Codec.DRIVER = struct
  type t = string array  (* Row as array of strings *)
  (* ... *)
end

type user = { id: int; name: string; email: string }
[@@deriving codec ~driver:(module Postgres_driver)]
```

### 4. Configuration with Multiple Formats

Support both JSON and YAML config files with the same types:

```ocaml
type config = { host: string; port: int }
[@@deriving codec ~driver:(module Json_driver)]
[@@deriving codec ~driver:(module Yaml_driver) ~suffix:"yaml"]

(* Use either *)
let config = Codec.decode config_codec json_data
let config = Codec.decode config_yaml_codec yaml_data
```

## Open Questions

1. **Naming**: Should the library be called `codec`, `encoding`, `serial`, or
   something else?

2. **Error handling**: Should we use `Result.t` everywhere or provide both
   exception and result-based APIs?

3. **Dependency on `fmt`**: Currently we depend on `fmt` for error formatting.
   Should we remove this dependency entirely and use `Format` from stdlib?
   This would make the library truly zero-dependency.

4. **Driver at compile-time vs runtime**: The current PPX requires specifying
   the driver at compile time. Should we support runtime driver selection?
   This would require a more complex type using rank-2 polymorphism:
   ```ocaml
   type 'a codec = { encode: 'driver. 'a -> 'driver; ... }
   ```

5. **Attribute syntax**: Should we use `[@codec.name]` namespace or keep the
   shorter `[@name]`?

6. **Backward compatibility**: How do we handle protocol evolution and versioning?

## Migration Path

For users of `ocamlpro-stdlib`:

1. The library will be moved to `ocaml-sdk` as `codec`
2. PPX will be renamed from `ocamlpro-ppx` to `ppx_codec` or similar
3. Module names will change: `Ocamlpro_codec.Codec` -> `Codec`

## References

- [ppx_deriving_yojson](https://github.com/ocaml-ppx/ppx_deriving_yojson)
- [ppx_protocol_conv](https://github.com/andersfugmann/ppx_protocol_conv)
- [data-encoding (Tezos)](https://octez.tezos.com/docs/developer/data_encoding.html)
- [Repr (MirageOS)](https://mirage.github.io/repr/repr/Repr/index.html)
- [ppx_sexp_conv](https://opam.ocaml.org/packages/ppx_sexp_conv/)
- [Real World OCaml - Data Serialization](https://dev.realworldocaml.org/data-serialization.html)
