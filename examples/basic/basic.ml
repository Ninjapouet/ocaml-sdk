(* Basic example: define type representations, encode/decode via JSON *)

(* -- Types ----------------------------------------------------------------- *)

type color = Red | Green | Blue

type point = { x : float; y : float }

type shape =
  | Circle of { center : point; radius : float }
  | Rectangle of { origin : point; width : float; height : float }
  | ColoredShape of { shape : shape; color : color }

(* -- Type representations -------------------------------------------------- *)

let color_codec =
  Codec.variant "color"
    [ Codec.case0 "Red" Red
    ; Codec.case0 "Green" Green
    ; Codec.case0 "Blue" Blue
    ]

let point_codec =
  Codec.record "point" (fun x y -> { x; y })
  |> Codec.field "x" Codec.float (fun p -> p.x)
  |> Codec.field "y" Codec.float (fun p -> p.y)
  |> Codec.seal

let rec shape_codec =
  lazy
    (Codec.variant "shape"
       [ Codec.case "Circle"
           (Codec.record "circle" (fun center radius -> Circle { center; radius })
            |> Codec.field "center" point_codec (function Circle { center; _ } -> center | _ -> assert false)
            |> Codec.field "radius" Codec.float (function Circle { radius; _ } -> radius | _ -> assert false)
            |> Codec.seal)
           (function Circle { center; radius } -> Some (Circle { center; radius }) | _ -> None)
           Fun.id
       ; Codec.case "Rectangle"
           (Codec.record "rectangle" (fun origin width height -> Rectangle { origin; width; height })
            |> Codec.field "origin" point_codec (function Rectangle { origin; _ } -> origin | _ -> assert false)
            |> Codec.field "width" Codec.float (function Rectangle { width; _ } -> width | _ -> assert false)
            |> Codec.field "height" Codec.float (function Rectangle { height; _ } -> height | _ -> assert false)
            |> Codec.seal)
           (function Rectangle r -> Some (Rectangle r) | _ -> None)
           Fun.id
       ; Codec.case "ColoredShape"
           (Codec.record "colored_shape" (fun shape color -> ColoredShape { shape; color })
            |> Codec.field "shape" (Codec.lazy_ shape_codec) (function ColoredShape { shape; _ } -> shape | _ -> assert false)
            |> Codec.field "color" color_codec (function ColoredShape { color; _ } -> color | _ -> assert false)
            |> Codec.seal)
           (function ColoredShape r -> Some (ColoredShape r) | _ -> None)
           Fun.id
       ])

let shape_codec = Codec.lazy_ shape_codec

(* -- Main ------------------------------------------------------------------ *)

let () =
  let shapes =
    [ Circle { center = { x = 0.0; y = 0.0 }; radius = 5.0 }
    ; Rectangle { origin = { x = 1.0; y = 2.0 }; width = 10.0; height = 5.0 }
    ; ColoredShape
        { shape = Circle { center = { x = 3.0; y = 4.0 }; radius = 1.5 }
        ; color = Red
        }
    ]
  in
  let shapes_codec = Codec.list shape_codec in

  (* Encode to JSON — the driver traverses the value directly,
     guided by the GADT type description. No intermediate copy. *)
  let json = Codec_yojson.Raw.encode_exn shapes_codec shapes in
  let json_str = Yojson.Safe.pretty_to_string json in
  Printf.printf "Encoded JSON:\n%s\n\n" json_str;

  (* Decode back *)
  let decoded = Codec_yojson.Raw.decode_exn shapes_codec json in
  Printf.printf "Roundtrip OK: %b\n\n" (shapes = decoded);

  (* Demonstrate error handling *)
  let bad_json = Yojson.Safe.from_string {|{"x": "not a number", "y": 1.0}|} in
  match Codec_yojson.Raw.decode point_codec bad_json with
  | Ok _ -> Printf.printf "unexpected success\n"
  | Error e -> Printf.printf "Expected error: %s\n" (Codec.Error.to_string e)
