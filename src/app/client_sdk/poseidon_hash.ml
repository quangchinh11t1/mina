open Core_kernel
open Js_of_ocaml

(* *************** *
 *    our field    *
 * *************** *)
module Field = struct
  include Snark_params_nonconsensus.Field

  let of_bytes bytearray =
    let aux i acc c =
      let big = Nat.of_int @@ int_of_char c in
      let offset = Nat.shift_left big (Int.( * ) i 8) in
      Nat.(acc + offset)
    in
    let zero = Nat.of_int 0 in
    let big = Array.foldi bytearray ~init:zero ~f:aux in
    of_bigint big
end

(* *************** *
 * our permutation *
 * *************** *)

module Permutation = struct
  module Field = Field

  let rounds_full = 54

  let rounds_partial = 0

  let to_the_alpha x =
    let open Field in
    let res = x in
    let x_2 = res * res in
    let x_4 = x_2 * x_2 in
    let x_7 = x_4 * x_2 * x in
    x_7

  module Operations = struct
    let add_assign ~state i x = Field.(state.(i) <- state.(i) + x)

    let apply_affine_map (matrix, constants) v =
      let dotv row =
        Array.reduce_exn (Array.map2_exn row v ~f:Field.( * )) ~f:Field.( + )
      in
      let res = Array.map matrix ~f:dotv in
      Array.map2_exn res constants ~f:Field.( + )

    let copy a = Array.map a ~f:Fn.id
  end
end

(* ***************** *
 *   hash function   *
 * ***************** *)

module Hash = struct
  include Sponge.Make_hash (Sponge.Poseidon (Permutation))

  let params : Field.t Sponge.Params.t =
    Sponge.Params.(map pasta_p ~f:Field.of_string)

  let update ~state = update ~state params

  let hash ?init = hash ?init params

  let pack_input =
    Random_oracle_input.pack_to_fields ~size_in_bits:Field.size_in_bits
      ~pack:Field.project

  let prefix_to_field (s : string) =
    let bits_per_character = 8 in
    assert (bits_per_character * String.length s < Field.size_in_bits) ;
    Field.project Fold_lib.Fold.(to_list (string_bits (s :> string)))

  (* create a hash state from a string *)
  let salt (s : string) = update ~state:initial_state [|prefix_to_field s|]

  (* taken from src/lib/pickles *)
  let bits_to_bytes bits =
    let byte_of_bits bs =
      List.foldi bs ~init:0 ~f:(fun i acc b ->
          if b then acc lor (1 lsl i) else acc )
      |> Char.of_int_exn
    in
    List.map
      (List.groupi bits ~break:(fun i _ _ -> i mod 8 = 0))
      ~f:byte_of_bits
    |> String.of_char_list

  (* encode a digest in a hexstring *)
  let digest_to_hex digest =
    Hex.encode @@ bits_to_bytes @@ Field.to_bits digest
end

(* ************************ *
 *   javascript interface   *
 * ************************ *)

(* input is a raw string of bytes *)
let hash input =
  let string_to_bitstring s =
    let char_bits = String_sign.Message.char_bits in
    let x = Stdlib.(Array.of_seq (Seq.map char_bits (String.to_seq s))) in
    Random_oracle_input.bitstrings x
  in
  let init = Hash.initial_state in
  let input = Js.to_string input |> string_to_bitstring |> Hash.pack_input in
  let digest = Hash.hash ~init input in
  let open Snark_params_nonconsensus in
  Field.to_string digest |> Js.string

(* input is an array of field elements encoded as bytearrays *)
let hash_field_elems field_elems =
  let field_of_js_field x =
    Field.of_bytes @@ String.to_array
    @@ Js_of_ocaml.Typed_array.String.of_uint8Array x
  in
  let input = Js.to_array field_elems in
  let input = Array.map input ~f:field_of_js_field in
  let init = Hash.initial_state in
  let digest = Hash.hash ~init input in
  let digest_hex = Hash.digest_to_hex digest in
  Ok (Js.string digest_hex)
