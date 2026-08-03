(* --- ex04 (added U) --- *)
type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | U | None
type nucleotide = { p : phosphate; d : deoxyribose; base : nucleobase }

let generate_nucleotide = function
  | 'A' -> { p = "phosphate"; d = "deoxyribose"; base = A }
  | 'T' -> { p = "phosphate"; d = "deoxyribose"; base = T }
  | 'C' -> { p = "phosphate"; d = "deoxyribose"; base = C }
  | 'G' -> { p = "phosphate"; d = "deoxyribose"; base = G }
  | 'U' -> { p = "phosphate"; d = "deoxyribose"; base = U }
  | _ -> { p = "phosphate"; d = "deoxyribose"; base = None }

(* --- ex05 (added U) --- *)
type helix = nucleotide list

let rec generate_helix n : helix =
  if n <= 0 then []
  else
    let base =
      match Random.int 4 with 0 -> 'A' | 1 -> 'T' | 2 -> 'C' | _ -> 'G'
    in
    generate_nucleotide base :: generate_helix (n - 1)

let nucleobase_to_string = function
  | A -> "A"
  | T -> "T"
  | C -> "C"
  | G -> "G"
  | U -> "U"
  | None -> ""

let rec helix_to_string : helix -> string = function
  | [] -> ""
  | { p; d; base } :: rest -> nucleobase_to_string base ^ helix_to_string rest

let complementary_nucleobase = function
  | A -> T
  | T -> A
  | C -> G
  | G -> C
  | U -> U
  | None -> None

let rec complementary_helix : helix -> helix = function
  | [] -> []
  | { p; d; base } :: rest ->
      { p; d; base = complementary_nucleobase base } :: complementary_helix rest

(* --- ex06 --- *)
type rna = nucleobase list

let t_to_u = function T -> U | base -> base

let rec extract_base = function
  | [] -> []
  | { p; d; base } :: rest -> t_to_u base :: extract_base rest

let generate_rna (h : helix) : rna = complementary_helix h |> extract_base

let () =
  Random.self_init ();
  generate_helix 10 |> generate_rna
  |> List.map nucleobase_to_string
  |> String.concat "" |> print_endline
