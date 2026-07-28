type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | None
type nucleotide = { p : phosphate; d : deoxyribose; base : nucleobase }

let generate_nucleotide = function
  | 'A' -> { p = "phosphate"; d = "deoxyribose"; base = A }
  | 'T' -> { p = "phosphate"; d = "deoxyribose"; base = T }
  | 'C' -> { p = "phosphate"; d = "deoxyribose"; base = C }
  | 'G' -> { p = "phosphate"; d = "deoxyribose"; base = G }
  | _ -> { p = "phosphate"; d = "deoxyribose"; base = None }

let () =
  let string_of_nucleobase = function
    | A -> "A"
    | T -> "T"
    | C -> "C"
    | G -> "G"
    | None -> "None"
  in

  let print_nucleotide { p; d; base } =
    Printf.printf "%s, %s, %s\n" p d (string_of_nucleobase base)
  in

  print_nucleotide (generate_nucleotide 'A');
  print_nucleotide (generate_nucleotide 'T');
  print_nucleotide (generate_nucleotide 'C');
  print_nucleotide (generate_nucleotide 'G');
  print_nucleotide (generate_nucleotide 'Z')
