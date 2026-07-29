let rec print_values = function
  | [] -> ()
  | value :: rest ->
      print_endline
        (string_of_int (Value.toInt value)
        ^ ": " ^ Value.toString value ^ " / "
        ^ Value.toStringVerbose value);
      print_values rest

let test_traversal traverse start =
  let rec loop value =
    print_endline (Value.toStringVerbose value);
    loop (traverse value)
  in
  try loop start with Invalid_argument message -> print_endline message

let () =
  print_endline "--- all ---";
  print_values Value.all;
  print_endline "--- next ---";
  test_traversal Value.next Value.T2;
  print_endline "--- previous ---";
  test_traversal Value.previous Value.As
