let encode lst =
  let rec count lst n =
    match lst with
    | x :: y :: rest when x = y -> count (y :: rest) (n + 1)
    | x :: rest -> (n, x) :: count rest 1
    | [] -> []
  in
  count lst 1

let () =
  let print_list string_of_element lst =
    Printf.printf "[%s]\n" (String.concat "; " (List.map string_of_element lst))
  in
  let string_of_tuple string_of_value (count, value) =
    Printf.sprintf "(%d, %s)" count (string_of_value value)
  in
  print_list
    (string_of_tuple Char.escaped)
    (encode [ 'a'; 'a'; 'a'; 'b'; 'b'; 'b' ]);
  print_list (string_of_tuple string_of_int) (encode [ 1; 1; 2; 3; 3; 3 ]);
  print_list (string_of_tuple Fun.id) (encode [ "hello"; "hello" ]);
  print_list (string_of_tuple Fun.id) (encode [ "Perl" ]);
  print_list (string_of_tuple Fun.id) (encode [])
