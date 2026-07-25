let rec crossover l1 l2 =
  let rec contains hay needle =
    match hay with
    | x :: _ when x = needle -> true
    | _ :: rest -> contains rest needle
    | [] -> false
  in
  match l1 with
  | x :: rest when contains l2 x -> x :: crossover rest l2
  | _ :: rest -> crossover rest l2
  | [] -> []

let () =
  let print_list string_of_value lst =
    Printf.printf "[%s]\n" (String.concat "; " (List.map string_of_value lst))
  in
  print_list string_of_int
    (crossover [ 1; 2; 3; 4; 5; 6 ] [ 0; 2; 4; 6; 8; 10 ]);
  print_list string_of_int (crossover [] [ 0; 2; 4; 6; 8; 10 ]);
  print_list string_of_int (crossover [ 1; 2; 3; 4; 5; 6 ] []);
  print_list Fun.id (crossover [ "a"; "b"; "c" ] [ "x"; "y"; "z" ]);
  print_list Fun.id (crossover [ "a"; "b"; "c" ] [ "a"; "a"; "a" ]);
  print_list Fun.id (crossover [ "a"; "b"; "a" ] [ "a"; "x"; "y" ])
