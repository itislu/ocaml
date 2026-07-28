let rec contains hay needle =
  match hay with x :: rest -> x = needle || contains rest needle | [] -> false

let rec crossover l1 l2 =
  match l1 with
  | x :: rest ->
      let result = crossover rest l2 in
      if contains l2 x then x :: result else result
  | [] -> []

let () =
  let print_list string_of_value lst =
    Printf.printf "[%s]\n" (String.concat "; " (List.map string_of_value lst))
  in
  crossover [ 1; 2; 3; 4; 5; 6 ] [ 0; 2; 4; 6; 8; 10 ]
  |> print_list string_of_int;
  crossover [] [ 0; 2; 4; 6; 8; 10 ] |> print_list string_of_int;
  crossover [ 1; 2; 3; 4; 5; 6 ] [] |> print_list string_of_int;
  crossover [ "a"; "b"; "c" ] [ "x"; "y"; "z" ] |> print_list Fun.id;
  crossover [ "a"; "b"; "c" ] [ "a"; "a"; "a" ] |> print_list Fun.id;
  crossover [ "a"; "b"; "a" ] [ "a"; "x"; "y" ] |> print_list Fun.id
