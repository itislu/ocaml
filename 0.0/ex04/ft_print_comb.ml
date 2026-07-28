let ft_print_comb () =
  let rec print3 n1 n2 n3 =
    if n3 <= 9 then (
      print_int n1;
      print_int n2;
      print_int n3;
      if n1 < 7 then print_string ", ";
      print3 n1 n2 (n3 + 1))
  in
  let rec print2 n1 n2 =
    if n2 <= 8 then (
      print3 n1 n2 (n2 + 1);
      print2 n1 (n2 + 1))
  in
  let rec print1 n1 =
    if n1 <= 7 then (
      print2 n1 (n1 + 1);
      print1 (n1 + 1))
  in
  print1 0;
  print_string "\n"

let () = ft_print_comb ()
