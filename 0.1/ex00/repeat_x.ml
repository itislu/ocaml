let repeat_x n =
  let rec recurse s i = if i = 0 then s else recurse (s ^ "x") (i - 1) in
  if n < 0 then "Error" else recurse "" n

let () =
  print_endline (repeat_x (-1));
  print_endline (repeat_x 0);
  print_endline (repeat_x 1);
  print_endline (repeat_x 2);
  print_endline (repeat_x 5);
  print_endline (repeat_x 90)
