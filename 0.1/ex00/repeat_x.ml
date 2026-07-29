let repeat_x n =
  let rec recurse s i = if i = 0 then s else recurse (s ^ "x") (i - 1) in
  if n < 0 then "Error" else recurse "" n

let () =
  let print_repeat_x n =
    print_string (repeat_x n);
    print_char '\n'
  in
  print_repeat_x (-1);
  print_repeat_x 0;
  print_repeat_x 1;
  print_repeat_x 2;
  print_repeat_x 5;
  print_repeat_x 90
