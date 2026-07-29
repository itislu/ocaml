let repeat_string ?(str = "x") n =
  let rec recurse res i = if i = 0 then res else recurse (res ^ str) (i - 1) in
  if n < 0 then "Error" else recurse "" n

let () =
  let print_repeat_string ?str n = print_endline (repeat_string ?str n) in
  print_repeat_string (-1);
  print_repeat_string 0;
  print_repeat_string ~str:"Toto" 1;
  print_repeat_string 2;
  print_repeat_string ~str:"a" 5;
  print_repeat_string ~str:"what" 3
