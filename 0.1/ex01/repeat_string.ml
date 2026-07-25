let repeat_string ?str n =
  let repeat s n =
    let rec helper res i = if i = 0 then res else helper (res ^ s) (i - 1) in
    if n < 0 then "Error" else helper "" n
  in
  match str with Some s -> repeat s n | None -> repeat "x" n

let () =
  let print_repeat_string ?str n =
    print_string (repeat_string ?str n);
    print_char '\n'
  in
  print_repeat_string (-1);
  print_repeat_string 0;
  print_repeat_string ~str:"Toto" 1;
  print_repeat_string 2;
  print_repeat_string ~str:"a" 5;
  print_repeat_string ~str:"what" 3
