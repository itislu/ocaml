let ft_print_rev s =
  let rec print i =
    if i >= 0 then (
      print_char (String.get s i);
      print (i - 1))
  in
  print (String.length s - 1);
  print_char '\n'

let () =
  ft_print_rev "Hello World !";
  ft_print_rev "";
  ft_print_rev "12221131"
