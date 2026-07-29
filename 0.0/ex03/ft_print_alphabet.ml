let ft_print_alphabet () =
  let next c = char_of_int (int_of_char c + 1) in
  let rec print c =
    print_char c;
    if c < 'z' then print (next c)
  in
  print 'a';
  print_char '\n'

let () = ft_print_alphabet ()
