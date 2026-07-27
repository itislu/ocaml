let ft_string_all p s =
  let len = String.length s in
  let rec test i =
    if i = len then true else p (String.get s i) && test (i + 1)
  in
  test 0

let () =
  let is_digit c = c >= '0' && c <= '9' in
  Printf.printf "%b\n" (ft_string_all is_digit "0123456789");
  Printf.printf "%b\n" (ft_string_all is_digit "O12EAS67B9");
  Printf.printf "%b\n" (ft_string_all is_digit "")
