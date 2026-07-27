let ft_is_palindrome s =
  let tail = String.length s - 1 in
  let stop = tail / 2 in
  let rec test i =
    if i = stop then true
    else String.(get s i = get s (tail - i)) && test (i + 1)
  in
  test 0

let () =
  Printf.printf "%b\n" (ft_is_palindrome "radar");
  Printf.printf "%b\n" (ft_is_palindrome "madam");
  Printf.printf "%b\n" (ft_is_palindrome "car");
  Printf.printf "%b\n" (ft_is_palindrome "");
  Printf.printf "%b\n" (ft_is_palindrome "abba");
  Printf.printf "%b\n" (ft_is_palindrome "abbc");
  Printf.printf "%b\n" (ft_is_palindrome "1")
