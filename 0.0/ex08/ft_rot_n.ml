let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

let ft_rot_n n str =
  let ascii_A = int_of_char 'A' in
  let rot c =
    if is_alpha c then
      let ascii = int_of_char c in
      let case_bit = ascii land 0x20 in
      let upper = ascii land 0x5F in
      (((upper - ascii_A + n) mod 26) + ascii_A) lor case_bit |> char_of_int
    else c
  in
  String.map rot str

let () =
  ft_rot_n 1 "abcdefghijklmnopqrstuvwxyz" |> Printf.printf "%s\n";
  ft_rot_n 13 "abcdefghijklmnopqrstuvwxyz" |> Printf.printf "%s\n";
  ft_rot_n 42 "0123456789" |> Printf.printf "%s\n";
  ft_rot_n 2 "OI2EAS67B9" |> Printf.printf "%s\n";
  ft_rot_n 0 "Damned !" |> Printf.printf "%s\n";
  ft_rot_n 42 "" |> Printf.printf "%s\n";
  ft_rot_n 1 "NBzlk qnbjr !" |> Printf.printf "%s\n"
