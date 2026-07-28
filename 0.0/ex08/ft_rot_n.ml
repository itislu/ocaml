let ft_rot_n n str =
  let upper_a = int_of_char 'A' in
  let rot c =
    let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') in
    let case_bit v = v land 0b00100000 in
    let to_upper v = v land 0b01011111 in
    let set_case case_bit v = v lor case_bit in
    let rotate upper =
      let alpha_len = 26 in
      ((((upper - upper_a + n) mod alpha_len) + alpha_len) mod alpha_len)
      + upper_a
    in

    if is_alpha c then
      let v = int_of_char c in
      let case_bit = case_bit v in
      let upper = to_upper v in
      rotate upper |> set_case case_bit |> char_of_int
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
  ft_rot_n 1 "NBzlk qnbjr !" |> Printf.printf "%s\n";
  ft_rot_n (-1) "abcdefghijklmnopqrstuvwxyz" |> Printf.printf "%s\n"
