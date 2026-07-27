let ft_rot_n n str =
  let upper_a = int_of_char 'A' in
  let rot c =
    let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') in
    let to_upper v = v land 0x5F in
    let get_case_bit v = v land 0x20 in
    let set_case case_bit v = v lor case_bit in
    let rotate upper = ((upper - upper_a + n) mod 26) + upper_a in

    if is_alpha c then
      let ascii_val = int_of_char c in
      let case_bit = get_case_bit ascii_val in
      let upper = to_upper ascii_val in
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
  ft_rot_n 1 "NBzlk qnbjr !" |> Printf.printf "%s\n"
