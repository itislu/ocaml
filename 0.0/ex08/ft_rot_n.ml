let ft_rot_n n str =
  let upper_a = int_of_char 'A' in
  let rot_char c =
    let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') in
    let case_bit v = v land 0b00100000 in
    let to_upper v = v land 0b01011111 in
    let set_case case_bit v = v lor case_bit in
    let rotate_upper v =
      let alpha_len = 26 in
      ((((v - upper_a + n) mod alpha_len) + alpha_len) mod alpha_len) + upper_a
    in

    if is_alpha c then
      let v = int_of_char c in
      to_upper v |> rotate_upper |> set_case (case_bit v) |> char_of_int
    else c
  in
  String.map rot_char str

let () =
  ft_rot_n 1 "abcdefghijklmnopqrstuvwxyz" |> print_endline;
  ft_rot_n 13 "abcdefghijklmnopqrstuvwxyz" |> print_endline;
  ft_rot_n 42 "0123456789" |> print_endline;
  ft_rot_n 2 "OI2EAS67B9" |> print_endline;
  ft_rot_n 0 "Damned !" |> print_endline;
  ft_rot_n 42 "" |> print_endline;
  ft_rot_n 1 "NBzlk qnbjr !" |> print_endline;
  ft_rot_n (-1) "abcdefghijklmnopqrstuvwxyz" |> print_endline
