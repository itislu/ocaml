let rec ft_countdown n =
  print_int (if n < 0 then 0 else n);
  print_char '\n';
  if n > 0 then ft_countdown (n - 1)

let () =
  ft_countdown 3;
  ft_countdown 0;
  ft_countdown (-1);
  ft_countdown (-100)
