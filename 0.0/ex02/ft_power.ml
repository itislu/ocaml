let rec ft_power n e = if e = 0 then 1 else n * ft_power n (e - 1)

let () =
  let print_ft_power n e =
    print_int (ft_power n e);
    print_char '\n'
  in
  print_ft_power 2 4;
  print_ft_power 3 0;
  print_ft_power 0 5
