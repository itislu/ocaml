let print_num n =
  if n < 10 then print_int 0;
  print_int n

let print n1 n2 =
  if n2 > 1 then (
    print_char ',';
    print_char ' ');
  print_num n1;
  print_char ' ';
  print_num n2

let rec inc_n2 n1 n2 =
  print n1 n2;
  if n2 < 99 then inc_n2 n1 (n2 + 1)

let rec inc_n1 n1 =
  let n2 = n1 + 1 in
  inc_n2 n1 n2;
  if n1 < 98 then inc_n1 n2

let ft_print_comb2 () =
  inc_n1 0;
  print_char '\n'

let () = ft_print_comb2 ()
