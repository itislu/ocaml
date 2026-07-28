let fibonacci n =
  let rec fib_helper n1 n2 i =
    if i = 0 then n1 else fib_helper n2 (n1 + n2) (i - 1)
  in
  if n < 0 then -1 else fib_helper 0 1 n

let () =
  let print_fibonacci n =
    print_int (fibonacci n);
    print_char '\n'
  in
  print_fibonacci (-1);
  print_fibonacci 0;
  print_fibonacci 1;
  print_fibonacci 2;
  print_fibonacci 3;
  print_fibonacci 4;
  print_fibonacci 5;
  print_fibonacci 20
