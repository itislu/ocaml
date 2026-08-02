(*
00000
00001
00011
00010
00110
00111
00101
00100
01100
01101
01111
01110
01010
01011
01001
01000
11000
11001

0000
0001 3
0011 2
0010 3
0110 1
0111 3
0101 2
0100 3
1100 0
1101 3
1111 2
1110 3
1010 1
1011 3
1001 2
1000 3

00000
00001 4
00011 3
00010 4
00110 2
00111 4
00101 3
00100 4
01100 1
01101 4 <
01111 3
01110 4
01010 2
01011 4
01001 3
01000 4
11000 0
11001 4
      3
      4
      2
      4
      3
      4
      1
      4
      3
      4
      2
      4
      3
      4
*)

let flip = function '0' -> '1' | '1' -> '0' | c -> c
let flipi i_toflip i_cur c = if i_cur = i_toflip then flip c else c

let rec to_name s i =
  if i = String.length s then Printf.printf "%s " s
  else (
    to_name s (i + 1);
    to_name (String.mapi (flipi i) s) (i + 1))

let wrapper n =
  let s = ref (String.make n '0') in
  let rec to_name i =
    if i = n then Printf.printf "%s " !s
    else (
      to_name (i + 1);
      s := String.mapi (flipi i) !s;
      to_name (i + 1))
  in
  to_name 0

let gray n =
  (* to_name (String.make n '0') 0; *)
  if n < 0 then Printf.printf "Error" else wrapper n;
  Printf.printf "\n"

let () =
  gray (-1);
  gray 0;
  gray 1;
  gray 2;
  gray 3;
  gray 4
