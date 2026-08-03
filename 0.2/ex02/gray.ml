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
let fst (a, _) = a

let gray n =
  if n < 0 then print_endline "Error"
  else
    let rec generate i acc s =
      if i = n then (acc ^ " " ^ s, s)
      else
        let acc, s = generate (i + 1) acc s in
        generate (i + 1) acc (String.mapi (flipi i) s)
    in
    String.make n '0' |> generate 0 "" |> fst |> String.trim |> print_endline

let () =
  gray (-1);
  gray 0;
  gray 1;
  gray 2;
  gray 3;
  gray 4
