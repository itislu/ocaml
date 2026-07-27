let pi = 4. *. atan 1.

let leibniz_pi target_delta =
  let leibniz i = (-1. ** i) /. ((2. *. i) +. 1.) in
  let delta s t = if s < t then t -. s else s -. t in
  let rec sigma sum i =
    let res = 4. *. sum in
    if delta res pi <= target_delta then (
      (* Printf.printf "%F\n" res; *) ();
      i)
    else sigma (sum +. leibniz (float_of_int i)) (i + 1)
  in
  if target_delta < 0. then -1 else sigma 0. 0

let () =
  (* Printf.printf "%F\n" pi; *)
  Printf.printf "%d\n" (leibniz_pi 0.000001)
