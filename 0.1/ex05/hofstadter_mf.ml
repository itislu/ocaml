let rec hfs_f n =
  let rec hfs_f_nonneg n =
    if n = 0 then 1 else n - hfs_m (hfs_f_nonneg (n - 1))
  in
  if n < 0 then -1 else hfs_f_nonneg n

and hfs_m n =
  let rec hfs_m_nonneg n =
    if n = 0 then 0 else n - hfs_f (hfs_m_nonneg (n - 1))
  in
  if n < 0 then -1 else hfs_m_nonneg n

let () =
  hfs_m 0 |> Printf.printf "%d\n";
  hfs_f 0 |> Printf.printf "%d\n";
  hfs_m 4 |> Printf.printf "%d\n";
  hfs_f 4 |> Printf.printf "%d\n";
  hfs_m (-4) |> Printf.printf "%d\n";
  hfs_f (-4) |> Printf.printf "%d\n"
