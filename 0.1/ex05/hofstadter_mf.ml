let rec hfs_f =
  let hfs_f_nonneg = function 0 -> 1 | n -> n - hfs_m (hfs_f (n - 1)) in
  function n when n < 0 -> -1 | n -> hfs_f_nonneg n

and hfs_m =
  let hfs_m_nonneg = function 0 -> 0 | n -> n - hfs_f (hfs_m (n - 1)) in
  function n when n < 0 -> -1 | n -> hfs_m_nonneg n

let () =
  hfs_m 0 |> Printf.printf "%d\n";
  hfs_f 0 |> Printf.printf "%d\n";
  hfs_m 4 |> Printf.printf "%d\n";
  hfs_f 4 |> Printf.printf "%d\n";
  hfs_m (-4) |> Printf.printf "%d\n";
  hfs_f (-4) |> Printf.printf "%d\n"
