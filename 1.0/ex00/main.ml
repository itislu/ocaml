include Color

let () =
  List.map toString all |> List.iter print_endline;
  List.map toStringVerbose all |> List.iter print_endline
