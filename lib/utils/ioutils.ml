open Canonical
    
let read_at_loc (loc: Warn.warn_loc) =
  (* Produce a line-by-line stream from a file *)
  let stream_of_chan file =
    let chan = open_in file in
    Stream.from
      (fun _ -> try (Some (input_line chan))
        with End_of_file -> None)
  in

  let code_at_line (line: int) stream =
    let rec consume (read: int) stream =
      if read = line - 1 then
        try Stream.next stream with _ -> ""
      else
        (Stream.junk stream; consume (read + 1) stream)
    in
    consume 0 stream in

  loc.file |>
  stream_of_chan |>
  code_at_line loc.line_start |>
  (fun s -> String.sub s loc.col_start loc.col_end)
