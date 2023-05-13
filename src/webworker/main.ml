(*---------------------------------------------------------------------------
   Copyright (c) 2020 The brr programmers
   Copyright (c) 2023 Patrick Ferris

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)

open Brr
open Brr_io
open Brr_webworkers

let recv_from_page e =
  let data = (Message.Ev.data (Ev.as_type e) : Jstr.t) in
  match Jstr.to_string data with
  | "Work!" -> Worker.G.post Jstr.(v "Page said: " + data + v " I say Revolt!")
  | _ -> assert false

let worker_main () =
  Console.(log [str "Worker hello!"]);
  let msg = Eio_browser.next_event Message.Ev.message G.target in
  recv_from_page msg

let spawn_worker () = try Ok (Worker.create (Jstr.v "index.js")) with
| Jv.Error e -> Error e

let recv_from_worker ~view e =
  let data : Jstr.t = Message.Ev.data (Ev.as_type e) in
  El.set_children view [El.p El.[txt Jstr.(v "Worker says: " + data)]]

let page_main () =
  let h1 = El.h1 [El.txt' "Test workers"] in
  let info = El.p [ El.strong [El.txt' "Note."];
                    El.txt' " Doesn't work over the file:// protocol."]
  in
  let view = El.div [] in
  El.set_children (Document.body G.document) [h1; info; view];
  match spawn_worker () with
  | Error e -> El.set_children view [El.p El.[txt (Jv.Error.message e)]]
  | Ok w ->
    Worker.post w (Jstr.v "Work!");
    let msg = Eio_browser.next_event Message.Ev.message (Worker.as_target w) in
    recv_from_worker ~view msg

let main () =
  Eio_browser.run @@ fun () ->
  if Worker.ami () then worker_main () else page_main ()

let () =
  ignore (main () : unit Fut.t)
