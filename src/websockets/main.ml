open Brr
open Brr_io
open Eio

module Websocket :sig
  type t
  (** A websocket *)

  val create : sw:Switch.t -> Jstr.t -> t
  (** Creates a new socket connected to url attached to [sw]. *)

  val send : t -> Jstr.t -> unit
  (** Sends a message over the websocket. *)

  val recv : t -> Message.Ev.t
  (** Receives a new message on the websocket, yielding if none have arrived. *)

  val close : t -> unit
  (** Close the websocket. *)
end = struct
  type t = {
    socket : Websocket.t;
    sw : Switch.t;
    hook : Switch.hook;
    s : Message.Ev.t Ev.t Stream.t;
  }

  let create ~sw uri =
    let socket = Websocket.create uri in
    let s = Stream.create max_int in
    (* Must use stream + event listener in order to buffer messages *)
    let _ : Ev.listener = Brr.Ev.listen Message.Ev.message (Stream.add s) (Websocket.as_target socket) in
    let hook = Switch.on_release_cancellable sw (fun () -> Websocket.close socket) in
    { socket; hook; sw; s }

  let send t msg = Websocket.send_string t.socket msg
  let recv t = Stream.take t.s |> Ev.as_type

  let close t =
    Switch.remove_hook t.hook;
    Websocket.close t.socket
end

let () =
  let main =
    Eio_browser.run @@ fun () ->
    Switch.run @@ fun sw ->
    let app = Document.find_el_by_id G.document (Jstr.v "app") |> Option.get in
    let ws = Websocket.create ~sw (Jstr.v "ws://localhost:7777/ping") in
    Fiber.fork ~sw (fun () ->
      while true do
        let msg : Jstr.t = Websocket.recv ws |> Message.Ev.data in
        El.append_children app [ El.p [ El.txt' "[RECV]"; El.txt msg ] ];
        Eio_browser.Timeout.sleep ~ms:1000;
        Websocket.send ws msg
      done
    )
  in
  ignore (main : unit Fut.t)