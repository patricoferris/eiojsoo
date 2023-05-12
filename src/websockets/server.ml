open Eio
open Websocket

let js_response body =
  let headers =
    Http.Header.of_list
      [
        ("content-type", "application/javascript; charset=UTF-8");
        ("content-length", string_of_int @@ String.length body);
      ]
  in
  let response =
    Http.Response.make ~version:`HTTP_1_1 ~status:`OK ~headers ()
  in
  (response, Cohttp_eio.Body.Fixed body)

let handler ~sw ~js ~index : Cohttp_eio.Server.handler =
 fun ((req, _body, _stream) as request) ->
  let open Frame in
  let uri = Http.Request.resource req in
  match uri with
  | "/" | "/index.html" -> Cohttp_eio.Server.html_response index
  | "/index.js" -> js_response js
  | "/ping" ->
      traceln "[PATH] /ws" ;
      let resp, send_frame =
        Websocket_eio.upgrade_connection request (fun {opcode; content; _} ->
            match opcode with
            | Opcode.Close -> traceln "[RECV] CLOSE"
            | _ -> traceln "[RECV] %s" content ) in
      (* send a message to the client every second *)
      Fiber.fork ~sw (fun () ->
        let num_ref = ref 0 in
        while true do
          let msg = Printf.sprintf "-> Ping %d" !num_ref in
          traceln "[SEND] %s" msg;
          send_frame @@ Frame.create ~content:msg ();
          incr num_ref;
          Eio_unix.sleep 1.
        done
      );
      resp
  | _ -> Cohttp_eio.Server.not_found_handler request

let start_server ~index ~js env sw port =
  traceln "[SERV] Listening for HTTP on port %d" port ;
  Cohttp_eio.Server.run ~port env (handler ~index ~js ~sw)

let () =
  let port = ref 7777 in
  Eio_main.run @@ fun env ->
  Switch.run @@ fun sw ->
  let index = Path.(load (env#fs / "_build" / "default" / "src" / "websockets" / "index.html")) in
  let js = Path.(load (env#fs / "_build" / "default" / "src" / "websockets" / "index.js")) in
  start_server ~index ~js env sw !port