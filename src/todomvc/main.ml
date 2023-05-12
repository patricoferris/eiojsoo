open Brr
open Brr_io
open Eio

let global_switch : Switch.t Fiber.key = Fiber.create_key ()
let keypresses : Ev.Keyboard.t Ev.type' = Ev.Type.create (Jstr.v "keypresses")

let fork_loop fn =
  match Fiber.get global_switch with
  | Some sw -> Fiber.fork ~sw (fun () -> while true do fn () done)
  | None -> failwith "Forget to set the global switch"

let on_click el fn =
  fork_loop (fun () ->
    let ev = Eio_browser.next_event Ev.click (El.as_target el) in
    fn ev
  )

let set_value el v =
  let jv = El.to_jv el in
  Jv.set jv "value" v

let get_value el =
  let el = Ev.target el in
  Ev.target_to_jv el |> fun v -> Jv.get v "value" |> Jv.to_jstr

(** Utility module for local storage. *)
module Storage = struct
  let storage = Storage.local G.window

  let key = Jstr.v "jsoo-todo-state"

  let find () = Storage.get_item storage key

  let set v =
    match Storage.set_item storage key v with
    | Ok () -> ()
    | Error e -> Jv.Error.message e |> Jstr.to_string |> failwith

  let init default = match find () with
    | None ->
      set default;
      default
    | Some v -> v
end

(** Application data *)
module Model = struct
  type visibility = Completed | Active | All

  let visibility_to_jv = function
    | Completed -> Jv.of_string "completed"
    | Active -> Jv.of_string "active"
    | All -> Jv.of_string "all"

  let visibility_of_jv j = match Jv.to_string j with
    | "completed" -> Completed
    | "active" -> Active
    | "all" -> All
    | _ -> assert false

  type task = {
    description : string;
    (* backup field keep the previous description to restore it when ESC key is pressed *)
    backup : string;
    completed : bool;
    editing : bool;
    id : int;
  }[@@deriving brr_jv]

  type t = {
    tasks : task list;
    field : string;
    uid : int;
    visibility : visibility;
  }[@@deriving brr_jv]

  let empty = {
    tasks = [];
    field = "";
    uid = 0;
    visibility = All;
  }

  let new_task desc id = {
    description = desc;
    backup = desc;
    completed = false;
    editing = false;
    id = id
  }

  let string_of_visibility v =
    match v with
    | Completed -> "Completed"
    | Active -> "Active"
    | All -> "All"

  let of_json s =
    match Json.decode s with
    | Ok jv -> of_jv jv
    | Error e -> Jv.Error.message e |> Jstr.to_string |> failwith

  let to_json m =
    to_jv m |> Json.encode

end

(** Utility module for routing *)
module Route = struct

  let visibility_from_url url =
    match Uri.fragment url |> Jstr.to_string with
    | "/" -> Model.All
    | "/active" -> Model.Active
    | "/completed" -> Model.Completed
    | _ -> Model.All

end

(** User interface actions *)
module Action = struct

  type action =
    | Update_field of Jstr.t
    | Editing_task of (int * bool)
    | Update_task of (int * Jstr.t)
    | Add of Jstr.t
    | Delete of int
    | Delete_complete
    | Check of (int * bool)
    | Check_all of bool
    | Change_visibility of Model.visibility
    | Escape of int

  let pp = function
    | Update_field _ -> Brr.Console.(log [ str "update-field"])
    | Editing_task _ -> Brr.Console.(log [ str "editing-task"])
    | Update_task (i, s) -> Brr.Console.(log [ str "update-task"; i; s])
    | Add _ -> Brr.Console.(log [ str "add"])
    | Delete _ -> Brr.Console.(log [ str "delete"])
    | Delete_complete -> Brr.Console.(log [ str "delete-field"])
    | Check _ -> Brr.Console.(log [ str "check"])
    | Check_all _ -> Brr.Console.(log [ str "check-all"])
    | Change_visibility _ -> Brr.Console.(log [ str "change-visibility"])
    | Escape _ -> Brr.Console.(log [ str "escape"])

end

(** The user actions are sent in this stream *)
let stream, (send_some : Action.action -> unit) =
  let s = Stream.create max_int in
  let add = Stream.add s in
  s, add


(** Build HTML and send user actions *)
module View = struct
  open Action

  let task_input =
    El.input ~at:[
      At.class' (Jstr.v "new-todo");
      At.placeholder (Jstr.v "What needs to be done?");
      At.autofocus
    ] ()

  (* New task input field *)
  let task_entry () =
    fork_loop (fun () ->
      let ev = Eio_browser.next_event Ev.input (El.as_target task_input) in
      send_some (Update_field (get_value ev))
    );
    fork_loop (fun () ->
      let ev = Eio_browser.next_event Ev.keydown (El.as_target task_input) in
      if Ev.Keyboard.key (Ev.as_type ev) = Jstr.v "Enter" then begin
        send_some (Add (get_value ev))
      end
    );
    El.header ~at:[At.class' (Jstr.v "header")] [
       El.h1 [ El.txt' "todos" ];
        task_input
    ]

  let set_task_input v =
    set_value task_input (Jv.of_string v)

  let focus_task_input () =
    El.set_has_focus true task_input

  (** One item in the tasks list *)
  let todo_item (todo : Model.task) =
    let at = [
      At.class' (Jstr.v "toggle");
      At.type' (Jstr.v "checkbox");
      At.if' todo.completed At.checked
    ] in
    let input_check =
      El.input ~at ()
    in
    let _ : Ev.listener = Ev.listen Ev.click (fun _ -> send_some (Check (todo.id, (not todo.completed)))) (El.as_target input_check) in
    let input_edit =
      El.input ~at:[
          At.type' (Jstr.v "text");
          At.class' (Jstr.v "edit");
          At.value (Jstr.v todo.description);
          At.id (Jstr.v @@ Fmt.str "todo-%u" todo.id);
        ] ()
    in
    fork_loop (fun () ->
      let _ : Ev.Focus.t Ev.t = Eio_browser.next_event Ev.blur (El.as_target input_edit) in
      send_some (Editing_task (todo.Model.id, false))
    );
    fork_loop (fun () ->
      let ev = Eio_browser.next_event Ev.input (El.as_target input_edit) in
      send_some (Update_task (todo.id, get_value ev))
    );
    fork_loop (fun () ->
      let key =
        Fiber.first
          (fun () -> Eio_browser.next_event Ev.keydown (El.as_target input_edit))
          (fun () -> Eio_browser.next_event keypresses (El.as_target input_edit))
      in
      let evt = Ev.as_type key in
      let code = Ev.Keyboard.code evt |> Jstr.to_int |> Option.get in
      if code = 13 then
        send_some (Editing_task (todo.id, false))
      else if code = 27 then
        send_some (Action.Escape todo.id)
      else ()
    );
    let css_class =
      if todo.completed then At.class' @@ Jstr.v "completed"
      else if todo.editing then At.class' @@ Jstr.v "editing" else At.void
    in
    let lbl =
      El.label [ El.txt' todo.Model.description ]
    in
    fork_loop (fun () ->
      let _ : Ev.Mouse.t Ev.t = Eio_browser.next_event Ev.dblclick (El.as_target lbl) in
      send_some (Editing_task (todo.id, true))
    );
    let btn =
      El.button ~at:[
        At.class' (Jstr.v "destroy")
      ] []
    in
    fork_loop (fun () ->
      let _ : Ev.Mouse.t Ev.t = Eio_browser.next_event Ev.dblclick (El.as_target btn) in
      send_some (Delete todo.Model.id)
    );
    El.li ~at:[css_class] [
      El.div ~at:[At.class' (Jstr.v "view")] [
        input_check;
        lbl;
        btn
      ];
      input_edit;
    ]

  let focus_todo_item id =
    let e = Document.find_el_by_id G.document (Jstr.v @@ Fmt.str "todo-%u" id) in
    Option.iter (El.set_has_focus true) e

  (** Build the tasks list *)
  let task_list visibility tasks =
    let is_visible todo =
      match visibility with
      | Model.Completed -> todo.Model.completed
      | Active -> not todo.completed
      | All -> true
    in
    let all_completed = List.for_all (fun e -> e.Model.completed) tasks in
    let css_visibility =
      match tasks with
      | [] -> "visibility: hidden;"
      | _ -> "visibility: visible;"
    in
    let toggle_input =
      El.input ~at:[
        At.type' (Jstr.v "checkbox");
        At.class' (Jstr.v "toggle-all");
        At.if' all_completed At.checked;
       ] ()
    in

    fork_loop (fun () ->
      let _ev : Ev.Mouse.t Ev.t = Eio_browser.next_event Ev.click (El.as_target toggle_input) in
      Brr.Console.log [ "check all" ];
      send_some (Check_all (not all_completed))
    );

    El.section ~at:[At.class' (Jstr.v "main"); At.style (Jstr.v css_visibility)] [
        toggle_input;
        El.label ~at:[At.for' (Jstr.v "toggle-all")] [El.txt' "Mark all as complete"];
        El.ul ~at:[At.class' (Jstr.v "todo-list")]
          (List.rev_map todo_item (List.filter is_visible tasks))
      ]

  let visibility_swap uri visibility actual_visibility =
    let css =
      if visibility = actual_visibility then ["selected"] else []
    in
    let li = El.li [
      El.a ~at:((At.href @@ Jstr.v uri) :: List.map (fun css -> At.class' (Jstr.v css)) css) [
        El.txt' (Model.string_of_visibility visibility)
      ]
    ]
    in
    on_click li (fun _ -> send_some (Change_visibility visibility));
    li

  let controls visibility tasks =
    let tasks_completed, tasks_left = List.partition (fun e -> e.Model.completed) tasks in
    let item = if (List.length tasks_left = 1) then " item" else " items" in
    let a_footer = [At.class' @@ Jstr.v "footer"] in
    let a_footer =
      match tasks with
      | [] -> At.hidden :: a_footer
      | _ -> a_footer
    in
    let a_button = [At.class' @@ Jstr.v "clear-completed"; ] in
    let a_button =
      match tasks_completed with
      | [] -> At.hidden :: a_button
      | _ -> a_button
    in
    let btn =
      El.button ~at:a_button [
          El.txt' "Clear completed"
        ]
    in
    on_click btn (fun _ev -> send_some Delete_complete);
    let html =
      El.footer ~at:a_footer [
        El.span ~at:[At.class' @@ Jstr.v "todo-count"] [
          El.strong [El.txt' (string_of_int (List.length tasks_left))];
          El.txt' (item ^ " left")
        ];
        El.ul ~at:[At.class' @@ Jstr.v "filters"] [
          visibility_swap "#/" Model.All visibility;
          visibility_swap "#/active" Model.Active visibility;
          visibility_swap "#/completed" Model.Completed visibility;
        ];
        btn
      ]
    in
    html

  let info_footer =
    El.footer ~at:[At.class' @@ Jstr.v "info"] [
      El.p [ El.txt' "Double-click to edit a todo" ];
      El.p [
        El.txt' "Written by ";
        El.a ~at:[At.href @@ Jstr.v "https://stephanelegrand.wordpress.com/"] [ El.txt' "Stéphane Legrand"]
      ];
      El.p [
        El.txt' "Various code improvements from ";
          El.a ~at:[At.href @@ Jstr.v "https://github.com/Drup"] [ El.txt' "Gabriel Radanne" ]
        ];
        El.p [
          El.txt' "Based on ";
          El.a ~at:[At.href @@ Jstr.v "https://github.com/evancz"] [ El.txt' "Elm implementation by Evan Czaplicki" ]
        ];
        El.p [
          El.txt' "Part of ";
          El.a ~at:[At.href @@ Jstr.v "http://todomvc.com"] [ El.txt' "TodoMVC" ]
        ]
      ]

  (** Build the HTML for the application *)
  let view () =
    let task_entry = task_entry () in
  fun m ->
    El.(
      div ~at:[At.class' @@ Jstr.v "todomvc-wrapper"] [
        section ~at:[At.class' @@ Jstr.v "todoapp"] [
          task_entry;
          task_list m.Model.visibility m.Model.tasks ;
          controls m.Model.visibility m.Model.tasks
        ];
        info_footer
      ])

  let refresh view parent m =
    El.set_children parent [ view m ]
end

(** Manage actions, refresh view if needed and save the state in local storage *)
module Controler =
struct

  let update view parent a m =
    let open Action in
    let open Model in
    let m = match a with
      | Add field ->
        let field = Jstr.to_string field in
        let uid = m.uid + 1 in
        let tasks =
          let v = String.trim field in
          if v = "" then m.tasks
          else (new_task v m.uid) :: m.tasks
        in
        { m with uid = uid; field = ""; tasks = tasks }
      | Update_field field ->
        { m with field = Jstr.to_string field }
      | Editing_task (id, is_edit) ->
        let update_task t =
          if (t.id = id) then
            let v = String.trim t.description in
            { t with editing = is_edit; description = v; backup = v }
          else { t with editing = false }
        in
        let l = List.map update_task m.tasks in
        let l = List.filter (fun e -> e.description <> "") l in
        { m with tasks = l }
      | Update_task (id, task) ->
        let update_task t =
          if (t.id = id) then { t with description = Jstr.to_string task }
          else t
        in
        { m with tasks = List.map update_task m.tasks }
      | Delete id ->
        { m with tasks = List.filter (fun e -> e.id <> id) m.tasks }
      | Delete_complete ->
        { m with tasks = List.filter (fun e -> not e.completed) m.tasks }
      | Check (id, is_compl) ->
        let update_task t =
          if (t.id = id) then { t with completed = is_compl }
          else t
        in
        { m with tasks = List.map update_task m.tasks }
      | Check_all is_compl ->
        let update_task t =
          { t with completed = is_compl }
        in
        { m with tasks = List.map update_task m.tasks }
      | Change_visibility visibility ->
        { m with visibility = visibility }
      | Escape id ->
        let unedit_task t =
          if (t.id = id) then { t with editing = false; description = t.backup }
          else t
        in
        { m with tasks = List.map unedit_task m.tasks }
    in
    begin match a with
      | Add _ -> View.set_task_input "" ;
      | _ -> ()
    end ;
    begin match a with
      | Update_field _
      | Update_task _ -> ()
      | _ -> View.refresh view parent m
    end ;
    begin match a with
      | Update_field _
      | Update_task _ -> ()
      | Editing_task (_, is_edit) ->
        if not is_edit then View.focus_task_input ()
      | _ -> View.focus_task_input () ;
    end ;
    begin match a with
      | Editing_task (id, is_edit) ->
        if is_edit then View.focus_todo_item id
      | _ -> ();
    end ;
    Storage.set @@ Model.to_json m;
    m

end

let main () =
  let doc = G.document in
  let parent =
    match Document.find_el_by_id doc (Jstr.v "todomvc") with
    | Some v -> v
    | None -> assert false
  in
  (* restore the saved state or empty state if not found *)
  let m =
    try
      Model.of_json @@ Storage.init @@ Model.to_json Model.empty
    with
    | _ -> Model.empty
  in
  (* set the visibility by looking at the current url *)
  let m =
    let u = Window.location G.window in
    let v = Route.visibility_from_url u in
    { m with Model.visibility = v }
  in
  (* init the view *)
  let init = View.view () in
  View.refresh init parent m ;
  View.set_task_input m.Model.field ;
  View.focus_task_input () ;
  (* main loop *)
  let rec run m =
    let action = Stream.take stream in
    Action.pp action;
    let m = Controler.update init parent action m in
    run m
  in
  run m

let () =
  let run_main () =
    let _ : Ev.Type.void Ev.t = Eio_browser.next_event Ev.load (Window.as_target G.window) in
    main ()
  in
  let run_hashchanges () =
    while true do
      let hc = Eio_browser.next_event Ev.hashchange (Window.as_target G.window) in
      let url = Ev.Hash_change.new_url @@ Ev.as_type hc in
      let uri = Uri.of_jstr url |> Result.get_ok in
      let v = Route.visibility_from_url uri in
      send_some (Change_visibility v)
    done
  in
  let main =
    Eio_browser.run @@ fun () ->
    Switch.run @@ fun sw ->
    Fiber.with_binding global_switch sw @@ fun () ->
    Fiber.both run_main run_hashchanges
  in
  Fut.await main (fun () -> Brr.Console.(log [ str "All done" ]))
