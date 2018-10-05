{client{

  exception Wrong_dom_value

  (* empty value is to handle correclty option for list and array
     When a value is missing from a list or an array, we don't want it to be set at None.
     We delete the value that generate a error, and keep the rest of the list/array
  *)
  exception Empty_value

  open Eliom_content
  open Html
  open D

  type 'a dom_value = [
    | `Input of Html_types.input Eliom_content.Html.D.elt
    | `Textarea of Html_types.textarea Eliom_content.Html.D.elt
    | `Select of Html_types.select Eliom_content.Html.D.elt * ((string * ('a dom_ext Lazy.t) list) list)
    | `List of 'a dom_ext list
    | `Record of (string * 'a dom_ext) list
  ]

  and ('a) dom_ext = {
    node : 'a Eliom_content.Html.D.elt ;
    mutable value_ : 'a dom_value ;
    error : Html_types.p Eliom_content.Html.D.elt option;
  }

  let node d = d.node
  let value d = d.value_

  (** UTILES **)
  let get_value dom = Js.to_string ((Html.To_dom.of_input dom)##value)
  let set_value e s = (Html.To_dom.of_input e)##value <- Js.string s
  let empty_value e = (Html.To_dom.of_input e)##value <- Js.string ""

  let get_opt_value dom =
    let s = get_value dom in
    if s = "" then None
    else Some s

  let get_value_select e = Js.to_string ((Html.To_dom.of_select e)##value)
  let set_value_select e s = (Html.To_dom.of_select e)##value <- Js.string s
  let select_index e i = (To_dom.of_select e)##selectedIndex <- i

  let get_value_textarea dom = Js.to_string ((Html.To_dom.of_textarea dom)##value)
  let set_value_textarea e s = (Html.To_dom.of_textarea e)##value <- Js.string s

  let get_opt_value_textarea dom =
    let s = get_value_textarea dom in
    if s = "" then None
    else Some s

  (** /UTILES **)

  module type Dom_type = sig
    type a
    val to_default : ?v:a -> unit -> [ Html_types.div_content_fun ] dom_ext
    val to_dom : a -> [ Html_types.div_content_fun ] dom_ext

    val save : [ Html_types.div_content_fun ] dom_ext -> a
  end

  module Default(D : Dom_type) : Dom_type with type a = D.a = struct
    include D
  end

  let handle_exception error_dom t v f =
    try
      f v
    with exn ->
      begin match error_dom with
        | Some error_dom ->
          let error = Printf.sprintf "Expected value of type %s but '%s' was given" t (match v with | Some v -> v | None -> "") in
          Manip.SetCss.display error_dom "block" ;
          Manip.replaceChildren error_dom [ pcdata error ]
        | None -> ()
      end;
      raise exn

  module Dom_type_int = Default(struct
      type a = int

      let to_default ?v () =
        let v =
          match v with
            | Some v ->
              a_value (string_of_int v)
            | None ->
              a_value ""
        in

        let error = p ~a:[ a_class ["error"]; a_style "display:none"] [ ] in
        let d = input ~a:[ v; a_input_type `Text ] () in
        {
          node = div ~a:[ a_class ["dom_ext_int"] ] [ error; d ] ;
          value_ = `Input d;
          error = Some error;
        }

      let to_dom i =
        to_default ~v:i ()

      let save d =
        match d.value_ with
          | `Input i ->
            let v = get_opt_value i in
            handle_exception d.error "int" v (
              function
                | Some i -> int_of_string i
                | None -> raise Empty_value
            )
          | _ -> raise Wrong_dom_value

    end)


  module Dom_type_int32 = Default(struct
      type a = int32

      let to_default ?v () =
        let v =
          match v with
            | Some v ->
              a_value (Int32.to_string v)
            | None ->
              a_value ""
        in

        let error = p ~a:[ a_class ["error"]; a_style "display:none"] [ ] in
        let d = input ~a:[ v; a_input_type `Text] () in
        {
          node = div ~a:[ a_class ["dom_ext_int"; "dom_ext_int32"] ] [ error; d ] ;
          value_ = `Input d;
          error = Some error ;
        }

      let to_dom i =
        to_default ~v:i ()

      let save d =
        match d.value_ with
          | `Input i ->
            let v = get_opt_value i in
            handle_exception d.error "int32" v (
              function
                | Some i -> Int32.of_string i
                | None -> raise Empty_value
            )
          | _ -> raise Wrong_dom_value

    end)

  module Dom_type_int64 = Default(struct
      type a = int64

      let to_default ?v () =
        let v =
          match v with
            | Some v ->
              a_value (Int64.to_string v)
            | None ->
              a_value ""
        in

        let error = p ~a:[ a_class ["error"]; a_style "display:none"] [ ] in
        let d = input ~a:[ v; a_input_type `Text ] () in
        {
          node = div ~a:[ a_class ["dom_ext_int"; "dom_ext_int64"] ] [ error; d ] ;
          value_ = `Input d;
          error = Some error ;
        }

      let to_dom i =
        Firebug.console##debug (Js.string "lol");
        to_default ~v:i ()

      let save d =
        match d.value_ with
        | `Input i ->
          let v = get_opt_value i in
          handle_exception d.error "int64" v (
            function
              | Some i -> Int64.of_string i
              | None -> raise Empty_value
          )
        | _ -> raise Wrong_dom_value

    end)

  module Dom_type_bool = Default(struct
      type a = bool

      let to_default ?v () =
        let sel =
          Raw.select [
            option (pcdata "");
            option ~a:[ a_value "true" ] (pcdata "true");
            option ~a:[ a_value "false" ] (pcdata "false");
          ]
        in

        begin
          match v with
            | Some v ->
              if v then select_index sel 1
              else select_index sel 2
            | None -> ()
        end;

        {
          node = div ~a:[ a_class ["dom_ext_bool"]] [ sel ];
          value_ = `Select (sel, []);
          error = None;
        }

      let to_dom b =
        to_default ~v:b ()

      let save d =
        match d.value_ with
          | `Select (sel, []) ->
            begin
              match get_value_select sel with
                | "true" -> true
                | "false" -> false
                | _ -> raise Empty_value
            end
          | _ -> raise Wrong_dom_value
    end)

  module Dom_type_float = Default(struct
      type a = float

      let to_default ?v () =
        let v =
          match v with
            | Some v ->
              a_value (string_of_float v)
            | None ->
              a_value ""
        in

        let error = p ~a:[ a_class ["error"]; a_style "display:none"] [ ] in
        let d = input ~a:[ v; a_input_type `Text ] () in
        {
          node = div ~a:[ a_class ["dom_ext_float"] ] [ error; d ] ;
          value_ = `Input d;
          error = Some error ;
        }

      let to_dom f =
        to_default ~v:f ()

      let save d =
        match d.value_ with
          | `Input f ->
            let v = get_opt_value f in
            handle_exception d.error "float" v (
              function
                | Some i -> float_of_string i
                | None -> raise Empty_value
            )
          | _ -> raise Wrong_dom_value

    end)

  module Dom_type_string = Default(struct
      type a = string

      let to_default ?v () =
        let v =
          match v with
            | Some v -> v
            | None -> ""
        in

        let error = p ~a:[ a_class ["error"]; a_style "display:none"] [ ] in
        let d = Raw.textarea (pcdata v) in
        {
          node = div ~a:[ a_class ["dom_ext_string"] ] [ error; d ] ;
          value_ = `Textarea d;
          error = Some error ;
        }

      let to_dom s =
        to_default ~v:s ()

      let save d =
        match d.value_ with
          | `Textarea s ->
            let v = get_opt_value_textarea s in
            handle_exception d.error "string" v (
              function
                | Some i -> i
                | None -> raise Empty_value
            )
          | _ -> raise Wrong_dom_value
    end)


  let display_list (type s) (module A : Dom_type with type a = s) () =
    let nodes = div ~a:[ a_class ["dom_ext_list_elems"]] [] in
    let node = div ~a:[ a_class ["dom_ext_list"]] [ nodes ] in

    let v =
      {
        node ;
        value_ = `List [];
        error = None;
      }
    in

    let add_single_node ?d () =
      let d = match d with
        | Some d -> A.to_dom d
        | None -> A.to_default ()
      in
      v.value_ <-
        begin match v.value_ with
          | `List l -> `List (l @ [ d ])
          | _ -> assert false
        end;

      let single_node = div ~a:[ a_class ["dom_ext_list_elem"]] [ d.node ] in
      let btn =
        button ~a:[ a_onclick (fun _ ->
                      Manip.removeChild nodes single_node;
                      v.value_ <-
                        begin match v.value_ with
                          | `List l -> `List (List.filter (fun d2 -> d <> d2) l)
                          | _ -> assert false
                        end;
                    ); a_class [ "btn"; "btn-warning"];
                    a_button_type `Button
                  ] [ pcdata "delete" ]
      in
      Manip.appendChild single_node btn ;
      Manip.appendChild nodes single_node ;
    in

    let add_btn = button ~a:[ a_onclick (fun _ -> add_single_node ()); a_class [ "btn"; "btn-info" ]; a_button_type `Button ] [ pcdata "add" ] in
    Manip.appendChild node add_btn ;

    v,add_single_node



  module Dom_type_list (A : Dom_type) = Default(struct
      type a = A.a list

      let to_default ?v () =
        let v,add_single_node = display_list (module A) () in
        add_single_node () ;
        v

      let to_dom l =
        let v,add_single_node = display_list (module A) () in
        List.iter (
          fun el ->
            add_single_node ~d:el ()
        ) l;

        v

      let save d =
        match d.value_ with
          | `List l ->
            List.fold_left (
              fun acc e ->
                try (A.save e::acc)
                with Empty_value ->
                  acc
            ) [] (List.rev l)
          | _ -> raise Wrong_dom_value
    end)


  module Dom_type_array (A : Dom_type) = Default(struct
      type a = A.a array

      let to_default ?v () =
        let v,add_single_node = display_list (module A) () in
        add_single_node () ;
        v

      let to_dom a =
        let v,add_single_node = display_list (module A) () in

        Array.iter (
          fun el ->
            add_single_node ~d:el ()
        ) a;

        v

      let save d =
        match d.value_ with
          | `List l ->
            let l =
              List.fold_left (
                fun acc e ->
                  try (A.save e::acc)
                  with Empty_value ->
                    acc
              ) [] (List.rev l)
            in
            Array.of_list l
          | _ -> raise Wrong_dom_value

    end)


  module Dom_type_option (A : Dom_type) = Default(struct
      type a = A.a option

      let to_default ?v () =
        let d = A.to_default () in
        let node = div ~a:[ a_class ["dom_ext_option"]] [ d.node ] in
        {
          d with
            node;
        }

      let to_dom o =
        match o with
          | Some s ->
            let d = A.to_dom s in
            let node = div ~a:[ a_class ["dom_ext_option"]] [ d.node ] in
            {
              d with
                node;
            }
          | None -> to_default ()

      let save o =
        try
          Some (A.save o)
        with Empty_value ->
          None
    end)

}}
