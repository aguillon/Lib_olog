

(* Base definitions for ologs *)

type aspect =
  | Is
  | Symbolic of string

type application_type =
  | Ordinary
  | Injective
  | Surjective

type object_role =
  | Empty
  | Singleton
  | Product
  | Coproducts
(* | Pullbacks | Pushouts  To be completed *)

type full_aspect = application_type * aspect * object_

and object_ = {
  name : string;
  mutable aspects : full_aspect list;
  role : object_role option
}

type diagram = (int list * int list)

let default = {name = ""; aspects = []; role = None}

let make_aspect ?(app_type = Ordinary) ?(aspect_type = Is) target =
  (app_type, aspect_type, target)

let (=>) name obj = {default with name = name; aspects = [make_aspect obj]}

let is_ x = {default with name = ""; aspects = [make_aspect x]}
let rel s target = {default with name = ""; aspects = [make_aspect target ~aspect_type:(Symbolic s)]}

let (<$>) name object_ = {default with name = name; aspects = object_.aspects}
let (<*>) obj1 obj2 = {default with name = obj1.name; aspects = obj1.aspects @ obj2.aspects}

let add_target obj type_asp aspect target = obj.aspects <- obj.aspects@[type_asp,aspect,target];;

type olog = {
  objects : object_ list;
  facts : (object_ * diagram) list
}

exception Invalid_path

(* Checks that a path exists in a given object *)
let check_path obj path =
  try
    List.fold_left
      (fun o n ->
        print_endline (string_of_int n);
        match List.nth o.aspects n with
          | (_, _, obj) -> obj)
      obj
      path
  with Failure _ -> raise Invalid_path

let make_diagram obj path1 path2 =
  let obj1 = check_path obj path1 in
  let obj2 = check_path obj path2 in
  if obj1 != obj2 then
    raise Invalid_path
  else
    (obj, (path1, path2))

let make_olog objects facts =
  let diagrams =
    List.map
      (fun (o, (p1, p2)) ->
        assert (List.mem o objects);
        make_diagram o p1 p2)
      facts
  in {objects = objects; facts = diagrams}

