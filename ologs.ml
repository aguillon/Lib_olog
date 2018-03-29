

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

type object_ = {
  name : string;
  mutable aspects : (application_type * aspect * object_) list;
  role : object_role option
}

type fact = (int list * int list)

type olog = {
  objects : object_ list;
  facts : fact list
}

let default = {name = ""; aspects = []; role = None}

let make_aspect ?(app_type = Ordinary) ?(aspect_type = Is) target =
  (app_type, aspect_type, target)

let (=>) name obj = {default with name = name; aspects = [make_aspect obj]}

let is_ x = {default with name = ""; aspects = [make_aspect x]}
let rel s target = {default with name = ""; aspects = [make_aspect target ~aspect_type:(Symbolic s)]}

let (<$>) name object_ = {default with name = name; aspects = object_.aspects}
let (<*>) obj1 obj2 = {default with name = obj1.name; aspects = obj1.aspects @ obj2.aspects}

let add_target obj type_asp aspect target = obj.aspects <- obj.aspects@[type_asp,aspect,target];;


exception Invalid_path

(* Checks that a path exists in a given object *)
let check_path obj path =
  try
    List.fold_left
      (fun o n ->
        match List.nth o.aspects n with
          | (_, _, obj) -> obj)
      obj
      path
  with Failure _ -> raise Invalid_path

let make_commute obj path1 path2 =
  let obj1 = check_path obj path1 in
  let obj2 = check_path obj path2 in
  if obj1 <> obj2 then
    raise Invalid_path
  else
    (path1, path2)

