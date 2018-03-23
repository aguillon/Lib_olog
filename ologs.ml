

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

let default = {name = ""; aspects = []; role = None}

let a_person = {default with name = "a person"; aspects = []}
let a_man = {default with name = "a man"; aspects = [None,Is, a_person]}
let a_father = {default with name = "a father"; aspects = [None,Is, a_man; None,Is, a_person]}

let default = {name = ""; aspects = []; role = None}

let make_aspect ?(app_type = Ordinary) ?(aspect_type = Is) target =
  (app_type, aspect_type, target)

let (=>) name obj = {default with name = name; aspects = [make_aspect obj]}

let is_ x = {default with name = ""; aspects = [make_aspect x]}
let rel s target = {default with name = ""; aspects = [make_aspect target ~aspect_type:(Symbolic s)]}

let (<$>) name object_ = {default with name = name; aspects = object_.aspects}
let (<*>) obj1 obj2 = {default with name = obj1.name; aspects = obj1.aspects @ obj2.aspects}

let add_target obj type_asp aspect target = obj.aspects <- obj.aspects@[type_asp,aspect,target];;
let a_woman = "a woman" <$> is_ a_person;; (*pareil que ligne 20, et c'est plus propre avec ton is_ et tout*)
let a_mother = "a mother" <$> is_ a_woman <*> rel None "has a first child" a_person;;

