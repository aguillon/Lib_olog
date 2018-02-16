

type aspect =
  | Is
  | Symbolic of string

type object_type = Product (* To be completed *)

type object_ = {name : string; mutable aspects : (aspect * object_) list; role : object_type option}

let default = {name = ""; aspects = []; role = None}

let a_person = {default with name = "a person"; aspects = []}
let a_man = {default with name = "a man"; aspects = [Is, a_person]}
let a_father = {default with name = "a father"; aspects = [Is, a_man; Is, a_person]}

let (=>) name obj = {default with name = name; aspects = [Is, obj]}

let a_woman = "a woman" => a_person
let a_mother = "a mother" => a_woman


