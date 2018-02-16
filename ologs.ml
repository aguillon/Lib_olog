

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

let is_ x = {default with name = ""; aspects = [(Is, x)]}
let rel s target = {default with name = ""; aspects = [(Symbolic s, target)]}

let (<$>) name object_ = {default with name = name; aspects = object_.aspects}
let (<*>) obj1 obj2 = {default with name = obj1.name; aspects = obj1.aspects @ obj2.aspects}

let a_mother = "a mother" <$> is_ a_woman <*> rel "has a first child" a_person

