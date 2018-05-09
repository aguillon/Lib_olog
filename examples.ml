
let a_number = {default with name = "a number"; aspects = []}

let a_person = "a person" <$> rel "has an age" a_number

let a_man = {default with name = "a man"; aspects = [Ordinary,Is, a_person]}

let a_father = {default with name = "a father"; aspects = [Ordinary,Is, a_man; Ordinary,Is, a_person]}

let a_woman = "a woman" <$> is_ a_person

let a_mother = "a mother" <$> is_ a_woman <*> rel "has a first child" a_person

module Parents = struct

  (* This should be a product *)
  let parents =
    "parents"
    <$> rel "a mother" a_mother
    <*> rel "a father" a_father

  (* Can't create recursive values in OCaml *)
  let () =
    add_target a_person Ordinary (Symbolic "has parents") parents

  let olog =
    let objects = [a_number; a_person; a_man; a_father; a_woman;
      a_mother; parents]
    in
    let diagram = ([1; 0; 1], []) in (* The first child of a person's parents is the person itself… stupid fact :( *)
    make_olog objects [(a_person, diagram)]

end
