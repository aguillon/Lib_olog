(* open Ologs *)

module Instance : sig

  type 'a small_instance = {
    object_ : object_;
    obj_content : 'a list
  }

  type 'a infinite_instance = {
    object_ : object_;
    first_values : int -> 'a list
  }

  type 'a object_instance =
    | Small of 'a small_instance
    | Infinite of 'a infinite_instance

  type ('a, 'b) aspect_instance = {
    aspect : full_aspect;
    asp_content : ('a * 'b) list
  }

  type olog_preinstance

  type olog_instance

  val check_instance : olog * olog_preinstance -> olog_instance

end = struct

  type 'a small_instance = {
    object_ : object_;
    obj_content : 'a list
  }

  type 'a infinite_instance = {
    object_ : object_;
    first_values : int -> 'a list
  }

  type 'a object_instance =
    | Small of 'a small_instance
    | Infinite of 'a infinite_instance

  type ('a, 'b) aspect_instance = {
    aspect : full_aspect;
    asp_content : ('a * 'b) list
  }

  (* Using GADTs for existential quantification *)
  type olog_preinstance =
    | SingleObject : 'a object_instance -> olog_preinstance
    | WithAspect : ('a, 'b) aspect_instance * olog_preinstance -> olog_preinstance
    | WithObject : 'a object_instance * olog_preinstance -> olog_preinstance

  type olog_instance = olog_preinstance

  let rec check_fact (path1, path2) object_name =
    assert false

  let check_instance (olog, preinstance) =
    (* TBC *)
    preinstance

end

module Planets = struct

  let a_planet = { default with name = "a planet" }
  let a_moon = "a moon" <$> rel "orbits" a_planet

  let olog = { objects = [a_planet; a_moon] ; facts = [] }

  let x1 = Small { object_ = a_moon ; obj_content = ["The Moon"; "Phobos"] }

  let x2 = Small { object_ = a_planet ; obj_content = ["Earth"; "Mars"] }

  let y1 = { aspect = List.nth a_moon.aspects 0 ; asp_content = [("The Moon", "Earth"); ("Phobos", "Mars")] }

  let test = WithAspect (y1,(WithObject (x2, (SingleObject x1))))

end

module Persons = struct

  let a_number = {default with name = "a number"}
  let a_person = "a person" <$> rel "has an age" a_number
  let a_man = {default with name = "a man"; aspects = [Ordinary,Is, a_person]}
  let a_father = {default with name = "a father"; aspects = [Ordinary,Is, a_man; Ordinary,Is, a_person]}

  let x1 =
    let rec k_first k =
      if k = 0 then [0]
      else k :: k_first (k-1)
    in let k_first k = List.rev (k_first k) in
    Infinite { object_ = a_number ; first_values = k_first }


  let x2 = Small { object_ = a_person ; obj_content = ["Pierre"; "Lucie"] }
  let x3 = Small { object_ = a_man ; obj_content = ["Pierre"] }

  let y1 = {
    aspect = List.nth a_person.aspects 0;
    asp_content = [("Pierre", 14); ("Lucie", 12)]
  }

  let test = WithAspect (y1, (WithObject (x3, (WithObject (x2, SingleObject x1)))))

end



