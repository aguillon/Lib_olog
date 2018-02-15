(*
	Bonne pratique : 

	La flèche provenant d'un olog (un 'aspect') et allant vers un autr e olog
	
	1/
		doit commencer par un verbe.
	2/
		doit avoir sa description est en français.
	3/
		Doit décrire une relation fonctionnelle entre le domaine source et le domaine visé


	Un olog contient un texte qui le décrit clairement
	Un olog qui contient au moins 2 chemins équivalent, est commutatif


	Un 'fait'
		C'est la déclaration de plusieurs chemins (graphe) équivalent
		2 chemins équivalent doivent avoir 
		le meme domaine de définition et le meme domaine d'arrivé 

	2 regles d'une relation (fleche) fonctionnelle

	1/
		Chaque flèche doit venir d'un domaine et aller vers un et un seul domaine
	2/
		chaque élement d'un domaine doit avoir une flèche émanant de lui.


*)

(*===========================(*CODE*)==============================*)


(* non concluant

class olog (name_init) =
object

	val mutable name = name_init
	val relations = List.tl [Is,olog]

	method get_nom = name
	method append_rel (arrow,target) = relations :: [[arrow,target]]
	(*method is ()
	method has ()
	method use ()*)
end;;


let olog = new olog("A Person");;
olog#get_nom;;*)




type aspect = 
	Empty
	| Is
	| Has
	| Use;;

let string_of_type e = match e with
	| Empty -> " "
	| Is -> " is "
	| Has -> " has "
	| Use -> " use " 


(*Graph*)

type 'a graph = 
	| Graph of 'a * 'a graph list;;


Graph ("a person",[]);;

let string_graph = Graph ("a man",[Graph("a person",[]);Graph("a set of childs",[])]);;



(*Olog 0.1*)
type ontology_log = 
	| Olog of string * aspect * ontology_log list;;


Olog("a father",Is,[Olog("a Person",Empty,[]);Olog("a man",Is,[Olog("a person",Empty,[])])]);; (*olog communatif/fact*)

(*Pas vraiment satisfaisant, ça risque d'etre chiant pour les parcours, et syntaxiquement aussi si ont fait des gros olog ça devient illisible*)



(*olog 0.2*)
module SS = Set.Make(String)

type olog = {name:string;domain:SS.t;relation:aspect;targets:olog list};;

let student = SS.singleton "";;
let student = List.fold_right SS.add ["Hakim";"David";"Bassem";"Louis";"Eve";"Adam";"Devola";"Popola"] student;;

let person = SS.singleton "";;
let person = List.fold_right SS.add ["Akira";"Miyazaki";"Kaneda"] person;; 

let person = SS.union student person;;

let olog_person = {name = "a person";domain = person;relation = Empty;targets = []};;

let olog_student = {name = "a student";domain = student;relation = Is;targets = [olog_person]};;

let relation source target = if(SS.is_empty(SS.inter source.domain target.domain)) 
						  then source.name ^ string_of_type(source.relation) ^ " not " ^ target.name 
						  else source.name ^ string_of_type(source.relation) ^ target.name;;

print_string(relation olog_student olog_person^"\n");; (*true*)

