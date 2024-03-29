(* -------------------------------------------------------------------------- *)
(* ----------------------- TP2 - IFT-3000 - Hiver 2018 ---------------------- *)
(* -------------------------------------------------------------------------- *)
(* Matricule étudiant: 111124564                                              *)
(* Matricule étudiant: .........                                              *)
(* Matricule étudiant: .........                                              *)
(* -------------------------------------------------------------------------- *)
(* -- PRINCIPALE FICHIER DU TP: FONCTIONS À IMPLANTER ----------------------- *)
(* -------------------------------------------------------------------------- *)

(* Enonces des problemes
   Ils sont representes par un couple contenant:
     - une liste de propositions (les Hi)
     - une proposition (le C)
   Chaque proposition est representee a l'aide du type proposition *)

(* Formes clausales
   Une forme clausale est une liste de clauses;
   implicitement, il y a un et logique entre les clauses.
   Une clause est une liste de litteraux;
   implicitement, il y a un ou logique entre les litteraux.
   Un litteral est une proposition de la forme `Var id' ou `Non (Var id)' *)

#use "parseur.ml";;
#use "resolution.mli";;

(******************************************************************************)
(* Implantation                                                               *)
(******************************************************************************)
module Resolution : RESOLUTION = 
struct
  open List;;
  open TypesUtiles;;
  (* Seul espace où implanter le code du TP2 *)

  exception Non_Implante of string

  type proposition = TypesUtiles.proposition

  type enonce_probleme = TypesUtiles.enonce_probleme

  type clause = TypesUtiles.clause

  type forme_clausale = TypesUtiles.forme_clausale

  let removeDuplicates lst =
    fold_left (fun acc x -> if mem x acc then acc else acc@[x]) [] lst
  ;;

  let removeFromListElements lst elementsToRemove =
    filter (fun x -> for_all (fun x' -> x <> x') elementsToRemove) lst
  ;;

  let rec clauseContainsPAndNotP clause =
    match clause with
    | [] -> false
    | x::r -> mem (Non(x)) r || mem x ((map (fun y -> Non(y))) r) || clauseContainsPAndNotP r
  ;;

  let removeClauseNonVraiDeFormeClausale fc =
    filter (fun x -> not (clauseContainsPAndNotP x)) fc 
  ;;

  let fcContainsFalse (fc) =
    exists (fun x -> x = []) fc
  ;;

  let union liste1 liste2 =
    removeDuplicates (liste1@liste2)
  ;;

  let prod liste1 liste2 =
    removeDuplicates (flatten (map (fun x -> map (fun y -> union x y) liste2 ) liste1))
  ;;

  let paires liste =
    let rec aux lst =
      match lst with
      | [] -> []
      | x::r -> (fold_left (fun acc' x' -> acc' @ [((x, x'), removeFromListElements liste [x; x'])]) [] r) @ (aux r)
    in
      aux liste
  ;;

  let enonce2proposition (propList, conclu) =
    fold_right (fun aProp accConjonction -> Et(aProp, accConjonction)) propList (Non(conclu))
  ;;

  let rec mfc prop =
      match prop with
      | Vrai | Non (Faux) -> []
      | Faux | Non (Vrai) -> [[]]
      | Var(_) | Non(Var(_)) -> [[prop]]
      | Non(Non(x1)) -> mfc x1
      | Non(Et(x1, x2)) -> mfc (Ou(Non(x1), Non(x2)))
      | Non(Ou(x1, x2)) -> mfc (Et(Non(x1), Non(x2)))
      | Non(Imp(x1, x2)) -> mfc (Non(Ou(Non(x1), x2)))
      | Non(Equ(x1, x2)) -> mfc (Equ(Non(x1), x2))
      | Et(x1, x2) -> union (mfc x1) (mfc x2)
      | Ou(x1, x2) -> prod (mfc x1) (mfc x2)
      | Imp(x1, x2) -> mfc (Ou(Non(x1), x2))
      | Equ(x1, x2) -> mfc (Et(Imp(x1, x2), Imp(x2, x1)))
  ;;

  let resolutions clause1 clause2 =
    map (fun (_,l) -> l)
      (
        filter (fun ((x,y), _) -> x = Non(y) || Non(x) = y)
          (
            paires (union clause1 clause2)
          )
      )
  ;;

  let resolutionsDesPairesEnNouvelleFormeClausale pairesList =
    map (fun ((x,y), r) -> union (resolutions x y) r) pairesList 
  ;;
  
  let decisionTrace prop =
    let res = ref [[[]]] in 
      let rec aux fc =
        let fcClauseNonVrai = removeClauseNonVraiDeFormeClausale fc in
          if fcClauseNonVrai = [[]] then
            true (* Sortir si la forme clausale est seulement Faux *)
          else if fcContainsFalse fcClauseNonVrai then
          (
            res := fc::(!res);
            true
          )
          else
            let p = paires fcClauseNonVrai in
              let resoFcList = resolutionsDesPairesEnNouvelleFormeClausale p in
                (* Depth first *)
                (*if (exists (fun x -> fcContainsFalse x || aux x) resoFcList) then*)
                (* Breadth first *)
                if (exists (fun x -> 
                    let falseTrouve = fcContainsFalse x in 
                    (
                      (if falseTrouve && x <> [[]] then res := x::(!res));
                      falseTrouve
                    )
                  ) resoFcList)
                 || (exists (fun x -> aux x) resoFcList) then
                (
                  res := fc::(!res);
                  true
                )
                else
                (
                  false
                )
      in
        if aux (mfc prop) then
          Some !res
        else
          None
  ;;

  let decision prop =
    decisionTrace prop <> None
  ;;

end;;

(* Exploitation des fonctions à implanter (fichier resolution.ml):

# #use "resolution.ml";;
module TypesUtiles :
  sig
    type jeton =
        LexParG
      | LexParD
      | LexVrai
      | LexFaux
      | LexNon
      | LexOu
      | LexEt
      | LexImp
      | LexEqu
      | LexVar of string
      | LexSep
      | LexFDF
    type proposition =
        Vrai
      | Faux
      | Non of proposition
      | Et of proposition * proposition
      | Ou of proposition * proposition
      | Imp of proposition * proposition
      | Equ of proposition * proposition
      | Var of string
    type enonce_probleme = proposition list * proposition
    type clause = proposition list
    type forme_clausale = clause list
  end
module Parseur :
  sig
    type jeton = TypesUtiles.jeton
    type proposition = TypesUtiles.proposition
    type enonce_probleme = TypesUtiles.enonce_probleme
    exception ErrAnalyseLexicale
    exception ErrAnalyseSyntaxique
    val aLex : string -> jeton list
    val aSynt : jeton list -> enonce_probleme
  end
module type RESOLUTION =
  sig
    type proposition = TypesUtiles.proposition
    type enonce_probleme = TypesUtiles.enonce_probleme
    type clause = TypesUtiles.clause
    type forme_clausale = TypesUtiles.forme_clausale
    val union : 'a list -> 'a list -> 'a list
    val prod : 'a list list -> 'a list list -> 'a list list
    val paires : 'a list -> (('a * 'a) * 'a list) list
    val enonce2proposition : enonce_probleme -> proposition
    val mfc : proposition -> forme_clausale
    val resolutions : clause -> clause -> clause list
    val decision : proposition -> bool
    val decisionTrace : proposition -> forme_clausale list option
  end
module Resolution : RESOLUTION

# open TypesUtiles;;
# open Parseur;;
# open Resolution;;

# union [1;2;3;2;4;1] [2;1;5;3;6];;
- : int list = [1; 2; 3; 4; 5; 6]
# union ["a"] ["b"; "a"];;
- : string list = ["a"; "b"]
# union [Non (Var "a"); Var "b"] [Non (Var "b"); Var "a"; Var "b"];;
- : TypesUtiles.proposition list =
[Non (Var "a"); Var "b"; Non (Var "b"); Var "a"]

# prod [[1;2];[3]] [[4];[5;2;3];[1]];;
- : int list list =
[[1; 2; 4]; [1; 2; 5; 3]; [1; 2]; [3; 4]; [3; 5; 2]; [3; 1]]
# prod [[1;2;1]] [[3];[]];;
- : int list list = [[1; 2; 3]; [1; 2]]
# prod [[1;2]] [];;
- : int list list = []
# prod [] [[4;5];[6]];;
- : int list list = []
# prod [[Non (Var "a"); Var "b"]; [Var "a"]] [[Var "b"]; [Var "a"]];;
- : TypesUtiles.proposition list list =
[[Non (Var "a"); Var "b"]; [Non (Var "a"); Var "b"; Var "a"];
 [Var "a"; Var "b"]; [Var "a"]]

# paires [1;2;3;4];;
- : ((int * int) * int list) list =
[((1, 2), [3; 4]); ((1, 3), [2; 4]); ((1, 4), [2; 3]); ((2, 3), [1; 4]);
 ((2, 4), [1; 3]); ((3, 4), [1; 2])]
# paires ["a";"b";"c"];;
- : ((string * string) * string list) list =
[(("a", "b"), ["c"]); (("a", "c"), ["b"]); (("b", "c"), ["a"])]
# paires ["a";"b"];;
- : ((string * string) * string list) list = [(("a", "b"), [])]
# paires ["a"];;
- : ((string * string) * string list) list = []
# paires [];;
- : (('a * 'a) * 'a list) list = []
# paires [[Non (Var "a"); Var "b"]; [Non (Var "b")]; [Var "a"]];;
- : ((TypesUtiles.proposition list * TypesUtiles.proposition list) *
     TypesUtiles.proposition list list)
    list
=
[(([Non (Var "a"); Var "b"], [Non (Var "b")]), [[Var "a"]]);
 (([Non (Var "a"); Var "b"], [Var "a"]), [[Non (Var "b")]]);
 (([Non (Var "b")], [Var "a"]), [[Non (Var "a"); Var "b"]])]

# enonce2proposition ([Imp (Var "a", Var "b"); Non (Var "b")], Non (Var "a"));;
- : Resolution.proposition =
Et (Imp (Var "a", Var "b"), Et (Non (Var "b"), Non (Non (Var "a"))))
# enonce2proposition ([Et (Var "a", Var "b")], Var "a");;
- : Resolution.proposition = Et (Et (Var "a", Var "b"), Non (Var "a"))
# enonce2proposition ([Ou (Var "a", Var "b")], Et (Var "a", Non (Var "b")));;
- : Resolution.proposition =
Et (Ou (Var "a", Var "b"), Non (Et (Var "a", Non (Var "b"))))
# enonce2proposition ([Var "a"], Var "a");;
- : Resolution.proposition = Et (Var "a", Non (Var "a"))

# mfc Vrai;;
- : Resolution.forme_clausale = []
# mfc Faux;;
- : Resolution.forme_clausale = [[]]
# mfc (Var "a");;
- : Resolution.forme_clausale = [[Var "a"]]
# mfc (Non Vrai);;
- : Resolution.forme_clausale = [[]]
# mfc (Non Faux);;
- : Resolution.forme_clausale = []
# mfc (Non (Var "a"));;
- : Resolution.forme_clausale = [[Non (Var "a")]]
# mfc (Non (Non (Var "a")));;
- : Resolution.forme_clausale = [[Var "a"]]
# mfc (Et (Ou (Var "a", Var "b"), Non (Et (Var "a", Non (Var "b")))));;
- : Resolution.forme_clausale =
[[Var "a"; Var "b"]; [Non (Var "a"); Var "b"]]

# let c1 = [Var "p"; Non(Var "q"); Var "r"; Non(Var "s")];;
val c1 : TypesUtiles.proposition list =
  [Var "p"; Non (Var "q"); Var "r"; Non (Var "s")]
# let c2 = [Var "r"; Non(Var "p"); Var "q"; Var "t"];;
val c2 : TypesUtiles.proposition list =
  [Var "r"; Non (Var "p"); Var "q"; Var "t"]
# resolutions c1 c2;;
- : Resolution.clause list =
[[Non (Var "q"); Var "r"; Non (Var "s"); Var "q"; Var "t"];
 [Var "p"; Var "r"; Non (Var "s"); Non (Var "p"); Var "t"]]
# resolutions [Non (Var "a"); Var "b"] [Non (Var "b")];;
- : Resolution.clause list = [[Non (Var "a")]]
# resolutions [Non (Var "a"); Var "b"] [Var "a"];;
- : Resolution.clause list = [[Var "b"]]
# resolutions [Non (Var "b")] [Var "a"];;
- : Resolution.clause list = []
# resolutions [Non (Var "a"); Var "b"] [Non (Var "b"); Var "a"];;
- : Resolution.clause list =
[[Var "b"; Non (Var "b")]; [Non (Var "a"); Var "a"]]
# resolutions [Var "b"] [Non (Var "b")];;
- : Resolution.clause list = [[]]

# decision (enonce2proposition (aSynt (aLex "a => b; non b; non a")));;
- : bool = true
# decision (enonce2proposition (aSynt (aLex "a => b; non b; non b")));;
- : bool = true
# decision (enonce2proposition (aSynt (aLex "a => b; non b; a")));;
- : bool = false
# decision (enonce2proposition (aSynt (aLex "p <=> q; q <=> r; p <=> r")));;
- : bool = true
# decision (enonce2proposition (aSynt (aLex "a => b; a; b")));;
- : bool = true
# decision (Et (Imp (Var "a", Var "b"), Et (Var "a", Non (Var "b"))));;
- : bool = true

# decisionTrace (enonce2proposition (aSynt (aLex "a => b; non b; non a")));;
- : Resolution.forme_clausale list option =
Some
 [[[Non (Var "a"); Var "b"]; [Non (Var "b")]; [Var "a"]];
  [[Non (Var "a")]; [Var "a"]]; [[]]]
# decisionTrace (enonce2proposition (aSynt (aLex "a => b; non b; a")));;
- : Resolution.forme_clausale list option = None
# decisionTrace (enonce2proposition (aSynt (aLex "p <=> q; q <=> r; p <=> r")));;
- : Resolution.forme_clausale list option =
Some
 [[[Non (Var "p"); Var "q"]; [Non (Var "q"); Var "p"];
   [Non (Var "q"); Var "r"]; [Non (Var "r"); Var "q"]; [Var "p"; Var "r"];
   [Non (Var "r"); Non (Var "p")]];
  [[Non (Var "p"); Var "r"]; [Non (Var "q"); Var "p"];
   [Non (Var "r"); Var "q"]; [Var "p"; Var "r"];
   [Non (Var "r"); Non (Var "p")]];
  [[Var "r"]; [Non (Var "q"); Var "p"]; [Non (Var "r"); Var "q"];
   [Non (Var "r"); Non (Var "p")]];
  [[Var "p"; Non (Var "r")]; [Var "r"]; [Non (Var "r"); Non (Var "p")]];
  [[Non (Var "r")]; [Var "r"]]; [[]]]
# decisionTrace (enonce2proposition (aSynt (aLex "a => b; a; b")));;
- : Resolution.forme_clausale list option =
Some
 [[[Non (Var "a"); Var "b"]; [Var "a"]; [Non (Var "b")]];
  [[Var "b"]; [Non (Var "b")]]; [[]]]
*)
