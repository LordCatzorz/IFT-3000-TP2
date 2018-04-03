(* IFT-3000: Travail pratique #2.
   Hiver 2018 *)

(* Syntaxe de l'entree:
       H1 ; H2 ; ... ; Hn ; C
   ou les Hi sont les hypotheses
   et ou C est la conclusion qui est potentiellement la consequence des Hi.
   On peut fournir aucune hypothese, auquel cas il y a seulement C en entree.
   Les Hi et C sont de propositions (provenant du non-terminal P).
   Voici la grammaire des propositions:
       P ::= A "<->" P | A
       A ::= B "->" A | B
       B ::= C "ou" B | C
       C ::= D "et" C | D
       D ::= "non" E | E
       E ::= "(" P ")" | "vrai" | "faux" | I
       I ::= idenficateur commencant par une lettre et se poursuivant
             avec 0 ou plus lettres, chiffres et "_"
   Des commentaires sont permis partout dans l'entree pourvu qu'ils
   ne coupent pas un lexeme en deux.  La syntaxe des commentaires est
   la meme qu'en ML:
       (* n'importe quoi sans etoile-parenthese-fermante *)
   sauf qu'on ne peut pas les imbriquer.
   L'analyseur lexical ne distingue pas les majuscules des minuscules *)

(* Enonces des problemes
   Ils sont representes par un couple contenant:
     - une liste de propositions (les Hi)
     - une proposition (le C)
   Chaque proposition est representee a l'aide du type proposition *)

#use "typesutiles.ml";;

(* -------------------------------------------------------------------------- *)
(* Module comprenant la définition du parseur                                 *)
(* -------------------------------------------------------------------------- *)
module Parseur :
sig
    type jeton = TypesUtiles.jeton
    type proposition = TypesUtiles.proposition
    type enonce_probleme = TypesUtiles.enonce_probleme

    exception ErrAnalyseLexicale
    exception ErrAnalyseSyntaxique

    val aLex : string -> jeton list
    val aSynt : jeton list -> enonce_probleme
end =
struct

  include TypesUtiles
  open List

  (* ------------------------------------------------------------------------ *)
  (* Fonctions utiles.                                                        *)
  (* ------------------------------------------------------------------------ *)
  (* explode; convert to list of char *)
  let explode s =
    let s = ref s in
    let r = ref [] in
    let _ =
      while(!s <> "") do
        r:=(String.get !s 0)::!r;
        s:=String.sub !s 1 (String.length !s - 1)
      done
    in
    List.rev !r;;

  (* implode; convert list of char to string *)
  let implode s =
    let s = Bytes.init (List.length s) (List.nth s) in
    Bytes.unsafe_to_string s;;

  let isUpper = (fun c -> let n = Char.code c in n >= 65 && n <= 90);;
  let isLower = (fun c -> let n = Char.code c in n >= 97 && n <= 122);;
  let isDigit = (fun c -> let n = Char.code c in n >= 48 && n <= 57);;
  let isAlpha c = (isUpper c) || (isLower c);;
  let isAlphaNum c = (isAlpha c) || (isDigit c);;
  let isSpace c = (c = ' ') || (c = '\t') || (c = '\n') || 
                  (c = '\011') || (c = '\012') || (c = '\r');;


  (* ------------------------------------------------------------------------ *)
  (* Analyseur lexicale.                                                      *)
  (* ------------------------------------------------------------------------ *)
  exception ErrAnalyseLexicale

  let rec aLex (s : string) : jeton list
    = aLexAux (map Char.lowercase (explode s))

  and aLexAux l = match l with
    | [] -> [LexFDF]
    | c::cs ->
      begin
        match c with
        | '(' -> aLexAuxParen l
        | ')' -> LexParD :: aLexAux cs
        | '=' -> aLexAuxImp l
        | '<' -> aLexAuxEqu l
        | ';' -> LexSep :: aLexAux cs
        | _    -> 
          if isAlpha c
          then aLexAuxId [] l
          else if isSpace c
               then aLexAux cs
               else raise ErrAnalyseLexicale
        
      end

  and aLexAuxParen l = match l with
    | '('::'*'::cs -> aLexAuxComm cs
    | '('::cs ->  LexParG :: aLexAux cs
    | _ -> raise ErrAnalyseLexicale

  and aLexAuxComm l = match l with
    | '*'::')'::cs -> aLexAux cs
    | _::cs ->   aLexAuxComm cs
    | [] -> raise ErrAnalyseLexicale

  and aLexAuxImp l = match l with
    | '='::'>'::cs -> LexImp :: aLexAux cs
    | _ -> raise ErrAnalyseLexicale

  and aLexAuxEqu l = match l with
    | '<'::'='::'>'::cs -> LexEqu :: aLexAux cs
    | _ -> raise ErrAnalyseLexicale

  and aLexAuxId revcs cs =
    if cs = [] || (not (aLexIdChar (hd cs)))
    then 
      let id = implode (rev revcs) in
       (aLexMotCle id) :: (aLexAux cs)
    else aLexAuxId ((hd cs) :: revcs) (tl cs)

  and aLexIdChar c = 
    (isAlpha c) || (isDigit c) || (c = '_')

  and aLexMotCle s = match s with
    | "vrai" -> LexVrai
    | "faux" -> LexFaux
    | "non"  -> LexNon
    | "ou"   -> LexOu
    | "et"   -> LexEt
    | id     -> LexVar id


  (* ------------------------------------------------------------------------ *)
  (* Analyseur syntaxique.                                                    *)
  (* ------------------------------------------------------------------------ *)
  exception ErrAnalyseSyntaxique

  let rec aSynt (js : jeton list) : enonce_probleme =
    let (enonce, rst) = aSyntEnonce js in
    match rst with
      | [LexFDF] -> enonce
      | _        -> raise ErrAnalyseSyntaxique

  and aSyntEnonce js =                               (* traitement des LexSep *)
    let (p, rst1) = aSyntProp1 js in
    match rst1 with
      | (LexSep::rst2) -> 
        let ((ps, c), rst3) = aSyntEnonce rst2 in
          ((p::ps, c), rst3)
      | (LexFDF::_)    -> (([], p), rst1)
      | _              -> raise ErrAnalyseSyntaxique

  and aSyntProp1 js =                                (* traitement des LexEqu *)
    let (p1, rst1) = aSyntProp2 js in
    match rst1 with
      | (LexEqu::rst2) -> 
        let (p2, rst3) = aSyntProp1 rst2 in
          (Equ (p1, p2), rst3)
      | _              -> (p1, rst1)

  and aSyntProp2 js =                                (* traitement des LexImp *)
    let (p1, rst1) = aSyntProp3 js in
    match rst1 with
      | (LexImp::rst2) -> 
        let (p2, rst3) = aSyntProp2 rst2 in
          (Imp (p1, p2), rst3)
      | _              -> (p1, rst1)

  and aSyntProp3 js =                                 (* traitement des LexOu *)
    let (p1, rst1) = aSyntProp4 js in
    match rst1 with
      | (LexOu::rst2) -> 
        let (p2, rst3) = aSyntProp3 rst2 in
          (Ou (p1, p2), rst3)
      | _             -> (p1, rst1)

  and aSyntProp4 js =                                 (* traitement des LexEt *)
    let (p1, rst1) = aSyntProp5 js in
    match rst1 with
      | (LexEt::rst2) -> 
        let (p2, rst3) = aSyntProp4 rst2 in
          (Et (p1, p2), rst3)
      | _             -> (p1, rst1)

  and aSyntProp5 js =                                 (* traitement des LexNon *)
    match js with
      | (LexNon::rst1) -> 
        let (p, rst2) = aSyntProp6 rst1 in
        (Non p, rst2)
      | _              -> aSyntProp6 js

  and aSyntProp6 js =                (* traitement des propositions atomiques *)
    match js with
      | (LexParG::rst1)  ->                                (* (proposition) *)
        let (p, rst2) = aSyntProp1 rst1 in
        begin
          match rst2 with
            | (LexParD::rst3) -> (p, rst3)
            | _               -> raise ErrAnalyseSyntaxique
        end
      | (LexVrai::rst1)     -> (Vrai, rst1)
      | (LexFaux::rst1)     -> (Faux, rst1)
      | (LexVar id :: rst1) -> (Var id, rst1)
      | _                   -> raise ErrAnalyseSyntaxique

end;;

(* Exploitation des fonctions du fichier parseur.ml:
  
#use "parseur.ml";;

# open TypesUtiles;;
# open Parseur;;

# let lj = aLex "n => p; p => (non f); (non n) => (non f); non f";;
val lj : Parseur.jeton list =
  [LexVar "n"; LexImp; LexVar "p"; LexSep; LexVar "p"; LexImp; LexParG;
   LexNon; LexVar "f"; LexParD; LexSep; LexParG; LexNon; LexVar "n"; LexParD;
   LexImp; LexParG; LexNon; LexVar "f"; LexParD; LexSep; LexNon; LexVar "f";
   LexFDF]

# aSynt lj;;
- : Parseur.enonce_probleme =
([Imp (Var "n", Var "p"); Imp (Var "p", Non (Var "f"));
  Imp (Non (Var "n"), Non (Var "f"))],
 Non (Var "f"))

# aSynt (aLex "n => p; p => (non f); (non n) => (non f); non f");;
- : Parseur.enonce_probleme =
([Imp (Var "n", Var "p"); Imp (Var "p", Non (Var "f"));
  Imp (Non (Var "n"), Non (Var "f"))],
 Non (Var "f"))
*)