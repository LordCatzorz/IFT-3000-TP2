(* IFT-3000: Travail pratique #2.
   Hiver 2018 *)

(* -------------------------------------------------------------------------- *)
(* Module définissant les structures de données utilisées par le parseur.     *)
(* Il comprend aussila définition des principales structures de données du Tp.*)
(* -------------------------------------------------------------------------- *)
module TypesUtiles =
struct
    
  (* Type des jetons *)
  type jeton
    = LexParG           (* ( *)
    | LexParD           (* ) *)
    | LexVrai           (* vrai *)
    | LexFaux           (* faux *)
    | LexNon            (* non *)
    | LexOu             (* ou *)
    | LexEt             (* et *)
    | LexImp            (* -> *)
    | LexEqu            (* <-> *)
    | LexVar of string  (* identificateur: [a-z][a-z0-9_]* *)
    | LexSep            (* ; *)
    | LexFDF            (* fin de fichier *)

  (* Type des propositions logiques *)
  type proposition
    = Vrai
    | Faux
    | Non of proposition
    | Et of proposition * proposition
    | Ou of proposition * proposition
    | Imp of proposition * proposition
    | Equ of proposition * proposition
    | Var of string

  (* Type des enonces de problemes *)
  type enonce_probleme = proposition list * proposition

  (* Type des clauses *)
  type clause = proposition list

  (* Type des formes clausales *)
  type forme_clausale = clause list

end