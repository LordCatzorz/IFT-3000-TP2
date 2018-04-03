(* -------------------------------------------------------------------------- *)
(* ----------------------- TP2 - IFT-3000 - Hiver 2018 ---------------------- *)
(* -------------------------------------------------------------------------- *)

(******************************************************************************)
(* Spécification                                                              *)
(******************************************************************************)
module type RESOLUTION =
sig
  (* Principales structures de données du Tp *)
  type proposition = TypesUtiles.proposition
  type enonce_probleme = TypesUtiles.enonce_probleme
  type clause = TypesUtiles.clause
  type forme_clausale = TypesUtiles.forme_clausale

  (* Signatures des fonctions su Tp à implanter *)
  (*(7 pts)*)  val union : 'a list -> 'a list -> 'a list
  (*(10 pts)*) val prod : 'a list list -> 'a list list -> 'a list list
  (*(15 pts)*) val paires : 'a list -> (('a * 'a) * 'a list) list

  (*(8 pts)*)  val enonce2proposition : enonce_probleme -> proposition
  (*(10 pts)*) val mfc : proposition -> forme_clausale
  (*(20 pts)*) val resolutions : clause -> clause -> clause list
  (*(20 pts)*) val decision : proposition -> bool
  (*(10 pts)*) val decisionTrace : proposition -> forme_clausale list option
end