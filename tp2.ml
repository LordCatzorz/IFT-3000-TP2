(* IFT-3000: Travail pratique #2.
   Hiver 2018 *)

#use "resolution.ml";;

module Tp2 :
sig
  type format = Latex | Texte

  val tp2 : string -> bool
  val tp2Fichier : string -> bool

  val tp2Trace : ?format:format -> string -> unit
  val tp2TraceFichier : ?format:format -> string -> unit
end =
struct

  open TypesUtiles
  open Parseur
  open Resolution

  open List

  type format = Latex | Texte

  (* ------------------------------------------------------------------------ *)
  (* Les fonctions qui suivent permettent de transformer une proposition, une *)
  (* clause, et une forme clausale en une chaîne de caractères.               *)
  (* ------------------------------------------------------------------------ *)
  let rec prop2Str p = match p with
  | Vrai -> "vrai"
  | Faux  -> "faux" 
  | Var( x )  -> x
  | Non( p )  -> "(non " ^ (prop2Str p) ^ ")"
  | Et( p, p' )  -> "(" ^ (prop2Str p) ^ " et " ^ (prop2Str p') ^ ")"
  | Ou( p, p' )  -> "(" ^ (prop2Str p) ^ " ou " ^ (prop2Str p') ^ ")"
  | Imp( p, p' )  -> "(" ^ (prop2Str p) ^ " => " ^ (prop2Str p') ^ ")"
  | Equ( p, p' )  -> "(" ^ (prop2Str p) ^ " <=> " ^ (prop2Str p') ^ ")"

  let rec clause2Str l = match l with
  | [] -> "faux"
  | [Var(x)]  -> x
  | [Non(p)]  -> "(non " ^ (prop2Str p) ^ ")"
  | lp  -> "(" ^ (clause2StrAux lp) ^ ")"

  and clause2StrAux l = match l with
  | [] -> ""
  | [p]  -> prop2Str p
  | p::r  -> (prop2Str p) ^ " ou " ^ (clause2StrAux r)

  let rec fclausale2Str l = match l with
  | [] -> ""
  | [c] -> clause2Str c
  | c::r -> (clause2Str c) ^ " et " ^ (fclausale2Str r)

  let rec lprop2Str l = match l with
  | [] -> ""
  | [p]  -> prop2Str p 
  | p::r  -> (prop2Str p) ^ " et " ^ (lprop2Str r)

  (* Version Latex des fonctions -------------------------------------------- *)
  (* ------------------------------------------------------------------------ *)
  let rec prop2Latex p = match p with
  | Vrai  -> "1"
  | Faux  -> "0" 
  | Var( x )  -> x
  | Non( p )  -> "\\neg " ^ (prop2Latex p)
  | Et( p, p' )  -> "(" ^ (prop2Latex p) ^ " \\wedge " ^ (prop2Latex p')  ^ ")"
  | Ou( p, p' )  -> "(" ^ (prop2Latex p) ^ " \\vee " ^ (prop2Latex p')  ^ ")"
  | Imp( p, p' )  -> 
    "(" ^ (prop2Latex p) ^ " \\Rightarrow " ^ (prop2Latex p')  ^ ")"
  | Equ( p, p' )  -> 
    "(" ^ (prop2Latex p) ^ " \\Leftrightarrow " ^ (prop2Latex p')  ^ ")"

  let rec clause2Latex l = match l with
  | [] -> "0"
  | [Var(x)]  -> x
  | [Non(p)]  -> "\\neg " ^ (prop2Latex p)
  | lp  -> "(" ^ (clause2LatexAux lp) ^ ")"

  and clause2LatexAux l = match l with
  | [] -> ""
  | [p]  -> prop2Latex p
  | p::r  -> (prop2Latex p) ^ " \\vee " ^ (clause2LatexAux r)

  let rec fclausale2Latex l = match l with
  | [] -> ""
  | [c]  -> clause2Latex c 
  | c::r  -> (clause2Latex c) ^ " \\wedge " ^ (fclausale2Latex r)

  let rec lprop2Latex l = match l with
  | [] -> ""
  | [p]  -> prop2Latex p 
  | p::r  -> (prop2Latex p) ^ " \\wedge " ^ (lprop2Latex r)

   
  (* ------------------------------------------------------------------------ *)
  (* Fonctions de manipulation de fichiers et de génération de résultats      *)
  (* ------------------------------------------------------------------------ *)
  (* read_lines_file : string -> string list *)
  let read_lines_file file =
    let rec read_lines f l =
      try 
        read_lines f (l @ [input_line f])
      with End_of_file -> l
    in
    if Sys.file_exists file then
      let ic = open_in file in
      let l = read_lines ic [] in
      let _ = close_in ic in
      l
    else
      failwith ("Fichier <" ^ file ^ "> introuvable!")

  (* mix_files : string -> string list -> string -> string -> unit *)
  let mix_files header latex footer dest =
    let l1 = read_lines_file header 
    and l2 = read_lines_file footer 
    and out_chan = open_out dest in
    iter (fun s -> output_string out_chan (s^"\n")) (l1@latex@l2);
    close_out out_chan

  (* Génère trace en format Latex *)
  let genTraceLatex (hyp, conc, lc) =
    let lr = 
      [ "\\begin{array}{rl}" ] @
      [ "{\\bf Hypoth&egrave;}{\\bf ses ({\\it H})}:&" ^ ( lprop2Latex hyp ) ^ 
        "\\\\\\\\" ] @
      [ "{\\bf Conclusion ({\\it C})}:&" ^ ( prop2Latex conc ) ^ "\\\\\\\\" ] @
      [ "{\\bf R&eacute;}{\\bf solution}:& L'objectif \\ est \\ de \\ montrer \\" ^ 
        " que \\ (H \\wedge \\neg C) \\Rightarrow 0\\\\\\\\" ] @
      [ "& " ^ ( lprop2Latex (hyp @ [Non(conc)]) ) ^ "\\\\" ] @ 
      ( map ( fun c -> "& \\Rightarrow \\ " ^ ( fclausale2Latex c ) ^ "\\\\" ) lc ) @ 
      [ "\\end{array}" ]
    in
    let _ = mix_files "header.html" lr "footer.html" "tp2-h18.html" in
    match Sys.os_type with
    | "Win32" -> Sys.command ("start tp2-h18.html")
    | _ -> Sys.command ("xdg-open tp2-h18.html")


  (* Génère trace en format texte *)
  let genTraceText (hyp, conc, lc) =
    [ "Hypotheses (H):\n\t"  ^ (lprop2Str hyp) ^ "\n\n" ] @
    [ "Conclusion (C):\n\t"  ^ (prop2Str conc ) ^ "\n\n" ] @
    [ "Resolution: L'objectif est de montrer que (H et (non C)) => faux\n\n\t" ] @
    [ (lprop2Str (hyp @ [Non(conc)])) ^ "\n" ] @ 
    ( map ( fun c -> "\t=> " ^ (fclausale2Str c) ^ "\n" ) lc )


  (* ------------------------------------------------------------------------ *)
  (* Fonctions principales                                                    *)
  (* ------------------------------------------------------------------------ *)
  let tp2 entree
    = decision (enonce2proposition (aSynt (aLex entree)))

  let tp2Fichier nomFichier
    = tp2 (fold_left (^) "" (read_lines_file nomFichier))

  let tp2Trace ?(format = Latex) entree =
    let (hyp,conc) as enonce = aSynt (aLex entree) in
    let prop = enonce2proposition enonce in
    let res = decisionTrace prop in
	  match res, format with
	  | None, _ -> ()
	  | Some lc, Texte -> iter print_string (genTraceText(hyp, conc, lc))
    | Some lc, Latex -> 
      print_endline "Voir affichage dans navigateur.";
      ignore(genTraceLatex(fst enonce, snd enonce, lc))

  let tp2TraceFichier ?(format = Latex) nomFichier 
    = tp2Trace ~format:format (fold_left (^) "" (read_lines_file nomFichier))

end;;

(* Exploitation des fonctions du fichier tp2.ml:

# #use "tp2.ml";;
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
module Tp2 :
  sig
    type format = Latex | Texte
    val tp2 : string -> bool
    val tp2Fichier : string -> bool
    val tp2Trace : ?format:format -> string -> unit
    val tp2TraceFichier : ?format:format -> string -> unit
  end

# open Tp2;;

# tp2 "a => b; non b; non a";;
- : bool = true
# tp2Fichier "test2.txt";;
- : bool = true

# tp2Trace  ~format:Texte "n => p; p => (non f); (non n) => (non f); non f";;
Hypotheses (H):
  (n => p) et (p => (non f)) et ((non n) => (non f))

Conclusion (C):
  (non f)

Resolution: L'objectif est de montrer que (H et (non C)) => faux

  (n => p) et (p => (non f)) et ((non n) => (non f)) et (non (non f))
  => ((non n) ou p) et ((non p) ou (non f)) et (n ou (non f)) et f
  => ((non n) ou (non f)) et (n ou (non f)) et f
  => (non f) et f
  => faux
- : unit = ()
# tp2Trace  "n => p; p => (non f); (non n) => (non f); non f";;
Voir affichage dans navigateur.
- : unit = ()

# tp2TraceFichier "test4.txt";;
Voir affichage dans navigateur.
- : unit = ()

# tp2TraceFichier ~format:Texte "test1.txt";;
Hypotheses (H):
  (a => b) et (non b)

Conclusion (C):
  (non a)

Resolution: L'objectif est de montrer que (H et (non C)) => faux

  (a => b) et (non b) et (non (non a))
  => ((non a) ou b) et (non b) et a
  => (non a) et a
  => faux
- : unit = ()

# tp2TraceFichier ~format:Texte "test2.txt";;
Hypotheses (H):
  (p <=> q) et (q <=> r)

Conclusion (C):
  (p <=> r)

Resolution: L'objectif est de montrer que (H et (non C)) => faux

  (p <=> q) et (q <=> r) et (non (p <=> r))
  => ((non p) ou q) et ((non q) ou p) et ((non q) ou r) et ((non r) ou q) et (p ou r) et ((non r) ou (non p))
  => ((non p) ou r) et ((non q) ou p) et ((non r) ou q) et (p ou r) et ((non r) ou (non p))
  => r et ((non q) ou p) et ((non r) ou q) et ((non r) ou (non p))
  => (p ou (non r)) et r et ((non r) ou (non p))
  => (non r) et r
  => faux
- : unit = ()

# tp2TraceFichier ~format:Texte "test3.txt";;
Hypotheses (H):
  (n => p) et (p => (non f)) et ((non n) => (non f))

Conclusion (C):
  (non f)

Resolution: L'objectif est de montrer que (H et (non C)) => faux

  (n => p) et (p => (non f)) et ((non n) => (non f)) et (non (non f))
  => ((non n) ou p) et ((non p) ou (non f)) et (n ou (non f)) et f
  => ((non n) ou (non f)) et (n ou (non f)) et f
  => (non f) et f
  => faux
- : unit = ()

# tp2TraceFichier ~format:Texte "test4.txt";;
Hypotheses (H):
  (r => x) et ((f et g) => r) et (sup38 => f) et (g et sup38)

Conclusion (C):
  x

Resolution: L'objectif est de montrer que (H et (non C)) => faux

  (r => x) et ((f et g) => r) et (sup38 => f) et (g et sup38) et (non x)
  => ((non r) ou x) et ((non f) ou (non g) ou r) et ((non sup38) ou f) et g et sup38 et (non x)
  => (x ou (non f) ou (non g)) et ((non sup38) ou f) et g et sup38 et (non x)
  => (x ou (non g) ou (non sup38)) et g et sup38 et (non x)
  => (x ou (non sup38)) et sup38 et (non x)
  => x et (non x)
  => faux
- : unit = ()
*)