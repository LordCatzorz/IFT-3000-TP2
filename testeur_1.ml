(* --------------------------------------------------------------------------- *)
(* ----------------------- TP2 - IFT-3000 - Hiver 2018 ----------------------- *)
(* --------------------------------------------------------------------------- *)
(* Fichier pertmettant de tester les fonctions implantées du TP                *)
(* --------------------------------------------------------------------------- *)
(* On suppose que le Tp (resolution.ml) est chargé dans la mémoire de          *)
(* l'interpréteur; il suffit alors d'entrer dans l'interpréteur ce qui suit:   *)
(*                                                                             *)
(* # #use "testeur.ml";;                                                       *)
(*                                                                             *)
(* Par la suite:                                                               *)
(*                                                                             *)
(* # testeur();;  (* Teste toutes les fonctions                *)              *)
(* # testn();;    (* n = 1 ou 2 ...; teste la fonction numéro n *)             *)
(*                                                                             *)
(* Lorsque le fichier resolution.ml est modifié, vous n'avez juste qu'à:       *)
(* - recharger le fichier resolution.ml;                                       *)
(* - recharger le fichier testeur.ml.                                          *)
(* Par la suite, vous pouvez de nouveau effectuer les tests                    *)
(* --------------------------------------------------------------------------- *)

open Resolution;; 
open TypesUtiles;;


(* Utiles *)
(* --------------------------------------------------------------------------- *)
open List;;

let f eq a b =
  length a = length b 
  &&
  for_all (fun e -> let c = filter (eq e) b in length c = 1) a;;

let g a b =
  length a = length b 
  &&
  for_all (fun ((e11,e12) as p,e2) -> 
             let c = filter (fun ((e11',e12') as p',e2') -> 
                               (p = p' || p = (e12',e11'))
                               &&
                               f (=) e2 e2'
                            ) b in 
               length c = 1
          ) a;;

let h a b = match a,b with
  | None,None -> true
  | Some a, Some b ->
    for_all2 (f (fun x y -> f (=) x y)) a b
  | _ -> false


(* Testn *)
(* ------------------------------------------------------------------------ *)

(* -- À IMPLANTER/COMPLÉTER (7 PTS) --------------------------------------- *)
(* @Fonction : union : ’a list -> ’a list -> ’a list                        *)
let test1() =
  let comment_l = ref [] in

  let jeu_donnees1 =
  (*   param  ,res, commentaires_test *)
    [ 
      (([],[]), [], "cas 1");
      (([1],[]), [1], "cas 2");
      (([],[1]), [1], "cas 3");
      (([],[1;2]), [1;2], "cas 4");
      (([1;3],[]), [1;3], "cas 5");
      (([1],[2]), [1;2], "cas 6");
      (([1],[1]), [1], "cas 7");
      (([1;2;3],[4;5]), [1;2;3;4;5], 
        "cas 8");
      (([1;2;3],[4;1;2;5]), [1;2;3;4;5], 
        "cas 9");
      (([1;4;2;3;5;0],[4;1;2;5]), [0;1;2;3;4;5], 
        "cas 10");
      (([2;1;3;1;2;3;2],[4;5;4;5]), [1;2;3;4;5], 
        "cas 11");
      (([4;2;1;3;4;1;2;3;2],[1;4;2;5;1;4;5]), [1;2;3;4;5], 
        "cas 12")
    ] in

  let jeu_donnees2 =
  (*   param  ,res, commentaires_test *)
    [ 
      (([Non (Var "a"); Var "b"],[Non (Var "b"); Var "a"; Var "b"]), 
        [Non (Var "a"); Var "b"; Non (Var "b"); Var "a"], 
        "cas 13 (polymorphisme)")
    ] in

  try
    List.iter 
      ( fun ((p1,p2), res, comment) ->
          if f (=) (union p1 p2) res  
          then () 
          else comment_l := !comment_l @ [comment ^ " --> incorrect!"]
      ) jeu_donnees1;
    List.iter 
      ( fun ((p1,p2), res, comment) ->
          if f (=) (union p1 p2) res  
          then () 
          else comment_l := !comment_l @ [comment ^ " --> incorrect!"]
      ) jeu_donnees2;

      (!comment_l, false)
  with
  | e -> 
    (!comment_l @ ["Exception soulevée: " ^ Printexc.to_string e], true);;



(* -- À IMPLANTER/COMPLÉTER (10 PTS) --------------------------------------- *)
(* @Fonction : prod : ’a list list -> ’a list list -> ’a list list           *)
let test2() =
  let comment_l = ref [] in

  let jeu_donnees1 =
  (*   param  ,res, commentaires_test *)
    [ (([], []), [], "cas 1");
      (([[]], []), [], "cas 2");
      (([], [[]]), [], "cas 3");
      (([[1]], []), [], "cas 4");
      (([], [[1]]), [], "cas 5");
      (([[]], [[]]), [[]], "cas 6");
      (([[1]], [[]]), [[1]], "cas 7");
      (([[]], [[1]]), [[1]], "cas 8");
      (([[];[1]], [[]]), [[];[1]], "cas 9");
      (([[]], [[];[1]]), [[1];[]], "cas 10");
      (([[1;1]], [[]]), [[1]], "cas 11");
      (([[1;2]], [[3]]), [[1; 2; 3]], "cas 12");
      (([[1;2]], [[3;4]]), [[1; 2; 3; 4]], "cas 13");
      (([[1;2];[3]], [[5]]), [[1; 2; 5]; [3; 5]], "cas 14");
      (([[1;2];[3;4]], [[5]]), [[1; 2; 5]; [3; 4; 5]], "cas 15");
      (([[1;2];[3;4]], [[5];[6;7]]), 
        [[1; 2; 5]; [1; 2; 6; 7]; [3; 4; 5]; [3; 4; 6; 7]], "cas 16")
    ] in
  let jeu_donnees2 =
  (*   param  ,res, commentaires_test *)
    [ (([[Non (Var "a"); Var "b"]; [Var "a"];[Var "c"]],[[Var "b"]; [Var "f"; Var "g"]; []]), 
        [[Non (Var "a"); Var "b"]; [Non (Var "a"); Var "b"; Var "f"; Var "g"];
         [Var "a"; Var "b"]; [Var "a"; Var "f"; Var "g"]; [Var "a"];
         [Var "c"; Var "b"]; [Var "c"; Var "f"; Var "g"]; [Var "c"]], 
        "cas 17 (polymorphisme)")
    ] in

  try
    List.iter 
      ( fun ((p1,p2), res, comment) ->
          if f (fun x y -> f (=) x y) (prod p1 p2) res  
          then () 
          else comment_l := !comment_l @ [comment ^ " --> incorrect!"]
      ) jeu_donnees1;
    List.iter 
      ( fun ((p1,p2), res, comment) ->
          if f (fun x y -> f (=) x y) (prod p1 p2) res  
          then () 
          else comment_l := !comment_l @ [comment ^ " --> incorrect!"]
      ) jeu_donnees2;
      (!comment_l, false)
  with
  | e -> 
    (!comment_l @ ["Exception soulevée: " ^ Printexc.to_string e], true);;


(* -- À IMPLANTER/COMPLÉTER (15 PTS) -------------------------------------- *)
(* @Fonction : paires : ’a list -> ((’a * ’a) * ’a list) list               *)
let test3() =
  let comment_l = ref [] in

  let jeu_donnees1 =
  (*   param  ,res, commentaires_test *)
    [ ([], [], "cas 1");
      ([1], [], "cas 2");
      ([1;2], [((1, 2), [])], "cas 3");
      ([1;2;3], [((1, 2), [3]); ((1, 3), [2]); ((2, 3), [1])], "cas 4");
      ([1;2;3;4], 
        [((1, 2), [3; 4]); ((1, 3), [2; 4]); ((1, 4), [2; 3]); ((2, 3), [1; 4]);
         ((2, 4), [1; 3]); ((3, 4), [1; 2])], "cas 5")
    ] in
  let jeu_donnees2 =
  (*   param  ,res, commentaires_test *)
    [ ([[Non (Var "a"); Var "b"]; [Non (Var "b")]; [Var "a"]; [Var "c"];[Var "d"]], 
        [(([Non (Var "a"); Var "b"], [Non (Var "b")]),
          [[Var "a"]; [Var "c"]; [Var "d"]]);
         (([Non (Var "a"); Var "b"], [Var "a"]),
          [[Non (Var "b")]; [Var "c"]; [Var "d"]]);
         (([Non (Var "a"); Var "b"], [Var "c"]),
          [[Non (Var "b")]; [Var "a"]; [Var "d"]]);
         (([Non (Var "a"); Var "b"], [Var "d"]),
          [[Non (Var "b")]; [Var "a"]; [Var "c"]]);
         (([Non (Var "b")], [Var "a"]),
          [[Non (Var "a"); Var "b"]; [Var "c"]; [Var "d"]]);
         (([Non (Var "b")], [Var "c"]),
          [[Non (Var "a"); Var "b"]; [Var "a"]; [Var "d"]]);
         (([Non (Var "b")], [Var "d"]),
          [[Non (Var "a"); Var "b"]; [Var "a"]; [Var "c"]]);
         (([Var "a"], [Var "c"]),
          [[Non (Var "a"); Var "b"]; [Non (Var "b")]; [Var "d"]]);
         (([Var "a"], [Var "d"]),
          [[Non (Var "a"); Var "b"]; [Non (Var "b")]; [Var "c"]]);
         (([Var "c"], [Var "d"]),
          [[Non (Var "a"); Var "b"]; [Non (Var "b")]; [Var "a"]])], 
        "cas 6 (polymorphisme)")
    ] in

  try
    List.iter 
      ( fun (p, res, comment) ->
          if g (paires p) res  
          then ()
          else comment_l := !comment_l @ [comment ^ " --> incorrect!"]
      ) jeu_donnees1;
    List.iter 
      ( fun (p, res, comment) ->
          if g (paires p) res  
          then () 
          else comment_l := !comment_l @ [comment ^ " --> incorrect!"]
      ) jeu_donnees2;
      (!comment_l, false)
  with
  | e -> 
    (!comment_l @ ["Exception soulevée: " ^ Printexc.to_string e], true);;


(* -- À IMPLANTER/COMPLÉTER (8 PTS) --------------------------------------- *)
(* @Fonction : enonce2proposition : enonce_probleme -> proposition          *)
let test4() =
  let comment_l = ref [] in

  let jeu_donnees =
  (*   param  ,res, commentaires_test *)
    [ (([], Vrai), Non Vrai, "cas 1");
      (([Vrai], Vrai), Et (Vrai, Non Vrai), "cas 2");
      (([Vrai;Faux], Vrai), Et (Vrai, Et (Faux, Non Vrai)), "cas 3");
      (([Vrai;Faux;Var "a"], Non Vrai), 
        Et (Vrai, Et (Faux, Et (Var "a", Non (Non Vrai)))), "cas 4")
    ] in

  try
    List.iter 
      ( fun (p, res, comment) ->
          if enonce2proposition p = res  
          then ()
          else comment_l := !comment_l @ [comment ^ " --> incorrect!"]
      ) jeu_donnees;
      (!comment_l, false)
  with
  | e -> 
    (!comment_l @ ["Exception soulevée: " ^ Printexc.to_string e], true);;


(* -- À IMPLANTER/COMPLÉTER (10 PTS) -------------------------------------- *)
(* @Fonction : mfc : proposition -> forme_clausale                          *)
let test5() =
  let comment_l = ref [] in

  let jeu_donnees =
  (*   param  ,res, commentaires_test *)
    [ (Vrai, [], "cas 1");
      (Faux, [[]], "cas 2");
      (Var "a", [[Var "a"]], "cas 3");
      (Non Vrai, [[]], "cas 4");
      (Non Faux, [], "cas 5");
      (Non (Var "a"), [[Non (Var "a")]], "cas 6");
      (Non (Non (Var "a")), [[Var "a"]], "cas 7");
      (Et(Vrai,Vrai), [], "cas 8");
      (Et(Vrai,Faux), [[]], "cas 9");
      (Ou(Vrai,Vrai), [], "cas 10");
      (Ou(Vrai,Faux), [], "cas 11");
      (Ou(Faux,Faux), [[]], "cas 12");
      (Imp(Vrai,Vrai), [], "cas 13");
      (Imp(Vrai,Faux), [[]], "cas 14");
      (Imp(Faux,Vrai), [], "cas 15");
      (Equ(Non Faux,Non Vrai), [[]], "cas 16");
      (Equ(Vrai,Non Faux), [], "cas 17");
      (Non(Equ(Non Faux,Non Vrai)), [], "cas 18");
      (Non(Et(Vrai,Faux)), [], "cas 19");
      (Non(Et(Var "a",Var "b")), [[Non (Var "a"); Non (Var "b")]], "cas 20");
      (Non(Ou(Var "a",Var "b")), [[Non (Var "a")]; [Non (Var "b")]], "cas 21");
      (Non(Imp(Var "a",Var "b")), [[Var "a"]; [Non (Var "b")]], "cas 22");
      (Non(Equ(Var "a",Var "b")), 
        [[Var "a"; Var "b"]; [Non (Var "b"); Non (Var "a")]], "cas 23");
      (Et(Var "a",Var "b"), [[Var "a"]; [Var "b"]], "cas 24");
      (Ou(Var "a",Var "b"), [[Var "a"; Var "b"]], "cas 25");
      (Imp(Var "a",Var "b"), [[Non (Var "a"); Var "b"]], "cas 26");
      (Equ(Var "a",Var "b"), 
        [[Non (Var "a"); Var "b"]; [Non (Var "b"); Var "a"]], "cas 27");
      (Et (Ou (Var "a", Var "b"), Non (Et (Var "a", Non (Var "b")))), 
        [[Var "a"; Var "b"]; [Non (Var "a"); Var "b"]], "cas 28")
    ] in

  try
    List.iter 
      ( fun (p, res, comment) ->
          if f (fun x y -> f (=) x y) (mfc p) res  
          then ()
          else comment_l := !comment_l @ [comment ^ " --> incorrect!"]
      ) jeu_donnees;
      (!comment_l, false)
  with
  | e -> 
    (!comment_l @ ["Exception soulevée: " ^ Printexc.to_string e], true);;


(* -- À IMPLANTER/COMPLÉTER (20 PTS) -------------------------------------- *)
(* @Fonction : resolutions : clause -> clause -> clause list                *)
let test6() =
  let comment_l = ref [] in

  let jeu_donnees =
  (*   param  ,res, commentaires_test *)
    [ (([], []), [], "cas 1");
      (([Var "p"], []), [], "cas 2");
      (([], [Var "p"]), [], "cas 3");
      (([Var "p"], [Var "p"]), [], "cas 4");
      (([Non(Var "p")], [Var "p"]), [[]], "cas 5");
      (([Var "p"], [Non(Var "p")]), [[]], "cas 6");
      (([Var "p"; Var "q"], [Non(Var "p")]), [[Var "q"]], "cas 7");
      (([Var "p"; Var "q"], [Non(Var "p"); Var "q"]), [[Var "q"]], "cas 8");
      (([Var "p"; Var "q"], [Non(Var "p"); Var "r"]), [[Var "q"; Var "r"]], "cas 9");
      (([Var "p"; Non(Var "q")], [Non(Var "p"); Var "q"]), 
        [[Non (Var "q"); Var "q"]; [Var "p"; Non (Var "p")]], "cas 10");
      (([Non (Var "a"); Var "b"], [Non (Var "b")]),[[Non (Var "a")]], "cas 11");
      (([Var "p"; Non (Var "q"); Var "r"; Non (Var "s")], 
        [Var "r"; Non (Var "p"); Var "q"; Var "t"]), 
        [[Non (Var "q"); Var "r"; Non (Var "s"); Var "q"; Var "t"];
         [Var "p"; Var "r"; Non (Var "s"); Non (Var "p"); Var "t"]], "cas 12");
    ] in

  try
    List.iter 
      ( fun ((p1,p2), res, comment) ->
          if f (fun x y -> f (=) x y) (resolutions p1 p2) res  
          then () 
          else comment_l := !comment_l @ [comment ^ " --> incorrect!"]
      ) jeu_donnees;
      (!comment_l, false)
  with
  | e -> 
    (!comment_l @ ["Exception soulevée: " ^ Printexc.to_string e], true);;


(* -- À IMPLANTER/COMPLÉTER (20 PTS) -------------------------------------- *)
(* @Fonction : decision : proposition -> bool                               *)
let test7() =
  let comment_l = ref [] in

  let jeu_donnees =
  (*   param  ,res, commentaires_test *)
    [ (Et (Vrai, Non Vrai), true, "cas 1");
      (Et (Vrai, Non Faux), false, "cas 2");
      (Et (Faux, Non Faux), true, "cas 3");
      (Et (Faux, Non Vrai), true, "cas 4");
      (Et (Var "p", Non (Var "p")), true, "cas 5");
      (Et (Vrai, Non (Var "p")), false, "cas 6");
      (Et (Faux, Non (Var "p")), true, "cas 7");
      (Et (Var "q", Non (Var "p")), false, "cas 8");
      (Et (Non(Var "p"), Non (Var "p")), false, "cas 9");
      (Et (Ou(Var "p", Var "q"), Non (Var "p")), false, "cas 10");
      (Et (Et(Var "p", Var "q"), Non (Var "p")), true, "cas 11");
      (Et (Et(Imp(Var "p",Var "q"), Var "p"), Non (Var "q")), true, "cas 12");
      (Et (Et(Equ(Var "p",Var "q"), Equ(Var "q",Var "r")), Non (Equ(Var "p",Var "r"))), 
        true, "cas 13")
    ] in

  try
    List.iter 
      ( fun (p, res, comment) ->
          if (decision p) = res  
          then ()
          else comment_l := !comment_l @ [comment ^ " --> incorrect!"]
      ) jeu_donnees;
      (!comment_l, false)
  with
  | e -> 
    (!comment_l @ ["Exception soulevée: " ^ Printexc.to_string e], true);;


(* -- À IMPLANTER/COMPLÉTER (10 PTS) -------------------------------------- *)
(* @Fonction : decisionTrace : proposition -> forme_clausale list option    *)
let test8() =
  let comment_l = ref [] in

  let jeu_donnees =
  (*   param  ,res, commentaires_test *)
    [ (Et (Vrai, Non Vrai), Some [[[]]], "cas 1");
      (Et (Vrai, Non Faux), None, "cas 2");
      (Et (Faux, Non Faux), Some [[[]]], "cas 3");
      (Et (Faux, Non Vrai), Some [[[]]], "cas 4");
      (Et (Var "p", Non (Var "p")), 
        Some [[[Var "p"]; [Non (Var "p")]]; [[]]], "cas 5");
      (Et (Vrai, Non (Var "p")), None, "cas 6");
      (Et (Faux, Non (Var "p")), 
        Some [[[]; [Non (Var "p")]]; [[]]], "cas 7");
      (Et (Var "q", Non (Var "p")), None, "cas 8");
      (Et (Non(Var "p"), Non (Var "p")), None, "cas 9");
      (Et (Ou(Var "p", Var "q"), Non (Var "p")), None, "cas 10");
      (Et (Et(Var "p", Var "q"), Non (Var "p")), 
        Some [[[Var "p"]; [Var "q"]; [Non (Var "p")]]; 
              [[]; [Var "q"]]; [[]]], "cas 11")
    ] in

  try
    List.iter 
      ( fun (p, res, comment) ->
          if h (decisionTrace p) res  
          then () 
          else comment_l := !comment_l @ [comment ^ " --> incorrect!"]
      ) jeu_donnees;
      (!comment_l, false)
  with
  | e -> 
    (!comment_l @ ["Exception soulevée: " ^ Printexc.to_string e], true);;


(* Teste tout *)
(* ------------------------------------------------------------------------ *)
let test() =
  let all_tests = [ "union", 7, test1;
                    "prod", 10, test2;
                    "paires", 15, test3;
                    "enonce2proposition", 8, test4;
                    "mfc", 10, test5;
                    "resolutions", 20, test6;
                    "decision", 20, test7;
                    "decisionTrace", 10, test8
                  ] in
  List.fold_left 
    (fun (l_res,errTest) (nom_f,bareme,t) -> 
       let (comment, errTest') = t () in
         (l_res @ [(nom_f, bareme, comment, errTest')], 
          (errTest || errTest'))
    ) ([],false) all_tests;;


(* Testeur *)
(* ------------------------------------------------------------------------ *)
let testeur () =
  print_endline "Resultats:";
  print_endline "----------";
  List.iter
      (fun (nom_f, bareme, comment, _) -> 
         Printf.printf "\t- %s (%d pts): " nom_f bareme;
         if comment = [] then
            Printf.printf "OK\n"
         else
            begin
              print_newline();
              List.iter (fun c -> print_endline ("\t\t- " ^ c)) comment
            end
      ) (fst(test()))