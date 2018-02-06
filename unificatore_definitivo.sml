(*DEFINIZIONE DEI DATATYPE*)
datatype termine =
	Var of string |
	Fun of string* (termine list);

datatype equazione =
	Eq of (termine*termine)|
	Neq of (termine*termine);

(*FUNZIONI PER LA STAMPA*)
fun termineToString (Var(v)) = "Var("^v^")" |
	termineToString (Fun(n,lista)) = n^"("^(listaToString lista)^")"
and listaToString [] = "" |
	listaToString (h::t) = (termineToString h) ^ (listaToString t);

fun equazioneToString (Eq(terminel,terminer)) = "Eq(" ^ (termineToString terminel) ^ "=" ^ (termineToString terminer) ^ ")" |
	equazioneToString (Neq(terminel, terminer)) = "Neq(" ^ (termineToString terminel) ^ "=" ^ (termineToString terminer) ^ ")";

fun listaEquazioniToString [] = ""
   | listaEquazioniToString (h :: t) = equazioneToString h ^"," ^ listaEquazioniToString t;

(*ECCEZIONE SE L'ALGORITMO FALLISCE*)
exception noUnificatore of string;

(*ALGORITMO DI UNIFICAZIONE*)
fun term_reduction(l1: (termine list), l2: (termine list)): equazione list =
	map (fn x => Eq(x)) (ListPair.zipEq (l1, l2));

fun presente (v: string, tl: termine):bool =
	case tl of
		Var(u) => if u=v then true else false |
		Fun(s,tl) => List.exists( fn x=> presente(v,x) ) tl;

fun  sostituisci (nomeIncognita:string, sostituzione: termine, t:termine): termine =
	case t of 
		Fun(s,tl) => Fun(s, map (fn x=> sostituisci (nomeIncognita,sostituzione,x) ) tl)|
		Var(v) => if v=nomeIncognita then sostituzione else t;
		

fun sostituisci_init (nomeincognita:string, sostituzione: termine, e: equazione): equazione =
	case e of
		Neq(l,r) => Neq( sostituisci(nomeincognita,sostituzione,l) , sostituisci(nomeincognita,sostituzione,r))|
		Eq(l,r) => Eq(sostituisci(nomeincognita,sostituzione,l) , sostituisci(nomeincognita,sostituzione,r));

fun unifica_elem (e: equazione, s1: equazione list):equazione list =
	case e of
		Neq(t) => [Neq(t)] @ s1|
		Eq(Var(v),Var(u)) => if v=u then s1 else (map (fn x => sostituisci_init(v,Var(u),x)) s1) @ [Neq(Var(v),Var(u))]|
		Eq(Var(v),Fun(s,tl)) => if (presente (v, Fun(s,tl))) then raise noUnificatore("No unificatore")
								else (map (fn x => sostituisci_init(v,Fun(s,tl),x)) s1) @ [Neq(Var(v),Fun(s,tl))]|
		Eq(Fun(s3,l3),Fun(s2,l2)) => if (s3=s2 andalso length l3 = length l2 ) 
			then  (if (not (null l3)) then (term_reduction(l3,l2) @ s1) else (*[Eq(Fun(s3,l3),Fun(s2,l2))]@*)s1 ) else raise noUnificatore("No unificatore")|
		Eq(Fun(s,tl),Var(v)) => [Eq(Var(v),Fun(s,tl))] @ s1;

fun unifica(s: equazione list): equazione list =
	let 
		val s1 = unifica_elem (hd s, tl s)
	in 
		if (s1 = s) then s1 else (unifica s1)
	end;
	
(*TESTING*)
val test1 = unifica [ Eq( Fun("arrow", [Fun("arrow", [Var("X"),Var("Y")]),Var("Z")]) ,
						  Fun("arrow", [Var("W")                         ,Var("Z")])	)];
print (listaEquazioniToString test1);

val test2 = unifica [ Eq( Fun("arrow", [Var("x"),Fun("bool",[])]) ,
						  Fun("arrow", [Fun("int",[]),Var("Y") ]) )]  ;
print (listaEquazioniToString test2);

val test3 = unifica [ Eq( Var("x"),
						  Fun("arrow", [Var("Y"),Var("Z") ]) )]  ;
print (listaEquazioniToString test3);

val test5 = unifica [ Eq( Fun("arrow", [Var("X"),Var("Y")]) ,
						  Fun("arrow", [Var("Y"),Var("X")]) )]  ;
print (listaEquazioniToString test5);

val test6 = unifica [ Eq( Fun("arrow", [Var("X")     ,Fun("arrow", [Fun("bool",[]),Var("Y")])]) ,
						  Fun("arrow", [Fun("int",[]),Fun("arrow", [Var("Z"),Var("Y")])]))];
print (listaEquazioniToString test6);

val test7 = unifica [ Eq( Fun("arrow", [Var("C"),Fun("arrow",[Var("D"),Var("C")])]),
						  Fun("arrow", [Fun("int",[]),Var("K")]) )];
print (listaEquazioniToString test7);

val test8 = unifica [ Eq( Var("x"),
						  Fun("arrow", [Fun("int",[]),Fun("bool",[])]) )];
print (listaEquazioniToString test8);

val test9 = unifica [ Eq( Fun("arrow", [Var("A"),Fun("arrow",[Var("B"),Var("A")])]),
						  Fun("arrow", [Var("D"),Fun("arrow",[Var("C"),Var("J")])]) )];
print (listaEquazioniToString test9);

val test10 = unifica [ Eq( Fun("arrow", [Var("X"), Fun("arrow", [Var("Y"), Var("X")])]), 
						   Fun("arrow", [Var("Z"), Fun("arrow", [Var("Z"), Var("X")])]))];
print (listaEquazioniToString test10);
(*val test4 = unifica [ Eq( Var("X"),
						  Fun("arrow", [Var("Y"),Var("X") ]) )]  ;
print (listaEquazioniToString test4);*)

