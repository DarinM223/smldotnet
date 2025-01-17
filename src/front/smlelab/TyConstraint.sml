(*======================================================================*)
(* Solve a set of type constraints generated by the inference algorithm	*)
(*======================================================================*)

structure TyConstraint :> TYCONSTRAINT =
struct

    open ElabState

(* If set, list initial and final sets of constraints *)
val showConstraints = Controls.add false "elab.showConstraints"

(* If set, display any constraint that "fails" *)
val showConstraintFailure = Controls.add false "elab.showConstraintFailure"

(* If set, display the partitioning of constraints *)
val showConstraintPartitions = Controls.add false "elab.showConstraintPartitions"

(* If unset, don't solve the constraints at all *)
val doSolve = Controls.add true "elab.doSolve"

(*----------------------------------------------------------------------*)
(* Various subtyping relations.						*)
(*----------------------------------------------------------------------*)
datatype Sub = 
    (* The first four are produced during elaboration and must be solved. *)
  MembSub	(* on method/field types *)
| MethSub 	(* on method types *)
| Super		(* immediate superclass *)
| Cast		(* widening or narrowing on classes and arrays *)

  (* These are produced during solving (by the new solver) *)
| ArgVecSub	(* on argument list types (products) *)
| ArgSub        (* on argument types. *)
| Eq		(* equal *)
  (* There is also a MembTyEq, but this is in Constraint. *)

  (* The remaining are produced by the old solver and need not be in the new. *)
| RefSub	(* widening on classes and arrays *)

(*----------------------------------------------------------------------*)
(* Three kinds of constraint.						*)
(*----------------------------------------------------------------------*)
datatype Constraint = 
  (* ty1 <=s ty2 *)
  Sub of SMLTy.Type * Sub * SMLTy.Type

  (* (t1, t2) = (t1', t2') *)
| MembTyEq of InterOpTypes.MemberType * InterOpTypes.MemberType

  (* ty has m : membty *)
| Has of SMLTy.Type * Symbol.symbol * InterOpTypes.MemberType

  (* ty1 has static m : ty2 *)
  (* m=NONE indicates a constructor *)
| HasStatic of SMLTy.Type * Symbol.symbol option * SMLTy.Type

type LocatedConstraint = Constraint * Syntax.Location

(*----------------------------------------------------------------------*)
(* Type variables in a constraint					*)
(*----------------------------------------------------------------------*)
(* SL: or *)
(*
fun tyvars' (Sub(ty1,_,ty2) | HasStatic(ty1,_,ty2)) = 
    TyVar.Set.union(SMLTy.tyvars ty1, SMLTy.tyvars ty2)
*)
fun tyvars' (Sub(ty1,_,ty2)) = 
    TyVar.Set.union(SMLTy.tyvars ty1, SMLTy.tyvars ty2)
  | tyvars' (HasStatic(ty1,_,ty2)) = 
    TyVar.Set.union(SMLTy.tyvars ty1, SMLTy.tyvars ty2)

  | tyvars' (Has(ty1,_,(ty2,ty3))) =
    TyVar.Set.union(SMLTy.tyvars ty1, 
    TyVar.Set.union(SMLTy.tyvars ty2, SMLTy.tyvars ty3))
  | tyvars' (MembTyEq((ty1, ty2), (ty3, ty4))) =
    TyVar.Set.union(SMLTy.tyvars ty1, 
    TyVar.Set.union(SMLTy.tyvars ty2,
    TyVar.Set.union (SMLTy.tyvars ty3, SMLTy.tyvars ty4)))

fun isMono s =
  case s of
    TyVar.Normal s => TySort.<=(s, TySort.mono) 
  | _ => false

fun tyvars c = TyVar.Set.filter (isMono o TyVar.sort) (tyvars' c)

(*----------------------------------------------------------------------*)
(* Relation to string for error messages and diagnostics.		*)
(*----------------------------------------------------------------------*)
fun relToString r =
case r of
  MembSub => " member-coerces to "
| MethSub => " method-coerces to "
| ArgVecSub => " argument-list-coerces to "
| ArgSub => " argument-coerces to "
| RefSub => " widens to "
| Super => " extends "
| Cast => " casts to "
| Eq => " = "

(*----------------------------------------------------------------------*)
(* Pretty-print a located-constraint for error messages and diagnostics.*)
(*----------------------------------------------------------------------*)
fun toString (c,loc) =
case c of
  Sub(ty1, r, ty2) => 
  SMLTy.toString ty1 ^ relToString r ^
  SMLTy.toString ty2

| MembTyEq((t1, t2), (t1', t2')) =>
  "(" ^ SMLTy.toString t1 ^ ", " ^ SMLTy.toString t2 ^ ") = (" ^
  SMLTy.toString t1' ^ ", " ^ SMLTy.toString t2' ^ ")"

| Has(ty1, id, (ty2,ty3)) => 
  SMLTy.toString ty1 ^ " has " ^ Id.toString id ^ " : " ^ 
  SMLTy.toString ty3 ^ " in " ^ SMLTy.toString ty2

| HasStatic(ty1, SOME id, ty2) => 
  SMLTy.toString ty1 ^ " has static " ^ Id.toString id ^ " : " ^
  SMLTy.toString ty2

| HasStatic(ty1, NONE, ty2) => 
  SMLTy.toString ty1 ^ " has constructor : " ^
  SMLTy.toString ty2

(*----------------------------------------------------------------------*)
(* Partition the constraints according to type variables		*)
(* The parts can be solved independently leading to improved performance*)
(*----------------------------------------------------------------------*)
structure Partition = Partition(IntMap)
fun partition C =
let
  fun tv (c,loc) = List.mapPartial TyVar.toInt (TyVar.Set.listItems (tyvars c))
  val all = List.mapPartial TyVar.toInt 
  (TyVar.Set.listItems (foldr TyVar.Set.union TyVar.Set.empty 
    (map (tyvars o #1) C)))
  val partition = Partition.new_partition all
  fun loop (partition, []) = partition
    | loop (partition, c::C) =
      case tv c of
        [] => loop (partition, C)
      | n::ns =>
        let
          val part = Partition.find_part(partition,n)
          fun merge (partition,[]) = loop (partition,C)
            | merge (partition,n'::ns) =
              merge (Partition.union(partition,
                Partition.find_part(partition,n'),
                Partition.find_part(partition,n)),ns)
        in merge(partition,ns) end      

  val partition = loop (partition,C)

  fun gather (ground, result, []) = ground @ IntMap.listItems result
    | gather (ground, result, c::C) =
      case tv c of
        [] => gather ([c]::ground, result, C)
      | n::ns =>
        let
          val part = Partition.find_part(partition,n)
          val rep = hd (Partition.list_part (partition,part))
        in
          case IntMap.find(result, rep) of
            NONE => gather(ground, IntMap.insert(result, rep, [c]), C)
          | SOME C' => gather(ground, IntMap.insert(result, rep, c::C'), C)
        end
in
  gather ([], IntMap.empty, C)
end


(*----------------------------------------------------------------------*)
(* The global list of constraints accumulated during inference.		*)
(*----------------------------------------------------------------------*)
val constraints = ref ([] : (Constraint*Syntax.Location) list)

(*----------------------------------------------------------------------*)
(* Add to the list							*)
(*----------------------------------------------------------------------*)
fun add (c,loc) = constraints := (c,loc) :: !constraints

(*----------------------------------------------------------------------*)
(* Dump constraints to the log with a prefixed message.			*)
(*----------------------------------------------------------------------*)
fun dump (message, C) =
(
  Debug.print ("\n" ^ message ^ ":");
  List.app (fn c => Debug.print ("\n  " ^ toString c)) C
)

(*----------------------------------------------------------------------*)
(* A substitution is represented as a map; the reference cell is 	*)
(* updated when the substitution is "frozen".				*)
(*----------------------------------------------------------------------*)
type Substitution = (SMLTy.Type * SMLTy.TyVarOrType ref) TyVar.Map.map

(*----------------------------------------------------------------------*)
(* Apply the substitution at one level.					*)
(*----------------------------------------------------------------------*)
fun ap S ty =
case SMLTy.fromTyVar ty of
  SOME tyvar =>
  (case TyVar.Map.find(S, tyvar) of
    NONE => ty
  | SOME (ty,_) => ty)

| NONE => 
  ty 

(* Apply the substitution to a constraint. *)
fun apC S c = 
    case c of
	Sub(t1, rel, t2) => Sub(ap S t1, rel, ap S t2)
      | MembTyEq((t1, t2), (t1', t2')) =>
	    MembTyEq((ap S t1, ap S t2), (ap S t1', ap S t2'))
      | Has (t1, symb, (t2, t3)) => Has (ap S t1, symb, (ap S t2, ap S t3))
      | HasStatic (t1, symb, t2) => HasStatic (ap S t1, symb, ap S t2)

(*----------------------------------------------------------------------*)
(* "Freeze" the substitution by assigning the refs.			*)
(*----------------------------------------------------------------------*)
val freeze = (TyVar.Map.app (fn (ty,(r:SMLTy.TyVarOrType ref)) => r := SMLTy.Type ty))

(*----------------------------------------------------------------------*)
(* Substitution constructors: the identity, singletons, and composition.*)
(*----------------------------------------------------------------------*)
val identity = TyVar.Map.empty

fun singleton (r as ref (SMLTy.TyVar tyvar)) ty = 
  TyVar.Map.insert (TyVar.Map.empty, tyvar, (ty, r))
  | singleton _ _ = raise Fail "singleton: not a TyVar"

fun compose (S1,S2) = TyVar.Map.unionWith (fn (ty1,ty2) => ty2)
  (S1, TyVar.Map.map 
    (fn (ty, r) => 
    (SMLTy.appSubst (TyVar.Map.listItemsi (TyVar.Map.map #1 S1)) ty, r)) S2)


(*----------------------------------------------------------------------*)
(* Dump a substitution to the log.					*)
(*----------------------------------------------------------------------*)
fun dumpSubst S =
  TyVar.Map.appi (fn (tyvar, (ty,_)) => 
    Debug.print ("\n  " ^ TyVar.toString tyvar
    ^ " |-> " ^ SMLTy.toString ty)) S

fun dumpSolutions sols =
List.app (fn (S,C) => 
(
  Debug.print "\nSolution subst:";
  dumpSubst S;
  Debug.print "\nSolution constraints:";
  List.app (fn c => Debug.print ("\n" ^ toString c)) C
)) sols

(*----------------------------------------------------------------------*)
(* Crude, purely-functional unification.				*)
(* Given a substitution S and two types ty1,ty2 return			*)
(*   NONE if S(ty1) cannot be unified with S(ty2), and			*)
(*   SOME S' for S<S' if S' is the mgu of S(ty1) and S(ty2)		*)
(*----------------------------------------------------------------------*)
fun unify S (ty1,ty2) =
let
  val ty1 = ap S ty1
  val ty2 = ap S ty2
in
  if SMLTy.eq (ty1,ty2) then SOME S
  else
  case (SMLTy.fromTyVarRef ty1, SMLTy.fromTyVarRef ty2) of
    (SOME r, _) =>
    if SMLTy.occurs r ty2 orelse not (TySort.<=(SMLTy.sort false ty1, TySort.mono)) then NONE
    else SOME (compose (singleton r ty2, S))

  | (_, SOME r) =>
    if SMLTy.occurs r ty1 orelse not (TySort.<=(SMLTy.sort false ty2, TySort.mono))  then NONE
    else SOME (compose (singleton r ty1, S))

  | _ =>
    (case (SMLTy.fromFunType ty1, SMLTy.fromFunType ty2) of
      (SOME (ty1,ty1'), SOME (ty2,ty2')) =>
      (case unify S (ty1,ty2) of
        NONE => NONE
 
      | SOME S' =>
        unify S' (ty1', ty2')
      )

    | _ => 
      case (SMLTy.fromConsType ty1, SMLTy.fromConsType ty2) of
        (SOME (tys1, tn1), SOME (tys2, tn2)) =>
        if TyName.eq(tn1,tn2) then unifyVec S (tys1,tys2) else NONE

      | _ =>
(*@TODO:review
        case (SMLTy.fromRefType ty1, SMLTy.fromRefType ty2) of
          (SOME ty1, SOME ty2) => unify S (ty1,ty2) *)
	  case (SMLTy.fromRefType ty1, SMLTy.fromRefType ty2) of
	       (SOME (ty1,ty1'), SOME (ty2,ty2')) =>
		   (case unify S (ty1,ty2) of
			NONE => NONE
		      | SOME S' =>
			    unify S' (ty1', ty2'))
        | _ => 
        case (SMLTy.fromArrayType ty1, SMLTy.fromArrayType ty2) of
          (SOME ty1, SOME ty2) => unify S (ty1,ty2)
        | _ => 
        case (SMLTy.fromClassType ty1, SMLTy.fromClassType ty2) of
          (SOME (SMLTy.MLClass { tyname, ...}), 
           SOME (SMLTy.MLClass {tyname=tyname',...})) =>
          if TyName.eq(tyname,tyname') then SOME S
          else NONE
 
        | _ =>
          case (SMLTy.fromRecType ty1, SMLTy.fromRecType ty2) of
            (SOME row1, SOME row2) =>
            if Eq.list (fn ((lab1,_),(lab2,_)) => Symbol.equal(lab1,lab2))
               (row1,row2) 
            then unifyVec S (map #2 row1, map #2 row2) else NONE
          | _ => NONE
    )
end

and unifyVec S ([],[]) = SOME S
  | unifyVec S (ty1::tys1, ty2::tys2) =
    (case unify S (ty1,ty2) of
      NONE => NONE
    | SOME S => unifyVec S (tys1,tys2)
    )
  | unifyVec S _ = NONE


(*****************************************************************************)
(*****************************************************************************)

fun failure c =
(if Controls.get showConstraintFailure 
 then Debug.print ("\nFail: " ^ toString c)
 else (); [])

(*----------------------------------------------------------------------*)
(* Given a single unresolved constraint, return an appropriate error 	*)
(* message								*)
(*----------------------------------------------------------------------*)

fun public flags = Symbol.Set.member(flags,Id.publicSym)

fun message (c,loc) =
case c of
  HasStatic(ty1,NONE,ty2) =>
  let
    val tys = map #2 (InterOp.getConstructors public ty1)
  in
    "constructor used at incorrect type " ^ SMLTy.toString ty2 ^ "; expected "^
    Pretty.simpleVec " or " SMLTy.toString tys
  end

| HasStatic(ty1,SOME name,ty2) =>
  let
    val fldtys = map #1 (InterOp.getStaticFields (ty1, name))
    val methtys = (InterOp.getStaticMethods (ty1, name))
  in
    "static member " ^ Id.toString name ^ 
    " used at incorrect type " ^ SMLTy.toString ty2 ^ "; expected " ^
    Pretty.simpleVec " or " SMLTy.toString (fldtys @ methtys)
  end
  
| Has(ty1,name,(_,ty2)) =>
  if isSome(SMLTy.fromTyVar ty1)
  then "expression on left of .# has unresolved type"
  else if not (SMLTy.isClass ty1 orelse InterOp.isValueType ty1 orelse InterOp.isValueTypeRef ty1)
  then "expression on left of .# has non-class type " ^ SMLTy.toString ty1
  else
  let
  (*   val ty1 = case SMLTy.fromRefType ty1 of SOME ty => ty | NONE => ty1 *)
    val ty1 = case SMLTy.fromRefType ty1 of SOME (ty,_) => ty | NONE => ty1 
    val fields = InterOp.getFields (ty1, name)
    val methods = map #2 (InterOp.getMethods (ty1, name)) (* strip off modifiers *)
    val members = fields @ methods
  in
    if null members
    then "no such member " ^ Id.toString name ^ " in class " ^
      SMLTy.toString ty1
    else
      "member " ^ Id.toString name ^ 
      " used at incorrect or ambiguous type " ^ SMLTy.toString ty2 ^ "; expected " ^
      Pretty.simpleVec " or " (fn (ownerty,memberty) => SMLTy.toString memberty ^ "(from " ^ SMLTy.toString ownerty ^ ")") members
  end

| _ =>
  "unresolved constraint: " ^ toString (c,loc)

(*----------------------------------------------------------------------*)
(* Given a set of unresolved constraints add appropriate error messages	*)
(*----------------------------------------------------------------------*)
fun report C =
let
  val firstpass = List.filter (fn (Sub _,_) => false | _ => true) C
  val secondpass = if null firstpass then C else firstpass
in
  List.app (fn c as (_,loc) => ElabState.error (Error.error (loc, message c), []))
  secondpass
end

(* SL: or *)
(*
fun resolveNum ((Sub(ty1,_,ty2) | HasStatic(ty1,_,ty2)),loc) =
  (SMLTyUnify.resolve loc TyVar.Set.empty ty1; 
   SMLTyUnify.resolve loc TyVar.Set.empty ty2;
   ())
*)
fun resolveNum (Sub(ty1,_,ty2),loc) =
  (SMLTyUnify.resolve loc TyVar.Set.empty ty1; 
   SMLTyUnify.resolve loc TyVar.Set.empty ty2;
   ())
  | resolveNum (HasStatic(ty1,_,ty2),loc) =
  (SMLTyUnify.resolve loc TyVar.Set.empty ty1; 
   SMLTyUnify.resolve loc TyVar.Set.empty ty2;
   ())

  | resolveNum (Has(ty1,_,(ty2,ty3)),loc) =
  (SMLTyUnify.resolve loc TyVar.Set.empty ty1; 
   SMLTyUnify.resolve loc TyVar.Set.empty ty2;
   SMLTyUnify.resolve loc TyVar.Set.empty ty3;
   ())

  | resolveNum (MembTyEq ((ty1, ty2), (ty3, ty4)), loc) =
  (SMLTyUnify.resolve loc TyVar.Set.empty ty1; 
   SMLTyUnify.resolve loc TyVar.Set.empty ty2;
   SMLTyUnify.resolve loc TyVar.Set.empty ty3;
   SMLTyUnify.resolve loc TyVar.Set.empty ty4;
   ())
  

(*----------------------------------------------------------------------*)
(* Given a constraint set C and a substitution S, return a list L 	*)
(* of (substitution,constraint set) pairs such that 			*)
(* S<S' and S'(C') |- S'(C) for every (S',C') in L			*)
(*----------------------------------------------------------------------*)
fun solve (S,C) =
let
  fun solve' (S,C) =
  case C of
    [] => [(S,[])]
  | c::C =>
    let
      val solutions = solveOne (S,c)
      fun solve'' (S',C') = 
        let val solutions' = solve' (S',C)
        in map (fn (S'',C'') => (S'',C''@C')) solutions' end
    in
      List.concat (map solve'' solutions)
    end      
  val solutions = solve' (S,C)
in
  solutions
end

and solveOne (S,c as (constraint,loc)) =
case constraint of
  Sub(ty1,rel,ty2) =>
  let
    val ty1 = ap S ty1
    val ty2 = ap S ty2
    val c = (Sub(ty1,rel,ty2),loc)
  in
    if SMLTy.eq(ty1, ty2) then [(S,[])]
    else
      case rel of
        Eq =>
        (case unify S (ty1, ty2) of
          NONE => failure c
        | SOME S' => [(S',[])])
          
      | MembSub =>
        if isSome (SMLTy.fromFunType ty1) orelse 
           isSome (SMLTy.fromFunType ty2)
        then solveOne (S, (Sub(ty1,MethSub,ty2),loc))
        else  
        if isSome (SMLTy.fromRefType ty1) orelse 
           isSome (SMLTy.fromRefType ty2) orelse
           InterOp.isPrimitive ty1 orelse InterOp.isPrimitive ty2 orelse 
           isSome (SMLPrimTy.fromOption ty1) orelse 
	   isSome (SMLPrimTy.fromOption ty2) 
        then (case unify S (ty1,ty2) of NONE => [] | SOME S => [(S,[])])
        else
        if isSome (SMLTy.fromTyVar ty1) andalso
           isSome (SMLTy.fromTyVar ty2)
        then [(S,[c])]
        else failure c

      | RefSub =>
        if isSome (SMLTy.fromTyVar ty1) orelse
           isSome (SMLTy.fromTyVar ty2)
        then [(S,[c])]
        else 
        if InterOp.refWidens (ty1,ty2) 
	then [(S,[])]
        else failure c

      | Super =>
        (case InterOp.super ty1 of
          NONE => [(S,[c])]
        | SOME ty1 => solveOne (S, (Sub(ty1,Eq,ty2),loc))
        )           

      | Cast =>
        if InterOp.isInterop ty1 andalso InterOp.isInterop ty2
        andalso 
          (InterOp.refWidens (ty1, ty2) orelse InterOp.refNarrows(ty1,ty2))
        then [(S,[])]
        else [(S,[c])]
            
      (*TODO: prim <argsub ref option when prim < ref  *)
      | ArgSub =>
        if InterOp.isPrimitive ty1 orelse InterOp.isPrimitive ty2
        then solveOne (S, (Sub(ty1,Eq,ty2),loc))
        else 
        (case (SMLPrimTy.fromOption ty1, SMLPrimTy.fromOption ty2) of
          (SOME ty1', SOME ty2') => 
	      (*was: solveOne (S, (Sub(ty1,RefSub,ty2),loc)) *)
	     (*@TODO: review, test added to prevent prim > prim option *)
	      if InterOp.isPrimitive ty1 then  [(S,[c])]
	      else solveOne (S, (Sub(ty1',RefSub,ty2'),loc))
        | (NONE, SOME ty2) => 
          if isSome (SMLTy.fromTyVar ty1) then [(S,[c])]
          else solveOne (S, (Sub(ty1,RefSub,ty2),loc))
        | (SOME ty1, NONE) => 
          if isSome (SMLTy.fromTyVar ty2) then [(S,[c])]
          else []
        | (NONE, NONE) =>
          if isSome (SMLTy.fromTyVar ty1) orelse isSome (SMLTy.fromTyVar ty2)
          then [(S, [c])]
          else (case unify S (ty1, ty2) of
            NONE => solveOne (S, (Sub(ty1,RefSub,ty2),loc))
          | SOME S' => [(S', [])]
          )
        )

      | MethSub =>
        (case (SMLTy.fromFunType ty1, SMLTy.fromFunType ty2) of
          (SOME (ty1,ty1'), SOME (ty2,ty2')) => 
          (case unify S (ty1',ty2') of
            NONE => failure c
          | SOME S => solveOne (S, (Sub(ty2,ArgVecSub,ty1),loc)))

        | (NONE, SOME (ty2,ty2')) => 
          if isSome (SMLTy.fromTyVar ty1)
          then
          let
            val argvar = freshMono ()
            val resvar = freshMono ()
            val S = valOf (unify S (ty1, SMLTy.funType (argvar, resvar)))
            val S = valOf (unify S (ty2', resvar))
          in
            solveOne (S, (Sub(ty2, ArgVecSub, argvar),loc))
          end
          else failure c

        | (SOME (ty1,ty1'), NONE) =>
          if isSome (SMLTy.fromTyVar ty2)
          then
          let
            val argvar = freshMono ()
            val resvar = freshMono ()
            val S = valOf (unify S (ty2, SMLTy.funType (argvar, resvar)))
            val S = valOf (unify S (ty1', resvar))
          in
            solveOne (S, (Sub(argvar, ArgVecSub, ty1),loc))
          end
          else failure c

        | (NONE, NONE) => 
          if isSome (SMLTy.fromTyVar ty1) orelse
             isSome (SMLTy.fromTyVar ty2)
          then [(S,[c])]
          else failure c
        )

  (*..................................................................*)
  (* Subtyping between sets of method arguments.		      *)
  (*..................................................................*)
      | ArgVecSub => 
        if InterOp.isPrimitive ty1 orelse InterOp.isPrimitive ty2
        then solveOne (S, (Sub(ty1,Eq,ty2),loc))
        else
        (case (SMLTy.fromProd ty1, SMLTy.fromProd ty2) of
          (SOME tys1, SOME tys2) =>
          if length tys1 = length tys2 
          then solve (S, ListPair.map 
            (fn (ty1,ty2) => (Sub(ty1,ArgSub,ty2),loc)) (tys1,tys2))
          else failure c

        | (SOME tys1, NONE) =>
          if isSome (SMLTy.fromTyVar ty2)
          then 
          let
            val tys2 = map (fn _ => freshMono ()) tys1
            val S = valOf (unify S (ty2, SMLTy.tupleType tys2))
          in
            solveOne (S, (Sub(ty1, ArgVecSub, SMLTy.tupleType tys2),loc))
          end
          else failure c
      
        | (NONE, SOME tys2) =>
          if isSome (SMLTy.fromTyVar ty1)
          then 
          let
            val tys1 = map (fn _ => freshMono ()) tys2
            val S = valOf (unify S (ty1, SMLTy.tupleType tys1))
          in
            solveOne (S, (Sub(SMLTy.tupleType tys1, ArgVecSub, ty2),loc))
          end
          else failure c

        | (NONE, NONE) =>
          if isSome (SMLTy.fromTyVar ty1) orelse isSome (SMLTy.fromTyVar ty2)
          then [(S,[c])]
          else solveOne (S, (Sub(ty1, ArgSub, ty2),loc))
        )
  end
      
| HasStatic(ty1,SOME name,ty2) =>
  let
    val ty1 = ap S ty1
    val ty2 = ap S ty2
    val fldtys = map #1 (InterOp.getStaticFields (ty1, name))
    val methtys = InterOp.getStaticMethods (ty1, name)
  in    
    if isSome (SMLTy.fromTyVar ty1) then [(S,[c])]
    else 
      List.concat (map (fn ty => solveOne (S, (Sub(ty,Eq,ty2),loc))) 
      (fldtys@methtys))
  end

| HasStatic(ty1,NONE,ty2) =>
  let
    val ty1 = ap S ty1
    val ty2 = ap S ty2
    val tys = map #2 (InterOp.getConstructors public ty1)
  in
    if isSome (SMLTy.fromTyVar ty1) then [(S,[c])]
    else
      List.concat (map (fn ty => solveOne (S, (Sub(ty,Eq,ty2),loc))) tys)
  end

| Has(ty1,name,(ty2,ty3)) =>
  let
    val ty1 = ap S ty1
(*    val ty1 = case SMLTy.fromRefType ty1 of SOME ty => ty | NONE => ty1 *)
(*@TODO: handling references to value types should probably be done in InterOp, not here !*)
    val ty1 = case SMLTy.fromRefType ty1 of SOME (ty,_) => ty | NONE => ty1
    val ty2 = ap S ty2
    val ty3 = ap S ty3
    val fldtys = InterOp.getFields (ty1, name)
    val methtys = map #2 (InterOp.getMethods (ty1, name))
  in
    if isSome (SMLTy.fromTyVar ty1) then [(S,[c])]
    else
      List.concat (map (fn (ty2',ty3') => 
        solveOne (S, (Sub(SMLTy.funType(ty2,ty3),Eq,SMLTy.funType(ty2',ty3')),
        loc)))
      (fldtys@methtys))
  end

(*----------------------------------------------------------------------*)
(* Iteratively simplify a set of constraints by simplifying each 	*)
(* independently. IMPORTANT: no backtracking possible.			*)
(*----------------------------------------------------------------------*)
fun simplify C =
let
  fun loop (n,C) =
  let
    val C' =
    List.concat (map (fn c =>
      case solveOne (identity, c) of
        [(S,C)] => (freeze S; C)
      | _ => [c]) C)
    val n' = length C'
  in
    if n=n' then C' else loop (n',C')
  end
in
  loop (length C,C)
end

(*----------------------------------------------------------------------*)
(* Determine which of two solutions is more specific.			*)
(*----------------------------------------------------------------------*)
exception Incomparable
fun mostSpecificOf ((S1,C1),(S2,C2)) =
if null C1 andalso null C2
then 
  (case TyVar.Map.collate (fn ((ty1,_),(ty2,_)) =>
    if SMLTy.eq(ty1,ty2) then EQUAL
    else
    if InterOp.refWidens (ty1,ty2) then LESS 
    else
    if InterOp.refNarrows (ty1,ty2) then GREATER
    else raise Incomparable) (S1,S2) of
    LESS => SOME (S1,C1)
  | GREATER => SOME (S2,C2)
  | EQUAL => SOME (S1,C1)) handle Incomparable => NONE
else NONE

fun mostSpecific [] = NONE
  | mostSpecific [sol] = SOME sol
  | mostSpecific (sol1::sol2::sols) =
    case mostSpecificOf (sol1,sol2) of
      NONE => NONE
    | SOME sol => mostSpecific (sol::sols)
    
(*----------------------------------------------------------------------*)
(* Solve a set of constraints.						*)
(*----------------------------------------------------------------------*)
fun solvepart C =
let
  val _ = 
    if Controls.get showConstraintPartitions
    then dump ("Partition", C)
    else ()
  fun isValid (S,C) =
  case solve (S,C) of
    [] => false
  | _ => true
  val C = simplify C
  val solutions = solve (identity, C)
  val solutions = List.filter isValid solutions
in 
  case solutions of
    [] => C
  | [(S,C)] => (freeze S; C)
  | _ => 
    ((* Debug.print "\nMULTIPLE SOLUTIONS"; dumpSolutions solutions;  *)
    case mostSpecific solutions of
      NONE => C
    | SOME (S,C) => (freeze S; C))
end

(*----------------------------------------------------------------------*)
(* Given a single unresolved constraint, return an appropriate error 	*)
(* message								*)
(*----------------------------------------------------------------------*)
(*@BUG: why are there two definitions of message in this file? *)
fun message (c,loc) =
case c of
  HasStatic(ty1,NONE,ty2) =>
  let
    val tys = map #2 (InterOp.getConstructors public ty1)
  in
    "constructor used at incorrect type " ^ SMLTy.toString ty2 ^ "; expected "^
    Pretty.simpleVec " or " SMLTy.toString tys
  end

| HasStatic(ty1,SOME name,ty2) =>
  let
    val fldtys = map #1 (InterOp.getStaticFields (ty1, name))
    val methtys = (InterOp.getStaticMethods (ty1, name))
  in
    "static member " ^ Id.toString name ^ 
    " used at incorrect type " ^ SMLTy.toString ty2 ^ "; expected " ^
    Pretty.simpleVec " or " SMLTy.toString (fldtys @ methtys)
  end
  
| Has(ty1,name,(_,ty2)) =>
  if isSome(SMLTy.fromTyVar ty1)
  then "expression on left of .# has unresolved type"
  else if not (SMLTy.isClass ty1 orelse InterOp.isValueType ty1 orelse InterOp.isValueTypeRef ty1)
  then "expression on left of .# has non-class type " ^ SMLTy.toString ty1 (*@TODO: improve error message *)
  else
  let
    val fldtys = map #2 (InterOp.getFields (ty1, name))
    val methtys = map (#2 o #2) (InterOp.getMethods (ty1, name))
    val tys = fldtys @ methtys
  in
    if null tys
    then "no such member " ^ Id.toString name ^ " in class " ^
      SMLTy.toString ty1
    else
      "member " ^ Id.toString name ^ 
      " used at incorrect " ^ SMLTy.toString ty2 ^ "; expected " ^
      Pretty.simpleVec " or " SMLTy.toString tys
  end

| _ =>
  "unresolved constraint: " ^ toString (c,loc)

fun resolve (C as [(Sub(ty1,_,ty2),loc)]) =
  if isSome (SMLTy.fromTyVar ty1) orelse isSome (SMLTy.fromTyVar ty2)
  then
  let
    val S = valOf(unify identity (ty1,ty2))
  in
    freeze S; []
  end
  else C

  | resolve C = C

fun init () = constraints := []

fun solve () = 
  if Controls.get doSolve 
  then 
  let
    val C = !constraints
    val _ = List.app resolveNum C
    val show = Controls.get showConstraints
    val _ = if show then dump ("Before solving", C) else ()
    val C = simplify C
    val _ = if show then dump ("After simplifying", C) else ()
    val parts = partition C
    val C = List.concat (map solvepart parts)
    val parts = partition C
    val C = List.concat (map resolve parts)
    val C = simplify C
    val _ = if show then dump ("Final set", C) else ()
  in
    (*@TODO: make sure that there's no mono tyvars in the exported signature *)
    report C
  end 
  else ()

end
