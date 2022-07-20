/// Motoko surface syntax, as a recursive type (a tree).
///
module {

  type Pos = {
    line : Nat;
    col : Nat;
  };

  type Region = {
    begin : Pos;
    end : Pos;
  };

  /// In a multi-user setting, an expression may "accumulate" authors.
  /// (Consider Motoko Playground, but in a multi-user mode.)
  type Authors = [Principal];

  /// Creation/modification time.
  type Time = Int;

  type Annotated<X> = {
    file : Text;
    region : Region;
    authors : ?Authors;
    time : Time;
    it : X;
  };
  
  /// Expressions whose sub-expressions are generically typed.
  type Exp_<E> = {
    #prim : Text;
    #var_ : Id;
    #lit : Lit;
    #actorUrl : E;
    #un : (UnOp, E);
    // to do -- continue.
  };

  /// Plain expressions, without annotations ("plain ASTs").
  type Exp = Exp_<Exp>; 

  /// Expressions where source-location annotations are present throughout.
  type ExpA = Annotated<Exp_<ExpA>>;

  /// Identifiers.
  type Id = Text;

  /// Unary operators.
  type UnOp = {
    #pos;
    #neg;
    #not_;
  };

  type Lit = {
    #Bool : Bool;
    #Null;
    #Text : Text;
    #Int : Int;
    #Nat : Nat;
    #Nat8 : Nat8;
    #Nat16 : Nat16;
    #Nat32 : Nat32;
    #Nat64 : Nat64;
    #Int8 : Int8;
    #Int16 : Int16;
    #Int32 : Int32;
    #Int64 : Int64;
    #Float : Float;
    #Blob : Text;
    #Char : Text;
    // | PreLit of string * Type.prim
  };
}

/*
Working notes about progress/coverage.

type exp = (exp', typ_note) Source.annotated_phrase
and exp' =
x  | PrimE of string                            (* primitive *)
x  | VarE of id                                 (* variable *)
x  | LitE of lit ref                            (* literal *)
x  | ActorUrlE of exp                           (* actor reference *)
x  | UnE of op_typ * unop * exp                 (* unary operator *)
  | BinE of op_typ * exp * binop * exp         (* binary operator *)
  | RelE of op_typ * exp * relop * exp         (* relational operator *)
  | ShowE of (op_typ * exp)                    (* debug show operator *)
  | ToCandidE of exp list                      (* to_candid operator *)
  | FromCandidE of exp                         (* from_candid operator *)
  | TupE of exp list                           (* tuple *)
  | ProjE of exp * int                         (* tuple projection *)
  | OptE of exp                                (* option injection *)
  | DoOptE of exp                              (* option monad *)
  | BangE of exp                               (* scoped option projection *)
  | ObjBlockE of obj_sort * dec_field list     (* object block *)
  | ObjE of exp_field list                     (* record literal *)
  | TagE of id * exp                           (* variant *)
  | DotE of exp * id                           (* object projection *)
  | AssignE of exp * exp                       (* assignment *)
  | ArrayE of mut * exp list                   (* array *)
  | IdxE of exp * exp                          (* array indexing *)
  | FuncE of string * sort_pat * typ_bind list * pat * typ option * sugar * exp  (* function *)
  | CallE of exp * inst * exp                  (* function call *)
  | BlockE of dec list                         (* block (with type after avoidance)*)
  | NotE of exp                                (* negation *)
  | AndE of exp * exp                          (* conjunction *)
  | OrE of exp * exp                           (* disjunction *)
  | IfE of exp * exp * exp                     (* conditional *)
  | SwitchE of exp * case list                 (* switch *)
  | WhileE of exp * exp                        (* while-do loop *)
  | LoopE of exp * exp option                  (* do-while loop *)
  | ForE of pat * exp * exp                    (* iteration *)
  | LabelE of id * typ * exp                   (* label *)
  | BreakE of id * exp                         (* break *)
  | RetE of exp                                (* return *)
  | DebugE of exp                              (* debugging *)
  | AsyncE of typ_bind * exp                   (* async *)
  | AwaitE of exp                              (* await *)
  | AssertE of exp                             (* assertion *)
  | AnnotE of exp * typ                        (* type annotation *)
  | ImportE of (string * resolved_import ref)  (* import statement *)
  | ThrowE of exp                              (* throw exception *)
  | TryE of exp * case list                    (* catch exception *)
  | IgnoreE of exp                             (* ignore *)
(*
  | FinalE of exp * exp                        (* finally *)
  | AtomE of string                            (* atom *)
*)

and dec_field = dec_field' Source.phrase
and dec_field' = {dec : dec; vis : vis; stab: stab option}

and exp_field = exp_field' Source.phrase
and exp_field' = {mut : mut; id : id; exp : exp}

and case = case' Source.phrase
and case' = {pat : pat; exp : exp}


(* Declarations *)

and dec = (dec', typ_note) Source.annotated_phrase
and dec' =
  | ExpD of exp                                (* plain unit expression *)
  | LetD of pat * exp                          (* immutable *)
  | VarD of id * exp                           (* mutable *)
  | TypD of typ_id * typ_bind list * typ       (* type *)
  | ClassD of                                  (* class *)
      sort_pat * typ_id * typ_bind list * pat * typ option * obj_sort * id * dec_field list


(* Program (pre unit detection) *)

type prog_note = { filename : string; trivia : Trivia.triv_table }
type prog = (prog', prog_note) Source.annotated_phrase
and prog' = dec list


- - - - - snip (reordered from original.) - - - - - 

and mut' = Const | Var

and path = (path', Type.typ) Source.annotated_phrase
and path' =
  | IdH  of id
  | DotH of path * id

type typ = (typ', Type.typ) Source.annotated_phrase
and typ' =
  | PathT of path * typ list                       (* type path *)
  | PrimT of string                                (* primitive *)
  | ObjT of obj_sort * typ_field list              (* object *)
  | ArrayT of mut * typ                            (* array *)
  | OptT of typ                                    (* option *)
  | VariantT of typ_tag list                       (* variant *)
  | TupT of typ_item list                          (* tuple *)
  | FuncT of func_sort * typ_bind list * typ * typ (* function *)
  | AsyncT of scope * typ                          (* future *)
  | AndT of typ * typ                              (* intersection *)
  | OrT of typ * typ                               (* union *)
  | ParT of typ                                    (* parentheses, used to control function arity only *)
  | NamedT of id * typ                             (* parenthesized single element named "tuple" *)

and scope = typ
and typ_field = typ_field' Source.phrase
and typ_field' = {id : id; typ : typ; mut : mut}

and typ_tag = typ_tag' Source.phrase
and typ_tag' = {tag : id; typ : typ}

and bind_sort = Type.bind_sort Source.phrase
and typ_bind = (typ_bind', Type.con option) Source.annotated_phrase
and typ_bind' = {var : id; sort : bind_sort; bound : typ;}

and typ_item = id option * typ


(* Literals *)

type lit =
x  | NullLit
x  | BoolLit of bool
x  | NatLit of Numerics.Nat.t
x  | Nat8Lit of Numerics.Nat8.t
x  | Nat16Lit of Numerics.Nat16.t
x  | Nat32Lit of Numerics.Nat32.t
x  | Nat64Lit of Numerics.Nat64.t
x  | IntLit of Numerics.Int.t
x  | Int8Lit of Numerics.Int_8.t
x  | Int16Lit of Numerics.Int_16.t
x  | Int32Lit of Numerics.Int_32.t
x  | Int64Lit of Numerics.Int_64.t
x  | FloatLit of Numerics.Float.t
x  | CharLit of Value.unicode
x  | TextLit of string
x  | BlobLit of string
?  | PreLit of string * Type.prim


(* Patterns *)

type pat = (pat', Type.typ) Source.annotated_phrase
and pat' =
  | WildP                                      (* wildcard *)
  | VarP of id                                 (* variable *)
  | LitP of lit ref                            (* literal *)
  | SignP of unop * lit ref                    (* signed literal *)
  | TupP of pat list                           (* tuple *)
  | ObjP of pat_field list                     (* object *)
  | OptP of pat                                (* option *)
  | TagP of id * pat                           (* tagged variant *)
  | AltP of pat * pat                          (* disjunctive *)
  | AnnotP of pat * typ                        (* type annotation *)
  | ParP of pat                                (* parenthesis *)
(*
  | AsP of pat * pat                           (* conjunctive *)
*)

and pat_field = pat_field' Source.phrase
and pat_field' = {id : id; pat : pat}


(* Expressions *)

type vis = vis' Source.phrase
and vis' =
  | Public of string option
  | Private
  | System

let is_public vis = match vis.Source.it with Public _ -> true | _ -> false

type stab = stab' Source.phrase
and stab' = Stable | Flexible

type op_typ = Type.typ ref (* For overloaded resolution; initially Type.Pre. *)


type inst = (typ list option, Type.typ list) Source.annotated_phrase (* For implicit scope instantiation *)

type sort_pat = (Type.shared_sort * pat) Type.shared Source.phrase

type sugar = bool (* Is the source of a function body a block `<block>`,
                     subject to further desugaring during parse,
                     or the invariant form `= <exp>`.
                     In the final output of the parser, the exp in FuncE is
                     always in its fully desugared form and the
                     value of the sugar field is irrelevant.
                     This flag is used to correctly desugar an actor's
                     public functions as oneway, shared functions *)


- - - - - - - 


(* Operators *)

type unop =
  | PosOp                                       (* +x *)
  | NegOp                                       (* -x *)
  | NotOp                                       (* bitwise negation *)

type binop =
  | AddOp                                       (* x+y *)
  | SubOp                                       (* x-y *)
  | MulOp                                       (* x*y *)
  | DivOp                                       (* x/y *)
  | ModOp                                       (* x%y *)
  | PowOp                                       (* x^y *)
  | AndOp                                       (* bitwise operators... *)
  | OrOp
  | XorOp
  | ShLOp
  | ShROp
  | RotLOp
  | RotROp
  | WAddOp                                      (* wrapping operators... *)
  | WSubOp
  | WMulOp
  | WPowOp
  | CatOp                                       (* concatenation *)

type relop =
  | EqOp                                        (* x=y *)
  | NeqOp                                       (* x!=y *)
  | LtOp                                        (* x<y *)
  | GtOp                                        (* x>y *)
  | LeOp                                        (* x<=y *)
  | GeOp                                        (* x>=y *)


*/

