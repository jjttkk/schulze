
type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

type nat =
| O
| S of nat

type ('a, 'b) sum =
| Inl of 'a
| Inr of 'b

(** val fst : ('a1 * 'a2) -> 'a1 **)

let fst = function
| (x, _) -> x

(** val snd : ('a1 * 'a2) -> 'a2 **)

let snd = function
| (_, y) -> y

(** val length : 'a1 list -> nat **)

let rec length = function
| [] -> O
| _ :: l' -> S (length l')

(** val app : 'a1 list -> 'a1 list -> 'a1 list **)

let rec app l m0 =
  match l with
  | [] -> m0
  | a :: l1 -> a :: (app l1 m0)

type comparison =
| Eq
| Lt
| Gt

type 'a sig0 = 'a
  (* singleton inductive, whose constructor was exist *)

type ('a, 'p) sigT =
| ExistT of 'a * 'p



module Pos =
 struct
  (** val compare_cont :
      comparison -> Big_int_Z.big_int -> Big_int_Z.big_int -> comparison **)

  let rec compare_cont = (fun c x y -> let s = Big_int_Z.compare_big_int x y in
  if s = 0 then c else if s < 0 then Lt else Gt)

  (** val compare : Big_int_Z.big_int -> Big_int_Z.big_int -> comparison **)

  let compare = (fun x y -> let s = Big_int_Z.compare_big_int x y in
  if s = 0 then Eq else if s < 0 then Lt else Gt)

  (** val eq_dec : Big_int_Z.big_int -> Big_int_Z.big_int -> bool **)

  let rec eq_dec p x0 =
    (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
      (fun p0 ->
      (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
        (fun p1 -> eq_dec p0 p1)
        (fun _ -> false)
        (fun _ -> false)
        x0)
      (fun p0 ->
      (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
        (fun _ -> false)
        (fun p1 -> eq_dec p0 p1)
        (fun _ -> false)
        x0)
      (fun _ ->
      (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
        (fun _ -> false)
        (fun _ -> false)
        (fun _ -> true)
        x0)
      p
 end

(** val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list **)

let rec map f = function
| [] -> []
| a :: t -> (f a) :: (map f t)

(** val forallb : ('a1 -> bool) -> 'a1 list -> bool **)

let rec forallb f = function
| [] -> true
| a :: l0 -> (&&) (f a) (forallb f l0)

module Z =
 struct
  (** val compare : Big_int_Z.big_int -> Big_int_Z.big_int -> comparison **)

  let compare = (fun x y -> let s = Big_int_Z.compare_big_int x y in
  if s = 0 then Eq else if s < 0 then Lt else Gt)

  (** val leb : Big_int_Z.big_int -> Big_int_Z.big_int -> bool **)

  let leb x y =
    match compare x y with
    | Gt -> false
    | _ -> true

  (** val ltb : Big_int_Z.big_int -> Big_int_Z.big_int -> bool **)

  let ltb x y =
    match compare x y with
    | Lt -> true
    | _ -> false

  (** val max :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  let max = Big_int_Z.max_big_int

  (** val min :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  let min = Big_int_Z.min_big_int

  (** val eq_dec : Big_int_Z.big_int -> Big_int_Z.big_int -> bool **)

  let eq_dec = Big_int_Z.eq_big_int
 end

(** val bool_of_sumbool : bool -> bool **)

let bool_of_sumbool = function
| true -> true
| false -> false

(** val z_lt_dec : Big_int_Z.big_int -> Big_int_Z.big_int -> bool **)

let z_lt_dec x y =
  match Z.compare x y with
  | Lt -> true
  | _ -> false

(** val z_le_dec : Big_int_Z.big_int -> Big_int_Z.big_int -> bool **)

let z_le_dec x y =
  match Z.compare x y with
  | Gt -> false
  | _ -> true

(** val z_ge_dec : Big_int_Z.big_int -> Big_int_Z.big_int -> bool **)

let z_ge_dec x y =
  match Z.compare x y with
  | Lt -> false
  | _ -> true

(** val z_lt_ge_dec : Big_int_Z.big_int -> Big_int_Z.big_int -> bool **)

let z_lt_ge_dec =
  z_lt_dec

(** val z_ge_lt_dec : Big_int_Z.big_int -> Big_int_Z.big_int -> bool **)

let z_ge_lt_dec =
  z_ge_dec

(** val z_lt_ge_bool : Big_int_Z.big_int -> Big_int_Z.big_int -> bool **)

let z_lt_ge_bool x y =
  bool_of_sumbool (z_lt_ge_dec x y)

(** val all_pairs : 'a1 list -> ('a1 * 'a1) list **)

let rec all_pairs = function
| [] -> []
| c :: cs ->
  (c,
    c) :: (app (all_pairs cs)
            (app (map (fun x -> (c, x)) cs) (map (fun x -> (x, c)) cs)))

(** val maxlist : Big_int_Z.big_int list -> Big_int_Z.big_int **)

let rec maxlist = function
| [] -> Big_int_Z.zero_big_int
| h :: t -> (match t with
             | [] -> h
             | _ :: _ -> Z.max h (maxlist t))

(** val max_of_nonempty_list_type :
    'a1 list -> ('a1 -> 'a1 -> bool) -> Big_int_Z.big_int -> ('a1 ->
    Big_int_Z.big_int) -> ('a1, __) sigT **)

let max_of_nonempty_list_type l h1 s f =
  let rec f0 l0 h2 s0 f1 =
    match l0 with
    | [] -> assert false (* absurd case *)
    | h :: t ->
      (match t with
       | [] -> (fun _ -> ExistT (h, __))
       | h3 :: t1 ->
         let hmax = z_ge_lt_dec (f1 h) (maxlist (map f1 (h3 :: t1))) in
         (fun _ ->
         if hmax
         then ExistT (h, __)
         else let f2 = f0 t h2 s0 f1 __ in
              let ExistT (x, _) = f2 in ExistT (x, __)))
  in f0 l h1 s f __

type 'a finite = ('a list, __) sigT

(** val phi_one_helper :
    ('a1 -> 'a1 -> Big_int_Z.big_int) -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1
    -> bool option) -> 'a1 -> 'a1 -> bool **)

let phi_one_helper _ _ pdec x a =
  let s = pdec x a in
  (match s with
   | Some s0 ->
     if s0
     then let s1 = pdec a x in (match s1 with
                                | Some _ -> false
                                | None -> true)
     else false
   | None ->
     let s0 = pdec a x in (match s0 with
                           | Some s1 -> s1
                           | None -> false))

(** val phi_two_helper :
    ('a1 -> 'a1 -> Big_int_Z.big_int) -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1
    -> bool option) -> 'a1 -> 'a1 -> 'a1 -> bool **)

let phi_two_helper _ _ pdec a x a0' =
  let s = pdec a x in
  (match s with
   | Some s0 ->
     if s0
     then let s1 = pdec a0' x in
          (match s1 with
           | Some s2 ->
             if s2
             then let s3 = pdec x a in
                  (match s3 with
                   | Some s4 ->
                     if s4
                     then let s5 = pdec x a0' in
                          (match s5 with
                           | Some s6 -> s6
                           | None -> false)
                     else let s5 = pdec x a0' in
                          (match s5 with
                           | Some s6 -> if s6 then false else true
                           | None -> false)
                   | None ->
                     let s4 = pdec x a0' in
                     (match s4 with
                      | Some _ -> false
                      | None -> true))
             else false
           | None -> false)
     else let s1 = pdec a0' x in
          (match s1 with
           | Some s2 ->
             if s2
             then false
             else let s3 = pdec x a in
                  (match s3 with
                   | Some s4 ->
                     if s4
                     then let s5 = pdec x a0' in
                          (match s5 with
                           | Some s6 -> s6
                           | None -> false)
                     else let s5 = pdec x a0' in
                          (match s5 with
                           | Some s6 -> if s6 then false else true
                           | None -> false)
                   | None ->
                     let s4 = pdec x a0' in
                     (match s4 with
                      | Some _ -> false
                      | None -> true))
           | None -> false)
   | None ->
     let s0 = pdec a0' x in
     (match s0 with
      | Some _ -> false
      | None ->
        let s1 = pdec x a in
        (match s1 with
         | Some s2 ->
           if s2
           then let s3 = pdec x a0' in
                (match s3 with
                 | Some s4 -> s4
                 | None -> false)
           else let s3 = pdec x a0' in
                (match s3 with
                 | Some s4 -> if s4 then false else true
                 | None -> false)
         | None ->
           let s2 = pdec x a0' in
           (match s2 with
            | Some _ -> false
            | None -> true))))

(** val phi_two_inhanced :
    ('a1 -> 'a1 -> Big_int_Z.big_int) -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1
    -> bool option) -> 'a1 -> 'a1 list -> 'a1 -> bool **)

let rec phi_two_inhanced p adec pdec a l a0' =
  match l with
  | [] -> true
  | y :: l0 ->
    if phi_two_inhanced p adec pdec a l0 a0'
    then phi_two_helper p adec pdec a y a0'
    else false

(** val phi_one_dec :
    ('a1 -> 'a1 -> Big_int_Z.big_int) -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1
    -> bool option) -> 'a1 -> 'a1 list -> bool **)

let rec phi_one_dec p adec pdec a = function
| [] -> true
| y :: l0 ->
  if phi_one_dec p adec pdec a l0
  then phi_one_helper p adec pdec y a
  else false

(** val phi_two_dec :
    ('a1 -> 'a1 -> Big_int_Z.big_int) -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1
    -> bool option) -> 'a1 -> 'a1 list -> 'a1 list -> bool **)

let rec phi_two_dec p adec pdec a l1 l2 =
  match l1 with
  | [] -> false
  | y :: l ->
    if phi_two_dec p adec pdec a l l2
    then true
    else phi_two_inhanced p adec pdec a l2 y

(** val phi_decidable :
    ('a1 -> 'a1 -> Big_int_Z.big_int) -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1
    -> bool option) -> 'a1 -> 'a1 list -> bool **)

let phi_decidable p adec pdec a l =
  let s = phi_two_dec p adec pdec a l l in
  if s then true else phi_one_dec p adec pdec a l

(** val transitive_dec_first_fn :
    ('a1 -> 'a1 -> Big_int_Z.big_int) -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1
    -> bool option) -> 'a1 -> 'a1 -> 'a1 -> bool **)

let transitive_dec_first_fn _ _ pdec c d e =
  let s = pdec c d in
  (match s with
   | Some s0 ->
     if s0
     then let s1 = pdec d e in
          (match s1 with
           | Some _ ->
             let s2 = pdec c e in
             (match s2 with
              | Some s3 -> s3
              | None -> false)
           | None -> true)
     else let s1 = pdec d e in
          (match s1 with
           | Some s2 ->
             if s2
             then let s3 = pdec c e in
                  (match s3 with
                   | Some s4 -> s4
                   | None -> false)
             else let s3 = pdec c e in
                  (match s3 with
                   | Some s4 -> if s4 then false else true
                   | None -> false)
           | None ->
             let s2 = pdec c e in
             (match s2 with
              | Some _ -> false
              | None -> true))
   | None ->
     let s0 = pdec d e in
     (match s0 with
      | Some s1 ->
        if s1
        then true
        else let s2 = pdec c e in
             (match s2 with
              | Some _ -> false
              | None -> true)
      | None ->
        let s1 = pdec c e in (match s1 with
                              | Some _ -> false
                              | None -> true)))

(** val transitive_dec_second_fn :
    ('a1 -> 'a1 -> Big_int_Z.big_int) -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1
    -> bool option) -> 'a1 -> 'a1 -> 'a1 list -> bool **)

let rec transitive_dec_second_fn p adec pdec c d = function
| [] -> true
| y :: l0 ->
  if transitive_dec_second_fn p adec pdec c d l0
  then transitive_dec_first_fn p adec pdec c d y
  else false

(** val transitive_dec_third_fn :
    ('a1 -> 'a1 -> Big_int_Z.big_int) -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1
    -> bool option) -> 'a1 -> 'a1 list -> 'a1 list -> bool **)

let rec transitive_dec_third_fn p adec pdec c l1 l2 =
  match l1 with
  | [] -> true
  | y :: l ->
    if transitive_dec_third_fn p adec pdec c l l2
    then transitive_dec_second_fn p adec pdec c y l2
    else false

(** val transitive_dec_fourth_fn :
    ('a1 -> 'a1 -> Big_int_Z.big_int) -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1
    -> bool option) -> 'a1 list -> 'a1 list -> 'a1 list -> bool **)

let rec transitive_dec_fourth_fn p adec pdec l1 l2 l3 =
  match l1 with
  | [] -> true
  | y :: l ->
    if transitive_dec_fourth_fn p adec pdec l l2 l3
    then transitive_dec_third_fn p adec pdec y l2 l3
    else false

(** val transitive_dec_fn :
    ('a1 -> 'a1 -> Big_int_Z.big_int) -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1
    -> bool option) -> 'a1 list -> bool **)

let transitive_dec_fn p adec pdec l =
  transitive_dec_fourth_fn p adec pdec l l l

(** val vl_or_notvl :
    ('a1 -> 'a1 -> Big_int_Z.big_int) -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1
    -> bool option) -> 'a1 list -> (__, __) sum **)

let rec vl_or_notvl p adec pdec = function
| [] -> Inl __
| y :: l0 ->
  (match vl_or_notvl p adec pdec l0 with
   | Inl _ ->
     let h0 = pdec y y in
     (match h0 with
      | Some s ->
        if s
        then Inr __
        else let h1 = transitive_dec_fn p adec pdec (y :: l0) in
             if h1
             then let h2 = phi_decidable p adec pdec y l0 in
                  if h2 then Inl __ else Inr __
             else Inr __
      | None -> Inr __)
   | Inr _ -> Inr __)

(** val decidable_valid :
    ('a1 -> 'a1 -> Big_int_Z.big_int) -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1
    -> bool option) -> 'a1 finite -> bool **)

let decidable_valid p adec pdec = function
| ExistT (x, _) ->
  let h0 = vl_or_notvl p adec pdec x in
  (match h0 with
   | Inl _ -> true
   | Inr _ -> false)

type 'cand pathT =
| UnitT of 'cand * 'cand
| ConsT of 'cand * 'cand * 'cand * 'cand pathT

type 'cand wins_type =
  'cand -> (Big_int_Z.big_int, 'cand pathT * (('cand * 'cand) -> bool, __)
  sigT) sigT

type 'cand loses_type =
  (Big_int_Z.big_int, ('cand, 'cand pathT * (('cand * 'cand) -> bool, __)
  sigT) sigT) sigT

(** val listify :
    'a1 list -> ('a1 -> 'a1 -> Big_int_Z.big_int) ->
    (('a1 * 'a1) * Big_int_Z.big_int) list **)

let listify cand_all0 m0 =
  map (fun s -> (((fst s), (snd s)), (m0 (fst s) (snd s))))
    (all_pairs cand_all0)

(** val linear_search :
    ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1 -> Big_int_Z.big_int) -> 'a1 -> 'a1
    -> (('a1 * 'a1) * Big_int_Z.big_int) list -> Big_int_Z.big_int **)

let rec linear_search dec_cand marg c d = function
| [] -> marg c d
| y :: t ->
  let (y0, k) = y in
  let (c1, c2) = y0 in
  if dec_cand c c1
  then if dec_cand d c2 then k else linear_search dec_cand marg c d t
  else linear_search dec_cand marg c d t

(** val mM :
    'a1 list -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1 -> Big_int_Z.big_int) ->
    nat -> (('a1 * 'a1) * Big_int_Z.big_int) list **)

let rec mM cand_all0 dec_cand marg = function
| O -> listify cand_all0 marg
| S n' ->
  let uu = mM cand_all0 dec_cand marg n' in
  listify cand_all0 (fun c d ->
    let u = linear_search dec_cand marg c d uu in
    let t =
      maxlist
        (map (fun x -> Z.min (marg c x) (linear_search dec_cand marg x d uu))
          cand_all0)
    in
    Z.max u t)

(** val m :
    'a1 list -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1 -> Big_int_Z.big_int) ->
    nat -> 'a1 -> 'a1 -> Big_int_Z.big_int **)

let m cand_all0 dec_cand marg n =
  let l = mM cand_all0 dec_cand marg n in
  (fun c d -> linear_search dec_cand marg c d l)

(** val iterated_marg_patht :
    'a1 list -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1 -> Big_int_Z.big_int) ->
    nat -> Big_int_Z.big_int -> 'a1 -> 'a1 -> 'a1 pathT **)

let rec iterated_marg_patht cand_all0 dec_cand marg n s c d =
  match n with
  | O -> UnitT (c, d)
  | S n0 ->
    let c0 =
      Z.compare
        (linear_search dec_cand marg c d (mM cand_all0 dec_cand marg n0))
        (maxlist
          (map (fun x ->
            Z.min (marg c x)
              (linear_search dec_cand marg x d
                (mM cand_all0 dec_cand marg n0))) cand_all0))
    in
    (match c0 with
     | Lt ->
       let h =
         max_of_nonempty_list_type cand_all0 dec_cand s (fun x ->
           Z.min (marg c x)
             (linear_search dec_cand marg x d (mM cand_all0 dec_cand marg n0)))
       in
       let ExistT (x, _) = h in
       let iHn = iterated_marg_patht cand_all0 dec_cand marg n0 s x d in
       ConsT (c, x, d, iHn)
     | _ -> iterated_marg_patht cand_all0 dec_cand marg n0 s c d)

(** val c_wins :
    'a1 list -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1 -> Big_int_Z.big_int) ->
    'a1 -> bool **)

let c_wins cand_all0 dec_cand marg c =
  forallb (fun d ->
    Z.leb (m cand_all0 dec_cand marg (length cand_all0) d c)
      (m cand_all0 dec_cand marg (length cand_all0) c d)) cand_all0

(** val iterated_marg_wins_type :
    'a1 list -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1 -> Big_int_Z.big_int) ->
    'a1 -> 'a1 wins_type **)

let iterated_marg_wins_type cand_all0 dec_cand marg c d =
  let s = m cand_all0 dec_cand marg (length cand_all0) c d in
  ExistT (s,
  (let hi =
     iterated_marg_patht cand_all0 dec_cand marg (length cand_all0) s c d
   in
   (hi,
   (let r = m cand_all0 dec_cand marg (length cand_all0) d c in
    ExistT ((fun x ->
    Z.leb (m cand_all0 dec_cand marg (length cand_all0) (fst x) (snd x)) r),
    __)))))

(** val exists_fin_reify : ('a1 -> bool) -> 'a1 list -> ('a1, __) sigT **)

let rec exists_fin_reify pdec = function
| [] -> assert false (* absurd case *)
| h :: t -> if pdec h then ExistT (h, __) else exists_fin_reify pdec t

(** val reify_opponent :
    'a1 list -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1 -> Big_int_Z.big_int) ->
    'a1 -> ('a1, __) sigT **)

let reify_opponent cand_all0 dec_cand marg c =
  let hdec = fun d ->
    let s =
      z_lt_ge_bool (m cand_all0 dec_cand marg (length cand_all0) c d)
        (m cand_all0 dec_cand marg (length cand_all0) d c)
    in
    if s then true else false
  in
  exists_fin_reify hdec cand_all0

(** val iterated_marg_loses_type :
    'a1 list -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1 -> Big_int_Z.big_int) ->
    'a1 -> 'a1 loses_type **)

let iterated_marg_loses_type cand_all0 dec_cand marg c =
  let hE = reify_opponent cand_all0 dec_cand marg c in
  let ExistT (x, _) = hE in
  let s = m cand_all0 dec_cand marg (length cand_all0) x c in
  ExistT (s, (ExistT (x,
  ((iterated_marg_patht cand_all0 dec_cand marg (length cand_all0) s x c),
  (ExistT ((fun x0 ->
  Z.ltb (m cand_all0 dec_cand marg (length cand_all0) (fst x0) (snd x0)) s),
  __))))))

(** val wins_loses_type_dec :
    'a1 list -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1 -> Big_int_Z.big_int) ->
    'a1 -> ('a1 wins_type, 'a1 loses_type) sum **)

let wins_loses_type_dec cand_all0 dec_cand marg c =
  let b = c_wins cand_all0 dec_cand marg c in
  if b
  then Inl (iterated_marg_wins_type cand_all0 dec_cand marg c)
  else Inr (iterated_marg_loses_type cand_all0 dec_cand marg c)

type plaintext = Big_int_Z.big_int

type 'cand pballot = 'cand -> 'cand -> plaintext

type ('cand, 'ciphertext) eballot = 'cand -> 'cand -> 'ciphertext

type ('prime, 'generator, 'pubkey) group =
| Group of 'prime * 'generator * 'pubkey

type 'cand permutation = ('cand -> 'cand, __) sigT

(** val pair_cand_dec :
    ('a1 -> 'a1 -> bool) -> ('a1 * 'a1) -> ('a1 * 'a1) -> bool **)

let pair_cand_dec dec_cand c d =
  let (c0, c1) = c in
  let (c2, c3) = d in
  let h = dec_cand c0 c2 in let h0 = dec_cand c1 c3 in if h then h0 else false

(** val partition_integer : Big_int_Z.big_int -> bool option option **)

let partition_integer b =
  let s =
    z_le_dec b (Big_int_Z.minus_big_int (Big_int_Z.mult_int_big_int 2
      Big_int_Z.unit_big_int))
  in
  if s
  then None
  else let s0 =
         z_ge_dec b (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int)
       in
       if s0
       then None
       else Some
              (let s1 =
                 Z.eq_dec b (Big_int_Z.minus_big_int Big_int_Z.unit_big_int)
               in
               if s1
               then Some true
               else let s2 = Z.eq_dec b Big_int_Z.zero_big_int in
                    if s2 then Some false else None)

(** val finite_gen :
    ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1 -> Big_int_Z.big_int) -> ('a1 * 'a1)
    list -> ('a1 -> 'a1 -> __ -> bool option, __) sum **)

let rec finite_gen dec_cand b = function
| [] -> Inl (fun _ _ _ -> assert false (* absurd case *))
| y :: l0 ->
  (match finite_gen dec_cand b l0 with
   | Inl s ->
     let (c, c0) = y in
     let s0 = partition_integer (b c c0) in
     (match s0 with
      | Some s1 ->
        Inl (fun c1 d _ ->
          let s2 = pair_cand_dec dec_cand (c1, d) (c, c0) in
          if s2 then s1 else s c1 d __)
      | None -> Inr __)
   | Inr _ -> Inr __)

(** val finiteness :
    ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1 -> Big_int_Z.big_int) -> ('a1 * 'a1)
    list -> ('a1 -> 'a1 -> bool option, __) sum **)

let finiteness dec_cand b l =
  let x = finite_gen dec_cand b l in
  (match x with
   | Inl s -> Inl (fun c d -> s c d __)
   | Inr _ -> Inr __)

(** val dec_pballot :
    'a1 list -> ('a1 -> 'a1 -> bool) -> 'a1 pballot -> bool **)

let dec_pballot cand_all0 dec_cand p =
  let x = finiteness dec_cand p (all_pairs cand_all0) in
  (match x with
   | Inl _ -> true
   | Inr _ -> false)

(** val pballot_valid_dec :
    'a1 list -> ('a1 -> 'a1 -> bool) -> 'a1 pballot -> bool **)

let pballot_valid_dec cand_all0 dec_cand b =
  let x = decidable_valid b dec_cand in
  let ht = finiteness dec_cand b (all_pairs cand_all0) in
  (match ht with
   | Inl s -> x s (ExistT (cand_all0, __))
   | Inr _ -> false)

(** val matrix_ballot_valid_dec :
    'a1 list -> ('a1 -> 'a1 -> bool) -> 'a3 -> 'a4 -> 'a6 -> 'a5 -> (('a3,
    'a4, 'a5) group -> plaintext -> 'a2) -> (('a3, 'a4, 'a5) group -> 'a6 ->
    'a2 -> plaintext) -> (('a3, 'a4, 'a5) group -> 'a6 -> 'a2 -> 'a7) ->
    (('a3, 'a4, 'a5) group -> nat -> 'a1 list -> 'a1 permutation) -> (('a3,
    'a4, 'a5) group -> nat -> 'a10) -> (('a3, 'a4, 'a5) group -> nat -> 'a1
    list -> 'a1 permutation -> 'a10 -> 'a8) -> (('a3, 'a4, 'a5) group -> nat
    -> 'a1 list -> 'a1 permutation -> 'a8 -> 'a10 -> 'a9) -> (('a3, 'a4, 'a5)
    group -> 'a2 -> 'a2 -> 'a2) -> (('a3, 'a4, 'a5) group -> nat -> 'a11) ->
    (('a3, 'a4, 'a5) group -> nat -> 'a1 list -> ('a1 -> 'a1 -> bool) -> ('a1
    -> 'a2) -> 'a1 permutation -> 'a11 -> 'a1 -> 'a2) -> (('a3, 'a4, 'a5)
    group -> nat -> 'a1 list -> ('a1 -> 'a2) -> ('a1 -> 'a2) -> 'a1
    permutation -> 'a8 -> 'a10 -> 'a11 -> 'a12) -> 'a1 pballot -> bool **)

let matrix_ballot_valid_dec cand_all0 dec_cand _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ p =
  let s = dec_pballot cand_all0 dec_cand p in
  if s then pballot_valid_dec cand_all0 dec_cand p else false

type ('cand, 'ciphertext, 'prime, 'generator, 'pubkey, 'decZkp, 'commitment,
      'permZkp, 'shuffleZkp) eCount =
| Ecax of ('cand, 'ciphertext) eballot list * ('cand -> 'cand -> 'ciphertext)
   * ('cand -> 'cand -> plaintext) * ('cand -> 'cand -> 'decZkp)
| Ecvalid of ('cand, 'ciphertext) eballot * ('cand, 'ciphertext) eballot
   * ('cand, 'ciphertext) eballot * 'cand pballot * ('cand -> 'shuffleZkp)
   * ('cand -> 'shuffleZkp) * ('cand -> 'cand -> 'decZkp) * 'commitment
   * 'permZkp * ('cand, 'ciphertext) eballot list
   * ('cand -> 'cand -> 'ciphertext) * ('cand -> 'cand -> 'ciphertext)
   * ('cand, 'ciphertext) eballot list
   * ('cand, 'ciphertext, 'prime, 'generator, 'pubkey, 'decZkp, 'commitment,
     'permZkp, 'shuffleZkp) eCount
| Ecinvalid of ('cand, 'ciphertext) eballot * ('cand, 'ciphertext) eballot
   * ('cand, 'ciphertext) eballot * 'cand pballot * ('cand -> 'shuffleZkp)
   * ('cand -> 'shuffleZkp) * ('cand -> 'cand -> 'decZkp) * 'commitment
   * 'permZkp * ('cand, 'ciphertext) eballot list
   * ('cand -> 'cand -> 'ciphertext) * ('cand, 'ciphertext) eballot list
   * ('cand, 'ciphertext, 'prime, 'generator, 'pubkey, 'decZkp, 'commitment,
     'permZkp, 'shuffleZkp) eCount
| Ecdecrypt of ('cand, 'ciphertext) eballot list
   * ('cand -> 'cand -> 'ciphertext) * ('cand -> 'cand -> plaintext)
   * ('cand -> 'cand -> 'decZkp)
   * ('cand, 'ciphertext, 'prime, 'generator, 'pubkey, 'decZkp, 'commitment,
     'permZkp, 'shuffleZkp) eCount
| Ecfin of ('cand -> 'cand -> Big_int_Z.big_int) * ('cand -> bool)
   * ('cand -> ('cand wins_type, 'cand loses_type) sum)
   * ('cand, 'ciphertext, 'prime, 'generator, 'pubkey, 'decZkp, 'commitment,
     'permZkp, 'shuffleZkp) eCount

(** val ecount_all_ballot :
    'a1 list -> 'a3 -> 'a4 -> 'a6 -> 'a5 -> (('a3, 'a4, 'a5) group ->
    plaintext -> 'a2) -> (('a3, 'a4, 'a5) group -> 'a6 -> 'a2 -> plaintext)
    -> (('a3, 'a4, 'a5) group -> 'a6 -> 'a2 -> 'a7) -> (('a3, 'a4, 'a5) group
    -> 'a2 -> 'a2 -> 'a2) -> ('a1, 'a2) eballot list -> ('a1 -> 'a1 -> 'a2,
    ('a1, 'a2, 'a3, 'a4, 'a5, 'a7, 'a8, 'a9, 'a10) eCount) sigT **)

let ecount_all_ballot _ prime gen privatekey publickey encrypt_message _ construct_zero_knowledge_decryption_proof _ bs =
  let grp = Group (prime, gen, publickey) in
  let encm = fun _ _ -> encrypt_message grp Big_int_Z.zero_big_int in
  ExistT (encm, (Ecax (bs, encm, (fun _ _ -> Big_int_Z.zero_big_int),
  (fun c d ->
  construct_zero_knowledge_decryption_proof grp privatekey (encm c d)))))

(** val idx_search_list :
    ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> 'a2 list -> 'a2 **)

let rec idx_search_list dec_cand c cl l =
  match cl with
  | [] -> assert false (* absurd case *)
  | c0 :: cs ->
    (match l with
     | [] -> assert false (* absurd case *)
     | a :: t -> if dec_cand c c0 then a else idx_search_list dec_cand c cs t)

(** val ppartial_count_all_counted :
    'a1 list -> ('a1 -> 'a1 -> bool) -> 'a3 -> 'a4 -> 'a6 -> 'a5 -> (('a3,
    'a4, 'a5) group -> plaintext -> 'a2) -> (('a3, 'a4, 'a5) group -> 'a6 ->
    'a2 -> plaintext) -> (('a3, 'a4, 'a5) group -> 'a6 -> 'a2 -> 'a7) ->
    (('a3, 'a4, 'a5) group -> nat -> 'a1 list -> 'a1 permutation) -> (('a3,
    'a4, 'a5) group -> nat -> 'a10) -> (('a3, 'a4, 'a5) group -> nat -> 'a1
    list -> 'a1 permutation -> 'a10 -> 'a8) -> (('a3, 'a4, 'a5) group -> nat
    -> 'a1 list -> 'a1 permutation -> 'a8 -> 'a10 -> 'a9) -> (('a3, 'a4, 'a5)
    group -> 'a2 -> 'a2 -> 'a2) -> (('a3, 'a4, 'a5) group -> nat -> 'a11) ->
    (('a3, 'a4, 'a5) group -> nat -> 'a1 list -> ('a1 -> 'a1 -> bool) -> ('a1
    -> 'a2) -> 'a1 permutation -> 'a11 -> 'a1 -> 'a2) -> (('a3, 'a4, 'a5)
    group -> nat -> 'a1 list -> ('a1 -> 'a2) -> ('a1 -> 'a2) -> 'a1
    permutation -> 'a8 -> 'a10 -> 'a11 -> 'a12) -> ('a1, 'a2) eballot list ->
    ('a1, 'a2) eballot list -> ('a1, 'a2) eballot list -> ('a1 -> 'a1 -> 'a2)
    -> ('a1, 'a2, 'a3, 'a4, 'a5, 'a7, 'a8, 'a9, 'a12) eCount -> (('a1, 'a2)
    eballot list, ('a1 -> 'a1 -> 'a2, ('a1, 'a2, 'a3, 'a4, 'a5, 'a7, 'a8,
    'a9, 'a12) eCount) sigT) sigT **)

let ppartial_count_all_counted cand_all0 dec_cand prime gen privatekey publickey encrypt_message decrypt_message construct_zero_knowledge_decryption_proof generatePermutation generateS generatePermutationCommitment zkpPermutationCommitment homomorphic_addition generateR shuffle shuffle_zkp _ ts inbs m0 x =
  let grp = Group (prime, gen, publickey) in
  let rec f l inbs0 m1 he =
    match l with
    | [] -> ExistT (inbs0, (ExistT (m1, he)))
    | y :: l0 ->
      let pi = generatePermutation grp (length cand_all0) cand_all0 in
      let s = generateS grp (length cand_all0) in
      let cpi =
        generatePermutationCommitment grp (length cand_all0) cand_all0 pi s
      in
      let zkpcpi =
        zkpPermutationCommitment grp (length cand_all0) cand_all0 pi cpi s
      in
      let rrowlistvalues =
        map (fun _ -> generateR grp (length cand_all0)) cand_all0
      in
      let rrowfunvalues = fun c ->
        idx_search_list dec_cand c cand_all0 rrowlistvalues
      in
      let v = fun c ->
        shuffle grp (length cand_all0) cand_all0 dec_cand (y c) pi
          (rrowfunvalues c)
      in
      let zkppermuv = fun c ->
        shuffle_zkp grp (length cand_all0) cand_all0 (y c) (v c) pi cpi s
          (rrowfunvalues c)
      in
      let rcollistvalues =
        map (fun _ -> generateR grp (length cand_all0)) cand_all0
      in
      let rcolfunvalues = fun c ->
        idx_search_list dec_cand c cand_all0 rcollistvalues
      in
      let t = fun c ->
        shuffle grp (length cand_all0) cand_all0 dec_cand (fun d -> v d c) pi
          (rcolfunvalues c)
      in
      let w = fun c d -> t d c in
      let zkppermvw = fun c ->
        shuffle_zkp grp (length cand_all0) cand_all0 (fun d -> v d c)
          (fun d -> w d c) pi cpi s (rcolfunvalues c)
      in
      let b = fun c d -> decrypt_message grp privatekey (w c d) in
      let zkpdecw = fun c d ->
        construct_zero_knowledge_decryption_proof grp privatekey (w c d)
      in
      let s0 =
        matrix_ballot_valid_dec cand_all0 dec_cand prime gen privatekey
          publickey encrypt_message decrypt_message
          construct_zero_knowledge_decryption_proof generatePermutation
          generateS generatePermutationCommitment zkpPermutationCommitment
          homomorphic_addition generateR shuffle shuffle_zkp b
      in
      if s0
      then let nm = fun c d -> homomorphic_addition grp (y c d) (m1 c d) in
           let x0 = Ecvalid (y, v, w, b, zkppermuv, zkppermvw, zkpdecw, cpi,
             zkpcpi, l0, m1, nm, inbs0, he)
           in
           f l0 inbs0 nm x0
      else let x0 = Ecinvalid (y, v, w, b, zkppermuv, zkppermvw, zkpdecw,
             cpi, zkpcpi, l0, m1, inbs0, he)
           in
           f l0 (y :: inbs0) m1 x0
  in f ts inbs m0 x

(** val pall_ballots_counted :
    'a1 list -> ('a1 -> 'a1 -> bool) -> 'a3 -> 'a4 -> 'a6 -> 'a5 -> (('a3,
    'a4, 'a5) group -> plaintext -> 'a2) -> (('a3, 'a4, 'a5) group -> 'a6 ->
    'a2 -> plaintext) -> (('a3, 'a4, 'a5) group -> 'a6 -> 'a2 -> 'a7) ->
    (('a3, 'a4, 'a5) group -> nat -> 'a1 list -> 'a1 permutation) -> (('a3,
    'a4, 'a5) group -> nat -> 'a10) -> (('a3, 'a4, 'a5) group -> nat -> 'a1
    list -> 'a1 permutation -> 'a10 -> 'a8) -> (('a3, 'a4, 'a5) group -> nat
    -> 'a1 list -> 'a1 permutation -> 'a8 -> 'a10 -> 'a9) -> (('a3, 'a4, 'a5)
    group -> 'a2 -> 'a2 -> 'a2) -> (('a3, 'a4, 'a5) group -> nat -> 'a11) ->
    (('a3, 'a4, 'a5) group -> nat -> 'a1 list -> ('a1 -> 'a1 -> bool) -> ('a1
    -> 'a2) -> 'a1 permutation -> 'a11 -> 'a1 -> 'a2) -> (('a3, 'a4, 'a5)
    group -> nat -> 'a1 list -> ('a1 -> 'a2) -> ('a1 -> 'a2) -> 'a1
    permutation -> 'a8 -> 'a10 -> 'a11 -> 'a12) -> ('a1, 'a2) eballot list ->
    (('a1, 'a2) eballot list, ('a1 -> 'a1 -> 'a2, ('a1, 'a2, 'a3, 'a4, 'a5,
    'a7, 'a8, 'a9, 'a12) eCount) sigT) sigT **)

let pall_ballots_counted cand_all0 dec_cand prime gen privatekey publickey encrypt_message decrypt_message construct_zero_knowledge_decryption_proof generatePermutation generateS generatePermutationCommitment zkpPermutationCommitment homomorphic_addition generateR shuffle shuffle_zkp bs =
  let hs =
    ecount_all_ballot cand_all0 prime gen privatekey publickey
      encrypt_message decrypt_message
      construct_zero_knowledge_decryption_proof homomorphic_addition bs
  in
  let ExistT (x, e) = hs in
  ppartial_count_all_counted cand_all0 dec_cand prime gen privatekey
    publickey encrypt_message decrypt_message
    construct_zero_knowledge_decryption_proof generatePermutation generateS
    generatePermutationCommitment zkpPermutationCommitment
    homomorphic_addition generateR shuffle shuffle_zkp bs bs [] x e

(** val decrypt_margin :
    'a1 list -> ('a1 -> 'a1 -> bool) -> 'a3 -> 'a4 -> 'a6 -> 'a5 -> (('a3,
    'a4, 'a5) group -> plaintext -> 'a2) -> (('a3, 'a4, 'a5) group -> 'a6 ->
    'a2 -> plaintext) -> (('a3, 'a4, 'a5) group -> 'a6 -> 'a2 -> 'a7) ->
    (('a3, 'a4, 'a5) group -> nat -> 'a1 list -> 'a1 permutation) -> (('a3,
    'a4, 'a5) group -> nat -> 'a10) -> (('a3, 'a4, 'a5) group -> nat -> 'a1
    list -> 'a1 permutation -> 'a10 -> 'a8) -> (('a3, 'a4, 'a5) group -> nat
    -> 'a1 list -> 'a1 permutation -> 'a8 -> 'a10 -> 'a9) -> (('a3, 'a4, 'a5)
    group -> 'a2 -> 'a2 -> 'a2) -> (('a3, 'a4, 'a5) group -> nat -> 'a11) ->
    (('a3, 'a4, 'a5) group -> nat -> 'a1 list -> ('a1 -> 'a1 -> bool) -> ('a1
    -> 'a2) -> 'a1 permutation -> 'a11 -> 'a1 -> 'a2) -> (('a3, 'a4, 'a5)
    group -> nat -> 'a1 list -> ('a1 -> 'a2) -> ('a1 -> 'a2) -> 'a1
    permutation -> 'a8 -> 'a10 -> 'a11 -> 'a12) -> ('a1, 'a2) eballot list ->
    ('a1 -> 'a1 -> plaintext, ('a1, 'a2, 'a3, 'a4, 'a5, 'a7, 'a8, 'a9, 'a12)
    eCount) sigT **)

let decrypt_margin cand_all0 dec_cand prime gen privatekey publickey encrypt_message decrypt_message construct_zero_knowledge_decryption_proof generatePermutation generateS generatePermutationCommitment zkpPermutationCommitment homomorphic_addition generateR shuffle shuffle_zkp bs =
  let grp = Group (prime, gen, publickey) in
  let hc =
    pall_ballots_counted cand_all0 dec_cand prime gen privatekey publickey
      encrypt_message decrypt_message
      construct_zero_knowledge_decryption_proof generatePermutation generateS
      generatePermutationCommitment zkpPermutationCommitment
      homomorphic_addition generateR shuffle shuffle_zkp bs
  in
  let ExistT (x, s) = hc in
  let ExistT (x0, e) = s in
  let decm = fun c d -> decrypt_message grp privatekey (x0 c d) in
  let zkpdecm = fun c d ->
    construct_zero_knowledge_decryption_proof grp privatekey (x0 c d)
  in
  ExistT (decm, (Ecdecrypt (x, x0, decm, zkpdecm, e)))

(** val pschulze_winners :
    'a1 list -> ('a1 -> 'a1 -> bool) -> 'a3 -> 'a4 -> 'a6 -> 'a5 -> (('a3,
    'a4, 'a5) group -> plaintext -> 'a2) -> (('a3, 'a4, 'a5) group -> 'a6 ->
    'a2 -> plaintext) -> (('a3, 'a4, 'a5) group -> 'a6 -> 'a2 -> 'a7) ->
    (('a3, 'a4, 'a5) group -> nat -> 'a1 list -> 'a1 permutation) -> (('a3,
    'a4, 'a5) group -> nat -> 'a10) -> (('a3, 'a4, 'a5) group -> nat -> 'a1
    list -> 'a1 permutation -> 'a10 -> 'a8) -> (('a3, 'a4, 'a5) group -> nat
    -> 'a1 list -> 'a1 permutation -> 'a8 -> 'a10 -> 'a9) -> (('a3, 'a4, 'a5)
    group -> 'a2 -> 'a2 -> 'a2) -> (('a3, 'a4, 'a5) group -> nat -> 'a11) ->
    (('a3, 'a4, 'a5) group -> nat -> 'a1 list -> ('a1 -> 'a1 -> bool) -> ('a1
    -> 'a2) -> 'a1 permutation -> 'a11 -> 'a1 -> 'a2) -> (('a3, 'a4, 'a5)
    group -> nat -> 'a1 list -> ('a1 -> 'a2) -> ('a1 -> 'a2) -> 'a1
    permutation -> 'a8 -> 'a10 -> 'a11 -> 'a12) -> ('a1, 'a2) eballot list ->
    ('a1 -> bool, ('a1, 'a2, 'a3, 'a4, 'a5, 'a7, 'a8, 'a9, 'a12) eCount) sigT **)

let pschulze_winners cand_all0 dec_cand prime gen privatekey publickey encrypt_message decrypt_message construct_zero_knowledge_decryption_proof generatePermutation generateS generatePermutationCommitment zkpPermutationCommitment homomorphic_addition generateR shuffle shuffle_zkp bs =
  let s =
    decrypt_margin cand_all0 dec_cand prime gen privatekey publickey
      encrypt_message decrypt_message
      construct_zero_knowledge_decryption_proof generatePermutation generateS
      generatePermutationCommitment zkpPermutationCommitment
      homomorphic_addition generateR shuffle shuffle_zkp bs
  in
  let ExistT (x, e) = s in
  ExistT ((c_wins cand_all0 dec_cand x), (Ecfin (x,
  (c_wins cand_all0 dec_cand x), (wins_loses_type_dec cand_all0 dec_cand x),
  e)))

type cand =
| A
| B
| C

(** val cand_all : cand list **)

let cand_all =
  A :: (B :: (C :: []))

(** val cand_eq_dec : cand -> cand -> bool **)

let cand_eq_dec a b =
  match a with
  | A -> (match b with
          | A -> true
          | _ -> false)
  | B -> (match b with
          | B -> true
          | _ -> false)
  | C -> (match b with
          | C -> true
          | _ -> false)

(** val eschulze_winners_pf :
    'a2 -> 'a3 -> 'a5 -> 'a4 -> (('a2, 'a3, 'a4) group -> plaintext -> 'a1)
    -> (('a2, 'a3, 'a4) group -> 'a5 -> 'a1 -> plaintext) -> (('a2, 'a3, 'a4)
    group -> 'a5 -> 'a1 -> 'a6) -> (('a2, 'a3, 'a4) group -> nat -> cand list
    -> cand permutation) -> (('a2, 'a3, 'a4) group -> nat -> 'a9) -> (('a2,
    'a3, 'a4) group -> nat -> cand list -> cand permutation -> 'a9 -> 'a7) ->
    (('a2, 'a3, 'a4) group -> nat -> cand list -> cand permutation -> 'a7 ->
    'a9 -> 'a8) -> (('a2, 'a3, 'a4) group -> 'a1 -> 'a1 -> 'a1) -> (('a2,
    'a3, 'a4) group -> nat -> 'a10) -> (('a2, 'a3, 'a4) group -> nat -> cand
    list -> (cand -> cand -> bool) -> (cand -> 'a1) -> cand permutation ->
    'a10 -> cand -> 'a1) -> (('a2, 'a3, 'a4) group -> nat -> cand list ->
    (cand -> 'a1) -> (cand -> 'a1) -> cand permutation -> 'a7 -> 'a9 -> 'a10
    -> 'a11) -> (cand, 'a1) eballot list -> (cand -> bool, (cand, 'a1, 'a2,
    'a3, 'a4, 'a6, 'a7, 'a8, 'a11) eCount) sigT **)

let eschulze_winners_pf prime gen privatekey publickey encrypt_message decrypt_message construct_zero_knowledge_decryption_proof generatePermutation generateS generatePermutationCommitment zkpPermutationCommitment homomorphic_addition generateR shuffle shuffle_zkp bs =
  pschulze_winners cand_all cand_eq_dec prime gen privatekey publickey
    encrypt_message decrypt_message construct_zero_knowledge_decryption_proof
    generatePermutation generateS generatePermutationCommitment
    zkpPermutationCommitment homomorphic_addition generateR shuffle
    shuffle_zkp bs
