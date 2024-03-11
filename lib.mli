
type __ = Obj.t

type nat =
| O
| S of nat

type ('a, 'b) sum =
| Inl of 'a
| Inr of 'b

val fst : ('a1 * 'a2) -> 'a1

val snd : ('a1 * 'a2) -> 'a2

val length : 'a1 list -> nat

val app : 'a1 list -> 'a1 list -> 'a1 list

type comparison =
| Eq
| Lt
| Gt

type 'a sig0 = 'a
  (* singleton inductive, whose constructor was exist *)

type ('a, 'p) sigT =
| ExistT of 'a * 'p



module Pos :
 sig
  val compare_cont :
    comparison -> Big_int_Z.big_int -> Big_int_Z.big_int -> comparison

  val compare : Big_int_Z.big_int -> Big_int_Z.big_int -> comparison

  val eq_dec : Big_int_Z.big_int -> Big_int_Z.big_int -> bool
 end

val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list

val forallb : ('a1 -> bool) -> 'a1 list -> bool

module Z :
 sig
  val compare : Big_int_Z.big_int -> Big_int_Z.big_int -> comparison

  val leb : Big_int_Z.big_int -> Big_int_Z.big_int -> bool

  val ltb : Big_int_Z.big_int -> Big_int_Z.big_int -> bool

  val max : Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int

  val min : Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int

  val eq_dec : Big_int_Z.big_int -> Big_int_Z.big_int -> bool
 end

val bool_of_sumbool : bool -> bool

val z_lt_dec : Big_int_Z.big_int -> Big_int_Z.big_int -> bool

val z_le_dec : Big_int_Z.big_int -> Big_int_Z.big_int -> bool

val z_ge_dec : Big_int_Z.big_int -> Big_int_Z.big_int -> bool

val z_lt_ge_dec : Big_int_Z.big_int -> Big_int_Z.big_int -> bool

val z_ge_lt_dec : Big_int_Z.big_int -> Big_int_Z.big_int -> bool

val z_lt_ge_bool : Big_int_Z.big_int -> Big_int_Z.big_int -> bool

val all_pairs : 'a1 list -> ('a1 * 'a1) list

val maxlist : Big_int_Z.big_int list -> Big_int_Z.big_int

val max_of_nonempty_list_type :
  'a1 list -> ('a1 -> 'a1 -> bool) -> Big_int_Z.big_int -> ('a1 ->
  Big_int_Z.big_int) -> ('a1, __) sigT

type 'a finite = ('a list, __) sigT

val phi_one_helper :
  ('a1 -> 'a1 -> Big_int_Z.big_int) -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1 ->
  bool option) -> 'a1 -> 'a1 -> bool

val phi_two_helper :
  ('a1 -> 'a1 -> Big_int_Z.big_int) -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1 ->
  bool option) -> 'a1 -> 'a1 -> 'a1 -> bool

val phi_two_inhanced :
  ('a1 -> 'a1 -> Big_int_Z.big_int) -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1 ->
  bool option) -> 'a1 -> 'a1 list -> 'a1 -> bool

val phi_one_dec :
  ('a1 -> 'a1 -> Big_int_Z.big_int) -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1 ->
  bool option) -> 'a1 -> 'a1 list -> bool

val phi_two_dec :
  ('a1 -> 'a1 -> Big_int_Z.big_int) -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1 ->
  bool option) -> 'a1 -> 'a1 list -> 'a1 list -> bool

val phi_decidable :
  ('a1 -> 'a1 -> Big_int_Z.big_int) -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1 ->
  bool option) -> 'a1 -> 'a1 list -> bool

val transitive_dec_first_fn :
  ('a1 -> 'a1 -> Big_int_Z.big_int) -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1 ->
  bool option) -> 'a1 -> 'a1 -> 'a1 -> bool

val transitive_dec_second_fn :
  ('a1 -> 'a1 -> Big_int_Z.big_int) -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1 ->
  bool option) -> 'a1 -> 'a1 -> 'a1 list -> bool

val transitive_dec_third_fn :
  ('a1 -> 'a1 -> Big_int_Z.big_int) -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1 ->
  bool option) -> 'a1 -> 'a1 list -> 'a1 list -> bool

val transitive_dec_fourth_fn :
  ('a1 -> 'a1 -> Big_int_Z.big_int) -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1 ->
  bool option) -> 'a1 list -> 'a1 list -> 'a1 list -> bool

val transitive_dec_fn :
  ('a1 -> 'a1 -> Big_int_Z.big_int) -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1 ->
  bool option) -> 'a1 list -> bool

val vl_or_notvl :
  ('a1 -> 'a1 -> Big_int_Z.big_int) -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1 ->
  bool option) -> 'a1 list -> (__, __) sum

val decidable_valid :
  ('a1 -> 'a1 -> Big_int_Z.big_int) -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1 ->
  bool option) -> 'a1 finite -> bool

type 'cand pathT =
| UnitT of 'cand * 'cand
| ConsT of 'cand * 'cand * 'cand * 'cand pathT

type 'cand wins_type =
  'cand -> (Big_int_Z.big_int, 'cand pathT * (('cand * 'cand) -> bool, __)
  sigT) sigT

type 'cand loses_type =
  (Big_int_Z.big_int, ('cand, 'cand pathT * (('cand * 'cand) -> bool, __)
  sigT) sigT) sigT

val listify :
  'a1 list -> ('a1 -> 'a1 -> Big_int_Z.big_int) ->
  (('a1 * 'a1) * Big_int_Z.big_int) list

val linear_search :
  ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1 -> Big_int_Z.big_int) -> 'a1 -> 'a1 ->
  (('a1 * 'a1) * Big_int_Z.big_int) list -> Big_int_Z.big_int

val mM :
  'a1 list -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1 -> Big_int_Z.big_int) ->
  nat -> (('a1 * 'a1) * Big_int_Z.big_int) list

val m :
  'a1 list -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1 -> Big_int_Z.big_int) ->
  nat -> 'a1 -> 'a1 -> Big_int_Z.big_int

val iterated_marg_patht :
  'a1 list -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1 -> Big_int_Z.big_int) ->
  nat -> Big_int_Z.big_int -> 'a1 -> 'a1 -> 'a1 pathT

val c_wins :
  'a1 list -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1 -> Big_int_Z.big_int) ->
  'a1 -> bool

val iterated_marg_wins_type :
  'a1 list -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1 -> Big_int_Z.big_int) ->
  'a1 -> 'a1 wins_type

val exists_fin_reify : ('a1 -> bool) -> 'a1 list -> ('a1, __) sigT

val reify_opponent :
  'a1 list -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1 -> Big_int_Z.big_int) ->
  'a1 -> ('a1, __) sigT

val iterated_marg_loses_type :
  'a1 list -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1 -> Big_int_Z.big_int) ->
  'a1 -> 'a1 loses_type

val wins_loses_type_dec :
  'a1 list -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1 -> Big_int_Z.big_int) ->
  'a1 -> ('a1 wins_type, 'a1 loses_type) sum

type plaintext = Big_int_Z.big_int

type 'cand pballot = 'cand -> 'cand -> plaintext

type ('cand, 'ciphertext) eballot = 'cand -> 'cand -> 'ciphertext

type ('prime, 'generator, 'pubkey) group =
| Group of 'prime * 'generator * 'pubkey

type 'cand permutation = ('cand -> 'cand, __) sigT

val pair_cand_dec : ('a1 -> 'a1 -> bool) -> ('a1 * 'a1) -> ('a1 * 'a1) -> bool

val partition_integer : Big_int_Z.big_int -> bool option option

val finite_gen :
  ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1 -> Big_int_Z.big_int) -> ('a1 * 'a1)
  list -> ('a1 -> 'a1 -> __ -> bool option, __) sum

val finiteness :
  ('a1 -> 'a1 -> bool) -> ('a1 -> 'a1 -> Big_int_Z.big_int) -> ('a1 * 'a1)
  list -> ('a1 -> 'a1 -> bool option, __) sum

val dec_pballot : 'a1 list -> ('a1 -> 'a1 -> bool) -> 'a1 pballot -> bool

val pballot_valid_dec :
  'a1 list -> ('a1 -> 'a1 -> bool) -> 'a1 pballot -> bool

val matrix_ballot_valid_dec :
  'a1 list -> ('a1 -> 'a1 -> bool) -> 'a3 -> 'a4 -> 'a6 -> 'a5 -> (('a3, 'a4,
  'a5) group -> plaintext -> 'a2) -> (('a3, 'a4, 'a5) group -> 'a6 -> 'a2 ->
  plaintext) -> (('a3, 'a4, 'a5) group -> 'a6 -> 'a2 -> 'a7) -> (('a3, 'a4,
  'a5) group -> nat -> 'a1 list -> 'a1 permutation) -> (('a3, 'a4, 'a5) group
  -> nat -> 'a10) -> (('a3, 'a4, 'a5) group -> nat -> 'a1 list -> 'a1
  permutation -> 'a10 -> 'a8) -> (('a3, 'a4, 'a5) group -> nat -> 'a1 list ->
  'a1 permutation -> 'a8 -> 'a10 -> 'a9) -> (('a3, 'a4, 'a5) group -> 'a2 ->
  'a2 -> 'a2) -> (('a3, 'a4, 'a5) group -> nat -> 'a11) -> (('a3, 'a4, 'a5)
  group -> nat -> 'a1 list -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a2) -> 'a1
  permutation -> 'a11 -> 'a1 -> 'a2) -> (('a3, 'a4, 'a5) group -> nat -> 'a1
  list -> ('a1 -> 'a2) -> ('a1 -> 'a2) -> 'a1 permutation -> 'a8 -> 'a10 ->
  'a11 -> 'a12) -> 'a1 pballot -> bool

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

val ecount_all_ballot :
  'a1 list -> 'a3 -> 'a4 -> 'a6 -> 'a5 -> (('a3, 'a4, 'a5) group -> plaintext
  -> 'a2) -> (('a3, 'a4, 'a5) group -> 'a6 -> 'a2 -> plaintext) -> (('a3,
  'a4, 'a5) group -> 'a6 -> 'a2 -> 'a7) -> (('a3, 'a4, 'a5) group -> 'a2 ->
  'a2 -> 'a2) -> ('a1, 'a2) eballot list -> ('a1 -> 'a1 -> 'a2, ('a1, 'a2,
  'a3, 'a4, 'a5, 'a7, 'a8, 'a9, 'a10) eCount) sigT

val idx_search_list :
  ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 list -> 'a2 list -> 'a2

val ppartial_count_all_counted :
  'a1 list -> ('a1 -> 'a1 -> bool) -> 'a3 -> 'a4 -> 'a6 -> 'a5 -> (('a3, 'a4,
  'a5) group -> plaintext -> 'a2) -> (('a3, 'a4, 'a5) group -> 'a6 -> 'a2 ->
  plaintext) -> (('a3, 'a4, 'a5) group -> 'a6 -> 'a2 -> 'a7) -> (('a3, 'a4,
  'a5) group -> nat -> 'a1 list -> 'a1 permutation) -> (('a3, 'a4, 'a5) group
  -> nat -> 'a10) -> (('a3, 'a4, 'a5) group -> nat -> 'a1 list -> 'a1
  permutation -> 'a10 -> 'a8) -> (('a3, 'a4, 'a5) group -> nat -> 'a1 list ->
  'a1 permutation -> 'a8 -> 'a10 -> 'a9) -> (('a3, 'a4, 'a5) group -> 'a2 ->
  'a2 -> 'a2) -> (('a3, 'a4, 'a5) group -> nat -> 'a11) -> (('a3, 'a4, 'a5)
  group -> nat -> 'a1 list -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a2) -> 'a1
  permutation -> 'a11 -> 'a1 -> 'a2) -> (('a3, 'a4, 'a5) group -> nat -> 'a1
  list -> ('a1 -> 'a2) -> ('a1 -> 'a2) -> 'a1 permutation -> 'a8 -> 'a10 ->
  'a11 -> 'a12) -> ('a1, 'a2) eballot list -> ('a1, 'a2) eballot list ->
  ('a1, 'a2) eballot list -> ('a1 -> 'a1 -> 'a2) -> ('a1, 'a2, 'a3, 'a4, 'a5,
  'a7, 'a8, 'a9, 'a12) eCount -> (('a1, 'a2) eballot list, ('a1 -> 'a1 ->
  'a2, ('a1, 'a2, 'a3, 'a4, 'a5, 'a7, 'a8, 'a9, 'a12) eCount) sigT) sigT

val pall_ballots_counted :
  'a1 list -> ('a1 -> 'a1 -> bool) -> 'a3 -> 'a4 -> 'a6 -> 'a5 -> (('a3, 'a4,
  'a5) group -> plaintext -> 'a2) -> (('a3, 'a4, 'a5) group -> 'a6 -> 'a2 ->
  plaintext) -> (('a3, 'a4, 'a5) group -> 'a6 -> 'a2 -> 'a7) -> (('a3, 'a4,
  'a5) group -> nat -> 'a1 list -> 'a1 permutation) -> (('a3, 'a4, 'a5) group
  -> nat -> 'a10) -> (('a3, 'a4, 'a5) group -> nat -> 'a1 list -> 'a1
  permutation -> 'a10 -> 'a8) -> (('a3, 'a4, 'a5) group -> nat -> 'a1 list ->
  'a1 permutation -> 'a8 -> 'a10 -> 'a9) -> (('a3, 'a4, 'a5) group -> 'a2 ->
  'a2 -> 'a2) -> (('a3, 'a4, 'a5) group -> nat -> 'a11) -> (('a3, 'a4, 'a5)
  group -> nat -> 'a1 list -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a2) -> 'a1
  permutation -> 'a11 -> 'a1 -> 'a2) -> (('a3, 'a4, 'a5) group -> nat -> 'a1
  list -> ('a1 -> 'a2) -> ('a1 -> 'a2) -> 'a1 permutation -> 'a8 -> 'a10 ->
  'a11 -> 'a12) -> ('a1, 'a2) eballot list -> (('a1, 'a2) eballot list, ('a1
  -> 'a1 -> 'a2, ('a1, 'a2, 'a3, 'a4, 'a5, 'a7, 'a8, 'a9, 'a12) eCount) sigT)
  sigT

val decrypt_margin :
  'a1 list -> ('a1 -> 'a1 -> bool) -> 'a3 -> 'a4 -> 'a6 -> 'a5 -> (('a3, 'a4,
  'a5) group -> plaintext -> 'a2) -> (('a3, 'a4, 'a5) group -> 'a6 -> 'a2 ->
  plaintext) -> (('a3, 'a4, 'a5) group -> 'a6 -> 'a2 -> 'a7) -> (('a3, 'a4,
  'a5) group -> nat -> 'a1 list -> 'a1 permutation) -> (('a3, 'a4, 'a5) group
  -> nat -> 'a10) -> (('a3, 'a4, 'a5) group -> nat -> 'a1 list -> 'a1
  permutation -> 'a10 -> 'a8) -> (('a3, 'a4, 'a5) group -> nat -> 'a1 list ->
  'a1 permutation -> 'a8 -> 'a10 -> 'a9) -> (('a3, 'a4, 'a5) group -> 'a2 ->
  'a2 -> 'a2) -> (('a3, 'a4, 'a5) group -> nat -> 'a11) -> (('a3, 'a4, 'a5)
  group -> nat -> 'a1 list -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a2) -> 'a1
  permutation -> 'a11 -> 'a1 -> 'a2) -> (('a3, 'a4, 'a5) group -> nat -> 'a1
  list -> ('a1 -> 'a2) -> ('a1 -> 'a2) -> 'a1 permutation -> 'a8 -> 'a10 ->
  'a11 -> 'a12) -> ('a1, 'a2) eballot list -> ('a1 -> 'a1 -> plaintext, ('a1,
  'a2, 'a3, 'a4, 'a5, 'a7, 'a8, 'a9, 'a12) eCount) sigT

val pschulze_winners :
  'a1 list -> ('a1 -> 'a1 -> bool) -> 'a3 -> 'a4 -> 'a6 -> 'a5 -> (('a3, 'a4,
  'a5) group -> plaintext -> 'a2) -> (('a3, 'a4, 'a5) group -> 'a6 -> 'a2 ->
  plaintext) -> (('a3, 'a4, 'a5) group -> 'a6 -> 'a2 -> 'a7) -> (('a3, 'a4,
  'a5) group -> nat -> 'a1 list -> 'a1 permutation) -> (('a3, 'a4, 'a5) group
  -> nat -> 'a10) -> (('a3, 'a4, 'a5) group -> nat -> 'a1 list -> 'a1
  permutation -> 'a10 -> 'a8) -> (('a3, 'a4, 'a5) group -> nat -> 'a1 list ->
  'a1 permutation -> 'a8 -> 'a10 -> 'a9) -> (('a3, 'a4, 'a5) group -> 'a2 ->
  'a2 -> 'a2) -> (('a3, 'a4, 'a5) group -> nat -> 'a11) -> (('a3, 'a4, 'a5)
  group -> nat -> 'a1 list -> ('a1 -> 'a1 -> bool) -> ('a1 -> 'a2) -> 'a1
  permutation -> 'a11 -> 'a1 -> 'a2) -> (('a3, 'a4, 'a5) group -> nat -> 'a1
  list -> ('a1 -> 'a2) -> ('a1 -> 'a2) -> 'a1 permutation -> 'a8 -> 'a10 ->
  'a11 -> 'a12) -> ('a1, 'a2) eballot list -> ('a1 -> bool, ('a1, 'a2, 'a3,
  'a4, 'a5, 'a7, 'a8, 'a9, 'a12) eCount) sigT

type cand =
| A
| B
| C

val cand_all : cand list

val cand_eq_dec : cand -> cand -> bool

val eschulze_winners_pf :
  'a2 -> 'a3 -> 'a5 -> 'a4 -> (('a2, 'a3, 'a4) group -> plaintext -> 'a1) ->
  (('a2, 'a3, 'a4) group -> 'a5 -> 'a1 -> plaintext) -> (('a2, 'a3, 'a4)
  group -> 'a5 -> 'a1 -> 'a6) -> (('a2, 'a3, 'a4) group -> nat -> cand list
  -> cand permutation) -> (('a2, 'a3, 'a4) group -> nat -> 'a9) -> (('a2,
  'a3, 'a4) group -> nat -> cand list -> cand permutation -> 'a9 -> 'a7) ->
  (('a2, 'a3, 'a4) group -> nat -> cand list -> cand permutation -> 'a7 ->
  'a9 -> 'a8) -> (('a2, 'a3, 'a4) group -> 'a1 -> 'a1 -> 'a1) -> (('a2, 'a3,
  'a4) group -> nat -> 'a10) -> (('a2, 'a3, 'a4) group -> nat -> cand list ->
  (cand -> cand -> bool) -> (cand -> 'a1) -> cand permutation -> 'a10 -> cand
  -> 'a1) -> (('a2, 'a3, 'a4) group -> nat -> cand list -> (cand -> 'a1) ->
  (cand -> 'a1) -> cand permutation -> 'a7 -> 'a9 -> 'a10 -> 'a11) -> (cand,
  'a1) eballot list -> (cand -> bool, (cand, 'a1, 'a2, 'a3, 'a4, 'a6, 'a7,
  'a8, 'a11) eCount) sigT
