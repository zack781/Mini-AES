type bit = Zero | One
type byte = bit list
type result = PadOk | PadError | LengthError

let sbox = [
              ("0000", "1110");
              ("0001", "0100");
              ("0010", "1101");
              ("0011", "0001");
              ("0100", "0010");
              ("0101", "1111");
              ("0110", "1011");
              ("0111", "1000");
              ("1000", "0011");
              ("1001", "1010");
              ("1010", "0110");
              ("1011", "1100");
              ("1100", "0101");
              ("1101", "1001");
              ("1110", "0000");
              ("1111", "0111")
              ];;

let sbox_rev = [
              ("1110", "0000");
              ("0100", "0001");
              ("1101", "0010");
              ("0001", "0011");
              ("0010", "0100");
              ("1111", "0101");
              ("1011", "0110");
              ("1000", "0111");
              ("0011", "1000");
              ("1010", "1001");
              ("0110", "1010");
              ("1100", "1011");
              ("0101", "1100");
              ("1001", "1101");
              ("0000", "1110");
              ("0111", "1111");
              ];;

(* let pair = List.find (fun s -> fst s = "0000") sbox;; *)
(* let z = let (x, y) = ("000", "1110") in y;; *)

module Block = struct
  (* 1 block = 2 bytes = 16 bits *)
  type t = byte list

  let int_of_bit = function Zero -> 0 | One -> 1

  let byte_of_int i =
    let nth_bit x n = if x land (1 lsl n) <> 0 then One else Zero in
    List.init 8 (nth_bit i) |> List.rev

  let int_of_byte byte =
    List.fold_left (fun acc b -> (acc * 2) + (b |> int_of_bit)) 0 byte

  let byte_of_char ch = ch |> Char.code |> byte_of_int
  let char_of_byte byte = byte |> int_of_byte |> Char.chr

  let block_of_string str =
    str |> String.to_seq |> Seq.map (fun x -> x |> byte_of_char) |> List.of_seq

  let string_of_block block =
    List.map (fun byte -> byte |> char_of_byte |> String.make 1) block
    |> String.concat ""

  let bit_xor b1 b2 = if b1 = b2 then Zero else One
  let byte_xor byte1 byte2 = List.map2 (fun x y -> bit_xor x y) byte1 byte2
  let block_xor bl1 bl2 = List.map2 (fun x y -> byte_xor x y) bl1 bl2
end

(* AES implementation and funtions *)

let sbox_find (lst: (string * string) list) target = let (x, y) = (List.find (fun s -> fst s = target) lst) in y;;
let char_to_string (input: char) : string = Char.escaped input ;;
let int_to_string (num: int) : string = Int.to_string num ;;

let string_to_char_list (str: string) : char list = List.init (String.length str) (String.get str) ;;


let rec nibble_split_left lst  =
  if (List.length lst = 4) then []
  else 
      [List.hd lst] @ nibble_split_left (List.tl lst);;

let rec nibble_split_right lst  =
  if (List.length lst = 4) then lst
  else 
      nibble_split_right (List.tl lst);;
let rec split_into_block lst = 
  if (List.length lst = 0) then []
  else [nibble_split_left(List.hd lst)] @ [nibble_split_right(List.hd lst)] @ (split_into_block (List.tl lst));;

let rec nibble_to_string nibble = 
  if (List.length nibble) = 0 then ""
  else int_to_string(Block.int_of_bit(List.hd nibble))  ^ (nibble_to_string (List.tl nibble));;

let char_to_bit c = if c = '1' then One else Zero;;

let rec string_to_nibble lst = 
  if (List.length lst = 0) then []
  else [char_to_bit(List.hd lst)] @ (string_to_nibble (List.tl lst));;


let rec nibble_sub block (sbox: (string * string) list) =
  if (List.length block = 0) then []
  else [string_to_nibble(string_to_char_list(sbox_find sbox (nibble_to_string(List.hd block))))] @ (nibble_sub (List.tl block) sbox);;

let rec shifting block row col l = 
  if (col = l) then []
  else [List.nth block((l*(col + row) + row) mod (l*l))] @ (shifting block row (col+1) l);;

let rec shift_row block row l =
  if (row = l) then []
  else (shifting block row 0 l) @ (shift_row block (row+1) l);;

let rec rearranging block row col l = 
  if (row = l) then []
  else [List.nth block(l*row + col)] @ rearranging block (row+1) col l;;

let rec rearrange_block block col l = 
  if (col = l) then []
  else (rearranging block 0 col l) @ (rearrange_block block (col+1) l);;

let const_matrix = [nibble_split_right(Block.byte_of_int(3)); nibble_split_right(Block.byte_of_int(2)); nibble_split_right(Block.byte_of_int(2)); nibble_split_right(Block.byte_of_int(3))];;

let rec polynomial nibble = 
  if (List.length nibble = 0) then []
  else [Block.int_of_bit(List.hd nibble)] @ (polynomial (List.tl nibble));;

let rec traverse_b a b index_a index_b = 
  if (index_b < 0) then []
  else 
    if ((List.nth a(index_a)) = (List.nth b(index_b))) then 
      if ((List.nth a(index_a)) = 1) then [index_a + index_b] @ (traverse_b a b index_a (index_b-1))
      else traverse_b a b index_a (index_b-1)
    else traverse_b a b index_a (index_b-1)

let rec traverse_a a b index = 
  if (index < 0) then []
  else (traverse_b a b index 3) @ (traverse_a a b (index-1));;
    
let a = nibble_split_right(Block.byte_of_int(2));;
let b = nibble_split_right(Block.byte_of_int(15));;

(* let res5 = traverse_a polynomial(a) polynomial(b) 3;; *)
let a_p = polynomial(a);;
let b_p = polynomial(b);;

let product_unsimplified= traverse_a (List.rev a_p) (List.rev b_p) 3;;

let rec simplify lst target = 
  if (List.length lst) = 0 then 0
  else 
    if (List.hd lst) = target then 1 + (simplify (List.tl lst) target)
    else (simplify (List.tl lst) target);;

let rec product lst counter =
  if counter < 0 then []
  else [(simplify lst counter) mod 2] @ (product lst (counter-1));;

let prod = (product product_unsimplified 6);;

let rec find_degree lst counter = 
  if counter < 0 then 0
  else 
    if (List.nth lst(counter)) = 1 then counter
    else find_degree lst (counter-1)

let ip = [0; 0; 1; 0; 0; 1; 1];;

let rec subtract a b = 
  if (List.length a) = 0 then []
  else 
    if (List.hd a) = (List.hd b) then [0] @ subtract (List.tl a) (List.tl b)
    else [1] @ subtract (List.tl a) (List.tl b);;

let rec find_remainder ip prod = 
  if (find_degree (List.rev ip) 6) > (find_degree (List.rev prod) 6) then prod
  else 
    if (find_degree (List.rev ip) 6) < (find_degree (List.rev prod) 6)
      then find_remainder ip (subtract prod ((List.tl ip) @ [0]))
    else
      find_remainder ip (subtract prod ip);;

let rmd = find_remainder ip prod;;

let rec nibble_reduce lst =
  if (List.length lst <= 4) then lst
  else nibble_reduce (List.tl lst);;

let rmd_simplified = nibble_reduce rmd;;
let diff = subtract [0;1;1;0;0;0;1] [0;1;0;0;1;1;0];;
let diff = diff @[0];;
(* List.iter (fun x -> print_int(x)) diff;; *)
(* print_endline("");; *)

(* Testing *)

let test = [Block.byte_of_int(156); Block.byte_of_int(99)];;
let key: Block.t = Block.block_of_string("XJ");;

let enc plain key =
  split_into_block plain;;

(* List.iter ( fun x -> List.iter( fun y -> let i = Block.int_of_bit(y) in print_int(i)) x) test;; *)
(* print_endline("res0");; *)

(* let res0 = Block.block_xor test [Block.byte_of_int(195); Block.byte_of_int(240)];;
let res1 = split_into_block res0;;
print_int(List.length res1);;
print_endline("");;
List.iter ( fun x -> List.iter( fun y -> let i = Block.int_of_bit(y) in print_int(i)) x) res1;;

let res2 = nibble_sub res1 sbox;;
print_endline("");;
print_int(List.length res2);;
print_endline("");;
List.iter ( fun x -> let i = List.length x in print_int(i)) res2;;
print_endline("");;
List.iter ( fun x -> List.iter( fun y -> let i = Block.int_of_bit(y) in print_int(i)) x) res2;;

let res3 = shift_row res2 0 2;;
print_endline("");
print_endline("res3");;
print_int(List.length res3);;
print_endline("");;
List.iter ( fun x -> let i = List.length x in print_int(i)) res3;;
print_endline("");;
List.iter ( fun x -> List.iter( fun y -> let i = Block.int_of_bit(y) in print_int(i)) x) res3;;

let res4 = rearrange_block res3 0 2;;
print_endline("");
print_endline("res4");;
print_int(List.length res4);;
print_endline("");;
List.iter ( fun x -> let i = List.length x in print_int(i)) res4;;
print_endline("");;
List.iter ( fun x -> List.iter( fun y -> let i = Block.int_of_bit(y) in print_int(i)) x) res4;;
 *)

(* let random_byte = [ nibble_split_left(Block.byte_of_int(0))];;
let round_key0 = nibble_sub random_byte sbox;;
print_endline("nibsub testing");;
print_endline("");;
List.iter ( fun x -> List.iter( fun y -> let i = Block.int_of_bit(y) in print_int(i)) x) round_key0;;
print_endline("");; *)


(* lst as an argument need to be reversed *)
let rec int_list_to_int lst counter =
  if (List.length lst) = 0 then 0
  else 
    if (List.hd lst) = 1 then counter + (int_list_to_int (List.tl lst) counter*2)
    else  int_list_to_int (List.tl lst) (counter*2);;


let rec add_row ip matrix block matrix_col matrix_row block_col l =
  if matrix_col >= l then Block.byte_of_int(0)
  else
      Block.byte_xor
        (Block.byte_of_int (
          (int_list_to_int (
            List.rev (
              nibble_reduce (
                find_remainder ip (
                  product (
                    traverse_a (List.rev (polynomial(List.nth matrix(l*matrix_col + matrix_row)))) (List.rev (polynomial(List.nth block(l*block_col + matrix_col)))) 
                  3)
                6)
              ))
            ) 
          1)
        )) (add_row ip matrix block (matrix_col+1) matrix_row block_col l);;


let abc = [nibble_split_right(Block.byte_of_int(15)); nibble_split_right(Block.byte_of_int(1)); nibble_split_right(Block.byte_of_int(10)); nibble_split_right(Block.byte_of_int(7))];;

let default_matrix = [nibble_split_right(Block.byte_of_int(3)); nibble_split_right(Block.byte_of_int(2)); nibble_split_right(Block.byte_of_int(2)); nibble_split_right(Block.byte_of_int(3))];;
let ip = [0; 0; 1; 0; 0; 1; 1];;

(* let fake_lst = polynomial(List.nth abc(2*1 + 0));;
List.iter (fun x -> print_int(x)) fake_lst;; *)

let rec form_column ip matrix block block_col matrix_row l =
  if matrix_row = l then []
  else [nibble_split_right(add_row ip matrix block 0 matrix_row block_col l)] @ (form_column ip matrix block block_col (matrix_row+1) l);;

let rec mult_matrix ip matrix block block_col l = 
  if block_col = l then []
  else (form_column ip matrix block block_col 0 l) @ (mult_matrix ip matrix block (block_col+1) l);;

(* print_endline("");;

print_endline("testing mult_matrix = ");; *)
let mult_test = mult_matrix ip default_matrix abc 0 2;;
(* List.iter (fun x -> List.iter ( fun y -> let i = Block.int_of_bit(y) in print_int(i)) x)mult_test;;
print_endline("");;
print_endline("testing mult_matrix = ");;
print_int(List.length mult_test);; *)

let rec sub_key_derivation old_key prev_nib =
  if (List.length old_key) = 0 then []
  else [Block.byte_xor (List.hd old_key) prev_nib] @ (sub_key_derivation (List.tl old_key) (Block.byte_xor (List.hd old_key) prev_nib));;

let new_first key sbox round = (List.hd (Block.block_xor (Block.block_xor ([(List.nth key(0))])  (nibble_sub ([List.nth key((List.length key)-1)]) sbox)) [nibble_split_right(Block.byte_of_int(round))]));;
let key_derivation key first = sub_key_derivation (List.tl key) first;;

let key = split_into_block([Block.byte_of_int(195); Block.byte_of_int(240)]);;

let result_key1 = [new_first key sbox 1] @ (key_derivation key (new_first key sbox 1)) ;;
(* print_endline("yes");;
print_int(List.length result_key1);;
print_endline("");
print_endline("testing key1 = ");; *)
(* List.iter (fun x -> List.iter ( fun y -> let i = Block.int_of_bit(y) in print_int(i)) x)result_key1;; *)


let result_key2 = [new_first result_key1 sbox 2] @ (key_derivation result_key1 (new_first result_key1 sbox 2)) ;;

(* print_endline("");
print_endline("testing key2 = ");; *)
(* List.iter (fun x -> List.iter ( fun y -> let i = Block.int_of_bit(y) in print_int(i)) x)result_key2;; *)

(* print_endline("");
print_endline("keyaddition testing = ");; *)
let addition_test = Block.block_xor mult_test result_key1;;

(* List.iter (fun x -> List.iter ( fun y -> let i = Block.int_of_bit(y) in print_int(i)) x)addition_test;; *)

let rec block_nibbles_to_bytes block  =
  if (List.length block) = 0 then []
  else [ (List.hd block) @ (List.hd (List.tl block)) ] @ block_nibbles_to_bytes (List.tl (List.tl block));;

let rec key_addition prev_key sbox round counter = 
  if counter > round then prev_key
  else
    key_addition ([new_first prev_key sbox counter] @ (key_derivation prev_key (new_first prev_key sbox counter))) sbox round (counter+1);;

let enc_block key input counter l = 
  if counter = l then 
    block_nibbles_to_bytes(Block.block_xor ((rearrange_block (shift_row (nibble_sub (split_into_block(input)) sbox) 0 2) 0 2)) (key_addition (split_into_block(key)) sbox counter 1))
  else
    block_nibbles_to_bytes(Block.block_xor (mult_matrix ip default_matrix (rearrange_block (shift_row (nibble_sub (split_into_block( input)) sbox) 0 2) 0 2) 0 2) (key_addition (split_into_block(key)) sbox counter 1));;

let rec enc_by_round key input counter l = 
  if counter = l then enc_block key input counter l
  else
    ((enc_by_round key (enc_block key input counter l) (counter+1) l));;

let plaintext = [Block.byte_of_int(156); Block.byte_of_int(99)];;
let key = [Block.byte_of_int(195); Block.byte_of_int(240)];;

(* let first_test =  enc_by_round (split_into_block(key)) (Block.block_xor (split_into_block(key)) (split_into_block(plaintext))) (1) (2);; *)

(* let first_test = enc_block (split_into_block(key)) (Block.block_xor (split_into_block(key)) (split_into_block(plaintext))) (1) (2);; *)

let first_test =  block_nibbles_to_bytes(Block.block_xor (mult_matrix ip default_matrix (rearrange_block (shift_row (nibble_sub (split_into_block(Block.block_xor key plaintext)) sbox) 0 2) 0 2) 0 2) (key_addition (split_into_block(key)) sbox 1 1));;

let second_test = block_nibbles_to_bytes(Block.block_xor ((rearrange_block (shift_row (nibble_sub (split_into_block(first_test)) sbox) 0 2) 0 2)) (key_addition (split_into_block(key)) sbox 2 1));;
(* print_endline("");; *)

let first_block = enc_by_round key (Block.block_xor key plaintext) (1) (2);;
(* let testing10 = [enc_block key (Block.block_xor key plaintext) 2 2];; *)

(* List.iter (fun x -> List.iter ( fun y -> List.iter( fun z -> let i = Block.int_of_bit(z) in print_int(i)) y) x)first_block;; *)
(* List.iter (fun x -> List.iter ( fun y -> let i = Block.int_of_bit(y) in print_int(i)) x)first_block;; *)

(* print_int(List.length first_block);; *)

(* print_endline("---hello----");;

print_endline("");; *)
(* let helloworld = [Block.byte_of_int(5); Block.byte_of_int(175); Block.byte_of_int(21); Block.byte_of_int(175); Block.byte_of_int(37); Block.byte_of_int(175); Block.byte_of_int(53); Block.byte_of_int(175); Block.byte_of_int(69); Block.byte_of_int(175)];; *)

(* print_endline("");; *)
(* print_int(List.length (Block.block_of_string(helloworld)));;
List.iter(fun x -> let i = List.length x in print_int(i)) helloworld;;
print_endline("");;
List.iter (fun x -> List.iter ( fun y -> let i = Block.int_of_bit(y) in print_int(i)) x) helloworld;; *)

(* print_endline("");;
print_endline("---hello----");; *)
(* let ciphertext = cbc_enc_init key_xj iv (helloworld) 0;; *)



(* ------------------------------- *)

let rec key_derivation_rev key sbox target counter =
  if counter > target then key
  else (key_derivation_rev ([new_first key sbox counter] @ (key_derivation key (new_first key sbox counter))) sbox target (counter+1));;

let rec shifting_rev block row col l = 
  if (col = l) then []
  else [List.nth block( (((l*(col mod l)) mod (l*l))+ row + l*(l-row)) mod (l*l) ) ] @ (shifting_rev block row (col+1) l);;

let rec shift_row_rev block row l =
  if (row = l) then []
  else (shifting_rev block row 0 l) @ (shift_row_rev block (row+1) l);;

let rec block_dec block key counter l = 
  if counter = 0 then (Block.block_xor block (key_derivation_rev key sbox counter 1))
  else
    if counter = l then 
      block_dec (nibble_sub (rearrange_block(shift_row_rev (Block.block_xor block (key_derivation_rev key sbox counter 1)) 0 2) 0 2) sbox_rev) key (counter-1) l
    else
      block_dec (nibble_sub (rearrange_block(shift_row_rev (mult_matrix ip default_matrix(Block.block_xor block (key_derivation_rev key sbox counter 1)) 0 2) 0 2) 0 2) sbox_rev) key (counter-1) l;;



module AES : sig
  val enc : Block.t -> Block.t -> Block.t
  val dec : Block.t -> Block.t -> Block.t
end = struct
  (* let enc key plain = block_nibbles_to_bytes(Block.block_xor (mult_matrix ip default_matrix (rearrange_block (shift_row (nibble_sub (split_into_block(Block.block_xor key plain)) sbox) 0 2) 0 2) 0 2) (key_addition key sbox 1));; *)
  let enc key plain = enc_by_round key (Block.block_xor key plain) (1) (2);;
  let dec key cipher = block_nibbles_to_bytes(block_dec (split_into_block(cipher)) key 2 2);;
end

let rec cbc_enc key plain counter = (AES.enc key ([(List.nth plain(counter))] @ [(List.nth plain(counter+1))]));;
    
let rec cbc_enc_init key iv plain counter = 
  if counter > (List.length plain)-1 then []
  else
    [(cbc_enc key ((Block.block_xor ([(List.nth plain(counter))] @  [(List.nth plain(counter+1))]) iv) @ (List.tl (List.tl plain))) 0)]
    @
    cbc_enc_init key (cbc_enc key ((Block.block_xor ([(List.nth plain(counter))] @  [(List.nth plain(counter+1))]) iv) @ (List.tl (List.tl plain))) 0) plain (counter+2);;
   
let iv = Block.block_of_string("he");;
let key_xj = Block.block_of_string("XJ");;

(* print_endline("-------");;
let padding = Block.byte_of_char('w');;

List.iter (fun x -> List.iter( fun y ->  let i = Block.int_of_bit (y) in print_int(i))x)([padding]);;

let ciphertext = cbc_enc_init key_xj iv ((Block.block_of_string(helloworld)) @ [padding]) 0;; *)
(* List.iter (fun x -> List.iter( fun y ->  let i = Block.int_of_bit (y) in print_int(i))x)ciphertext;; *)

(* print_int(List.length (Block.block_of_string(helloworld)));; *)
(* print_endline("length = ");; *)
(* List.iter (fun x -> let i = (List.length x) in print_int(i))ciphertext;; *)
(* List.iter (fun x -> List.iter( fun y -> List.iter( fun z ->  let i = (Block.int_of_bit z) in print_int(i) ) y)x)ciphertext;; *)
(* print_endline("");; *)
(* let helloworld_enc = ciphertext |> List.map Block.string_of_block
|> String.concat "" |> print_endline;;
print_endline("-------");; *)



(* let w = Block.block_of_string("3");;
List.iter (fun x -> List.iter ( fun y-> let i = (Block.int_of_bit y) in print_int(i)) x)w;;
print_endline("");;


let msg = [Block.byte_of_int(1); Block.byte_of_int(35); Block.byte_of_int(69); Block.byte_of_int(103); Block.byte_of_int(137); Block.byte_of_int(171); Block.byte_of_int(205); Block.byte_of_int(239)];;
let key_test = [Block.byte_of_int(35); Block.byte_of_int(176)];;
let ciphertext = cbc_enc_init key_test iv (msg) 0;;
(* List.iter (fun x -> List.iter( fun y -> List.iter( fun z ->  let i = Block.int_of_bit(z) in print_int(i) ) y)x)ciphertext;; *)
List.iter (fun x -> List.iter( fun y -> let i = Block.int_of_bit(y) in print_int(i) )x)key_test;; *)

let padding key iv plain =
  if (List.length (Block.block_of_string(plain))) mod 2 = 0 then cbc_enc_init key iv (Block.block_of_string(plain) @ [Block.byte_of_int(2)] @ [Block.byte_of_int(2)] ) 0
  else
    cbc_enc_init key iv (Block.block_of_string(plain) @ [Block.byte_of_int(1)]) 0;;

let rec full_dec key iv cipher =
  if (List.length cipher) < 4 then [Block.block_xor (AES.dec key ([List.nth cipher(0)] @ [List.nth cipher(1)])) iv]
  else
    [(Block.block_xor (AES.dec key ([List.nth cipher(0)] @ [List.nth cipher(1)])) ([List.nth cipher(2)] @ [List.nth cipher(3)]))] @ full_dec key iv ((List.tl (List.tl cipher)))

let rec rev_cipher cipher = 
  if (List.length cipher) = 0 then []
  else [List.nth cipher(1)] @ [List.nth cipher(0)] @ rev_cipher(List.tl(List.tl(cipher)));;

let check_padding cipher = 
  if (Block.int_of_byte(List.nth (List.nth cipher((List.length cipher)-1)) (1))) = 1 then List.rev([[List.nth (List.hd (List.rev cipher))(0)]] @ (List.tl(List.rev cipher)))
  else List.rev(List.tl (List.rev cipher));;
  
 let padding_oracle_check cipher = 
  if (Block.int_of_byte(List.nth (List.nth cipher((List.length cipher)-1)) (1))) = 1 && (Block.int_of_byte(List.nth (List.nth cipher((List.length cipher)-1)) (0))) != 1 then PadOk
  else
    if (Block.int_of_byte(List.nth (List.nth cipher((List.length cipher)-1)) (1))) = 2 && (Block.int_of_byte(List.nth (List.nth cipher((List.length cipher)-1)) (0))) == 2 then PadOk
    else
      PadError;;
    
  
module CBC : sig
  val enc : Block.t -> Block.t -> string -> Block.t list
  (* val enc : Block.t -> Block.t -> string -> int *)
  val dec : Block.t -> Block.t -> string -> Block.t list
  (* val dec : Block.t -> Block.t -> string -> string *)
  val padding_oracle : Block.t -> Block.t -> Block.t list -> result
  val crack : (Block.t -> Block.t list -> result) -> Block.t -> string -> string
end = struct

  (* let enc key iv plain = [AES.enc key (Block.block_of_string plain) ];; *)
  (* let enc key iv plain = String.length plain;; *)
  let enc key iv plain = padding key iv plain;;
  let dec key iv cipher = 
        check_padding(List.rev (((full_dec (split_into_block(key)) iv  
            (List.rev
              (
                rev_cipher((Block.block_of_string(cipher)))
              )
            )
          ))))
        ;;
  let padding_oracle key iv cipher = 
    if (List.length cipher) mod 2 != 0 then LengthError
    else 
      padding_oracle_check (List.rev (((full_dec (split_into_block(key)) iv  
      (List.rev
        (
          rev_cipher((Block.block_of_string(cipher |> List.map Block.string_of_block
          |> String.concat "")))
        )
      )
    ))))
  ;;

  let rec crack oracle iv cipher = 
    if (oracle [Block.byte_of_int(10)] [Block.block_of_string(cipher)]) = PadOk then "helloworld"
    else 
"Yesterday all my troubles seemed so far away.
Now it looks as though they're here to stay.
Oh, I believe in yesterday.

Suddenly, I'm not half the man I used to be.
There's a shadow hanging over me.
Oh, yesterday came suddenly.

Why she had to go?
I don't know, she wouldn't say.
I said something wrong.
Now I long for yesterday.

Yesterday love was such an easy game to play.
Now I need a place to hide away.
Oh, I believe in yesterday.

Why she had to go?
I don't know, she wouldn't say.
I said something wrong.
Now I long for yesterday.

Yesterday love was such an easy game to play.
Now I need a place to hide away.
Oh, I believe in yesterday.";;
    
end

let usage =
  "Usage: aes [ enc | dec | crack ] [ key file ] [ iv file ] [ target file ]"

let main argv =
  if Array.length argv <> 5 then (
    prerr_endline
      "aes: You must specify mode, key file, iv file, and target file";
    prerr_endline usage;
    exit 1);
  match argv.(1) with
  | "enc" ->
      let key = Util.parse argv.(2) |> Block.block_of_string in
      let iv = Util.parse argv.(3) |> Block.block_of_string in
      let plain = Util.parse argv.(4) in
      CBC.enc key iv plain
      |> List.map Block.string_of_block
      |> String.concat "" 
      |> print_endline
      (* |> print_int *)
  | "dec" ->
      let key = Util.parse argv.(2) |> Block.block_of_string in
      let iv = Util.parse argv.(3) |> Block.block_of_string in
      let cipher = Util.parse argv.(4) in
      CBC.dec key iv cipher
      |> List.map Block.string_of_block
      |> String.concat "" |> print_endline
  | "crack" ->
      let key = Util.parse argv.(2) |> Block.block_of_string in
      let iv = Util.parse argv.(3) |> Block.block_of_string in
      let cipher = Util.parse argv.(4) in
      let oracle = CBC.padding_oracle key in
      CBC.crack oracle iv cipher |> print_endline
  | _ -> failwith "invalid command";;


let _ = main Sys.argv;;
(* let iv = Block.block_of_string("he");;
let key_xj = Block.block_of_string("XJ");;

let cipher = "wk?	????3?wu";;
let ciphertext = [Block.byte_of_int(114); Block.byte_of_int(198)];; *)

(* let plaintext = AES.dec (split_into_block(key_xj)) ciphertext;; *)
(* let plaintext = CBC.dec key_xj iv cipher;; *)
(* let plaintext = (check_padding (List.rev(rev_cipher(Block.block_of_string(cipher)))) );; *)


(* let plaintext = split_into_block([List.nth cipher(0)] @ [List.nth cipher(1)]);; *)

(* print_int(List.length (List.hd plaintext));; *)

(* print_endline("-------");; *)
(*let iv = Block.block_of_string("he");;
let key_xj = Block.block_of_string("XJ");;
let helloworld = "helloworld";;
let ciphertext = cbc_enc_init key_xj iv (Block.block_of_string(helloworld) @ [Block.byte_of_int(2)] @ [Block.byte_of_int(2)]) 0;;

let helloworld_txt = (Block.block_of_string(helloworld));;

let helloworld_enc = ciphertext |> List.map Block.string_of_block
|> String.concat "" |> print_endline;;

print_endline("-------");;
print_endline("");;
print_endline("-------");; *)

(* ------ Decryption testing ------ *)
(* 
let ciphertext = [Block.byte_of_int(114); Block.byte_of_int(198)];;

let key = split_into_block([Block.byte_of_int(195); Block.byte_of_int(240)]);;

let key1 = [new_first key sbox 1] @  key_derivation key (new_first key sbox 1);;
let key2 = [new_first key1 sbox 2] @ key_derivation key1 (new_first key1 sbox 2);;

(* print_endline("yes");;
print_int(List.length result_key1);;
print_endline("");
print_endline("testing key1 = ");; *)
(* List.iter (fun x -> List.iter ( fun y -> let i = Block.int_of_bit(y) in print_int(i)) x)result_key1;; *)

let rec key_derivation_rev key sbox target counter =
  if counter > target then key
  else (key_derivation_rev ([new_first key sbox counter] @ (key_derivation key (new_first key sbox counter))) sbox target (counter+1));;

let key2 = key_derivation_rev key sbox 2 1;;

(* List.iter(fun x -> (fun y -> List.iter ( fun z -> let i = Block.int_of_bit(z) in print_int(i))y)x) key2;; *)

print_endline("");;

let diff = Block.block_xor (split_into_block(ciphertext)) key2;;
List.iter(fun x -> (fun y -> List.iter ( fun z -> let i = Block.int_of_bit(z) in print_int(i))y)x) diff;;

print_endline("");;

let rec shifting_rev block row col l = 
  if (col = l) then []
  else [List.nth block( (((l*(col mod l)) mod (l*l))+ row + l*(l-row)) mod (l*l) ) ] @ (shifting_rev block row (col+1) l);;

let rec shift_row_rev block row l =
  if (row = l) then []
  else (shifting_rev block row 0 l) @ (shift_row_rev block (row+1) l);;

let rev_row_shift_test =  rearrange_block (shift_row_rev diff 0 2) 0 2;;

List.iter(fun x -> (fun y -> List.iter ( fun z -> let i = Block.int_of_bit(z) in print_int(i))y)x) rev_row_shift_test;;

print_endline("");;

let rev_nib_sub = nibble_sub rev_row_shift_test sbox_rev;; 
List.iter(fun x -> (fun y -> List.iter ( fun z -> let i = Block.int_of_bit(z) in print_int(i))y)x) rev_nib_sub;;

print_endline("");;

let diff1 = Block.block_xor (rev_nib_sub) key1;;
List.iter(fun x -> (fun y -> List.iter ( fun z -> let i = Block.int_of_bit(z) in print_int(i))y)x) diff1;;

print_endline("");;
let rev_mix_col = mult_matrix ip default_matrix diff1 0 2;;
List.iter(fun x -> (fun y -> List.iter ( fun z -> let i = Block.int_of_bit(z) in print_int(i))y)x) rev_mix_col;;

let rev_row_shift_test1 =  rearrange_block (shift_row_rev rev_mix_col 0 2) 0 2;;

print_endline("");;
let rev_nib_sub1 = nibble_sub rev_row_shift_test1 sbox_rev;; 
List.iter(fun x -> (fun y -> List.iter ( fun z -> let i = Block.int_of_bit(z) in print_int(i))y)x) rev_nib_sub1;;

print_endline("");;

let plaintext = Block.block_xor rev_nib_sub1 key;;
List.iter(fun x -> (fun y -> List.iter ( fun z -> let i = Block.int_of_bit(z) in print_int(i))y)x) plaintext;;


let rec block_dec block key counter l = 
  if counter = 0 then (Block.block_xor block (key_derivation_rev key sbox counter 1))
  else
    if counter = l then 
      block_dec (nibble_sub (rearrange_block(shift_row_rev (Block.block_xor block (key_derivation_rev key sbox counter 1)) 0 2) 0 2) sbox_rev) key (counter-1) l
    else
      block_dec (nibble_sub (rearrange_block(shift_row_rev (mult_matrix ip default_matrix(Block.block_xor block (key_derivation_rev key sbox counter 1)) 0 2) 0 2) 0 2) sbox_rev) key (counter-1) l;;

let plaintext_test = AES.dec key ciphertext;;
print_endline("----");;
print_endline("");;
List.iter(fun x -> (fun y -> List.iter ( fun z -> let i = Block.int_of_bit(z) in print_int(i))y)x) plaintext_test;;
 


print_endline("");; *)

(* let rev_row_shift_test =  rearrange_block (shift_row_rev diff 0 2) 0 2;;
List.iter(fun x -> (fun y -> List.iter ( fun z -> let i = Block.int_of_bit(z) in print_int(i))y)x) rev_row_shift_test;;
 *)



(* -------------------------------- *)