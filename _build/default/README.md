# Mini-AES
The goal of this homework is to implement the Mini-AES encryption scheme and demonstrate a padding oracle attack.

Students will write the following modules in [src/main.ml](src/main.ml):
* **AES** module for implementing Mini-AES encryption and decryption
* **CBC** module for block coding with CBC (Cipher Block Chaining) mode

In short, replace all failwith `"Not implemented"` with your implementation.

Notice that students are not permitted to change directory structures and types of functions. All the functions you implement must have the same types as described in the module signatures. However, students are allowed to change `let` to `let rec` if needed.

## Setup
This homework assumes that students set up the OCaml environment following the same instructions in the previous homework.

After running `make` under the root directory of this repository, you can run each tool with the corresponding inputs:

```
# run the mini-aes encryption
./aes enc test/key.txt test/iv.txt test/helloworld.txt
# run the mini-aes decryption
./aes dec test/key.txt test/iv.txt test/helloworld.txt
# run the mini-aes crack
./aes crack test/key.txt test/iv.txt test/helloworld.txt
```

Initially, you will get the `Not implemented` exception. Once you complete the implementation, the corresponding output will be printed
for each command such as
```
./aes enc test/key.txt test/iv.txt test/helloworld.txt
wk�	����3�wu
```
The output will be the same as `test/helloworld.expected`. Due to encoding problems, some characters may not be printed properly from the command line interface. Please use the `make test` command for accurate comparison.
Running `make test` checks whether all the outputs are the same as expected.
The command prints out discrepancies if they are different. Note that no output will be generated if the results are correct.



## Instructions
### 1. Implementing AES module for mini-AES encryption and decryption

This homework has been reconstructed regarding the following paper: [Mini Advanced Encryption Standard (Mini-AES): A Testbed for Cryptanalysis Students](https://piazza.com/class_profile/get_resource/ixlc30gojpe5fs/iyv0273azwtz4)

In this homework, you will implement a module for mini-AES encryption and decryption.
Mini-AES is a small version of AES. As you learned in the class, the standard version of AES has a block size of 128 bits and supports key sizes of 128, 192 and 256 bits. The number of rounds is 10, 12 or 14 for the three different key sizes respectively.

The mini-AES has a block size of 16 bits. Due to the difference in block size, several operations are different from the original AES. Please refer to the contents described in the paper of the above link.

### 2. Implementing the CBC (Cipher Block Chaining) mode

Students will implement block coding with CBC mode.
There are three data types:
- Bit (`type bit`): `Zero` or `One`
- Byte (`type byte = bit list`): `1 byte` always consists of `8 bits`
- Block (`Block.t`): `1 block` consists of `2 bytes` (i.e., `1 Block` = `2 Bytes` = `16 Bits`)

#### Encryption
Given a key, IV, and plaintext written in the input files, function `enc` will encrypt it with a CBC mode.
Students should encrypt the plaintext with padding.

#### Decryption
Given a key, IV, and ciphertext, function `dec` will decrypt it with a CBC mode.
Students should decrypt the ciphertext and remove the padding.

#### Padding Oracle
Students implement the padding oracle.
Given a key, IV, and ciphertext, the function `padding_oracle` returns either `PadOk`, `PadError` or `LengthError`.
- `PadOk`: the input text has correct padding.
- `PadError`: the input text has incorrect padding.
- `LengthError`: the length of the input text is incorrect (i.e., not a multiple of the block size).

#### Crack
Given a padding oracle, IV, and ciphertext, function `crack` will decrypt the ciphertext without a key using a padding oracle attack.

## Reference
- [OCaml Standard Library](https://v2.ocaml.org/api/)
- [Mini Advanced Encryption Standard (Mini-AES): A Testbed for Cryptanalysis Students](https://piazza.com/class_profile/get_resource/ixlc30gojpe5fs/iyv0273azwtz4)
