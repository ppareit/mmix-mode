;;; mmix-describe.el --- Help mode for MMIX.  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 - 2022 Pieter Pareit <pieter.pareit@gmail.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;; Authors: Pieter Pareit <pieter.pareit@gmail.com>
;; Maintainer: Pieter Pareit <pieter.pareit@gmail.com>
;; Homepage: https://github.com/ppareit/mmix-mode/
;; Created: 8 April 2022
;; Last modified:
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.2"))
;; Keywords: mode mmix mmixal mix mixal knuth asm risc

;;; Commentary:
;; Help system for editing files in the MMIXAL language
;; as described in
;; The Art of Computer Programming, Volume 1
;; MMIX A RISC Computer for the New Millennium
;; Fascicle 1
;; By Donald E. Knuth

;;; Code:

(require 'cl-lib)

(cl-defstruct mmix-description
  "Structure that describes an operation and pseudo-operation codes for MMIXAL.
SYMBOL is the symbolic form of the opcode or register as a symbol,
CALL shows how an instruction with the opcode is called and empty when register,
especially it shows what operands are required,
CATEGORY is category that the opcode falls into as a string,
TYPE is one of 'op, 'pseudo-op, 'alias-op or 'register
NAME is the human readable name as a string,
DESCRIPTION is the description as a string,
ALIAS is the real opcode as a string,
HEX is the hexadecimal value of the opcode as a string and empty when a register.
CODE is the register code and nil when a opcode
SAVED is true when the register is saved
PUT is true when the register can be put
These descriptions are based on the material available in
  * The Art of Computer Programming: Fascicle 1 (Knuth, 2005)
  * The MMIX Supplement (Ruckert, 2015)
  * MMIXware: A RISC Computer for the Third Millennium (Knuth, 1999)
  * MMIX Quick Reference Card (Ruckert, 2012)
To be able to follow allong, you will at least need the first two references."
  symbol call category type name description hex alias code saved put)

(defvar mmix-description-table (make-hash-table)
  "Table containing all the MMIX items that have a description.")

(defvar mmix-description-symbol-name-list '()
  "List containing symbols of all the MMIX items that have a description.")

(defun def-mmix-description (symbol &rest rest)
  "Define a mmix description item.
SYMBOL is the MMIXAL symbolic name.
REST are the properties."
  (let* ((args (append `(:symbol ,symbol) rest))
	 (sym (apply 'make-mmix-description args)))
    (puthash symbol sym mmix-description-table)
    (push (symbol-name symbol) mmix-description-symbol-name-list)))

(defvar mmix-description-categories-alist
  '((assembler-directive . "Assembler directive")
    (allocate-data . "Allocate data")
    (loading-data . "Loading data")
    (storing-data . "Storing data")
    (setting-register . "Setting register")
    (integer-arithmetic . "Integer arithmetic")
    (bitwise-operation . "Bitwise operation")
    (floating-point-arithmetic . "Floating point arithmetic")
    (jump . "Jump")
    (branch . "Branch")
    (subroutine . "Subroutine")
    (interrupts . "Interrupts")
    (namespace . "Name space")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Assembler Directives
;;

(def-mmix-description 'IS
  :call "Label IS Expression"
  :category 'assembler-directive
  :type 'pseudo-op
  :name "is"
  :description "Declare Label to be a shorthand for Expression.

Expression must not be a future reference. For example
x       IS      $1
makes x an abbreviation for register number 1. And similarly
FIVE    IS      5
makes FIVE an abbreviation for the constant 5. It is convention to use lowercase
for register numbers and uppercase for constants.")

(def-mmix-description 'LOC
  :call "Label|_ LOC Expression"
  :category 'assembler-directive
  :type 'pseudo-op
  :name "change location"
  :description "Change location to assemble at Expression.

If  the Label  is nonempty,  define the  Label to  be the  value of  the current
location. Then the  current location is changed to the  value of the Expression.
Often the  Expression is
  * #100  where programs start,
  *  Data_Segment where the data is placed.")

(def-mmix-description 'PREFIX
  :call "PREFIX Symbol"
  :category 'name-space
  :type 'pseudo-op
  :name "set prefix"
  :description "Redefines the current prefix to be the given Symbol.")

(def-mmix-description 'GREG
  :call "Label GREG Expression"
  :category 'assembler-directive
  :type 'pseudo-op
  :name "global register"
  :description "Allocate a new global register.

Set aside a new global register  containing the value Expression. Label can then
be used  as the name  for this  register. A commonly  used expression is  @ (the
current location). If Expression is 0, the value is considered dynamic an can be
chnged by the programm. If Expression is not 0, it is considered a constant that
will not change during the programm.

TODO: Improve documentation as my understanding grows.")

(def-mmix-description 'LOCAL
  :call "LOCAL Register"
  :category 'assembler-directive
  :type 'pseudo-op
  :name "ensure local"
  :description "Ensure that Register will be a local register.

This ensures that  the Register will be  a local register in  the programm. When
assembling, MMIXAL will  report an error if  the final value of  the `G' counter
does not exceed all register numbers that are declared local in this way.

For more information, see the `L' and `G' counters.")

(def-mmix-description 'BSPEC
  :call "BSPEC Expression"
  :category 'assembler-directive
  :type 'pseudo-op
  :name "begin special mode"
  :description "Begins special mode.

The Expression should have  a value that fits in two bytes.  See also `ESPEC' to
end the special mode.")

(def-mmix-description 'ESPEC
  :call "ESPEC"
  :category 'assembler-directive
  :type 'pseudo-op
  :name "end special mode"
  :description "Ends special mode.

See also `BSPEC'.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Allocating Data
;;

(def-mmix-description 'BYTE
  :call "Label BYTE ExpressionList"
  :category'allocate-data
  :type 'pseudo-op
  :name "byte"
  :description "Assemble one byte for each expression in ExpressionList.

If the Label is nonempty, Label becomes  a name for the allocated address.  Then
assemble one byte for each expression  in ExpressionList and advance the current
location by the number of bytes. The expressions should fit in one byte.")

(def-mmix-description 'WYDE
  :call "Label WYDE ExpressionList"
  :category'allocate-data
  :type 'pseudo-op
  :name "wyde"
  :description "Assemble each expression in ExpressionList as a two-byte value.

First make the  current location even by  adding 1 to it if  necessary.  Then if
the Label  is nonempty, Label  becomes a name  for the allocated  address.  Then
assemble each expression  in ExpressionList as a two-byte value  and advance the
current location by twice the number  of expressions. The expressions should fit
in two bytes.")

(def-mmix-description 'TETRA
  :call "Label TETRA ExpressionList"
  :category'allocate-data
  :type 'pseudo-op
  :name "tetra"
  :description "Assemble each expression in ExpressionList as a four-byte value.

First make the current location align to a  multiple of 4.  Then if the Label is
nonempty, Label  becomes a name for  the allocated address.  Then  assemble each
expression  in ExpressionList  as  a  four-byte value  and  advance the  current
location by 4n  if there are n  expressions. The expressions should  fit in four
bytes.")

(def-mmix-description 'OCTA
  :call "Label OCTA ExpressionList"
  :category'allocate-data
  :type 'pseudo-op
  :name "octa"
  :description "Assemble each expression in ExpressionList as a eight-byte value.

First make the current location align to a  multiple of 8.  Then if the Label is
nonempty, Label  becomes a name for  the allocated address.  Then  assemble each
expression  in ExpressionList  as a  eight-byte  value and  advance the  current
location by 8n if there are n expressions.  Any or all of the expressions may be
future references.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Loading Data
;;

(def-mmix-description 'LDB
  :call "LDB $X,$Y,$Z|Z"
  :category 'loading-data
  :type 'op
  :name "load byte"
  :description "Byte s(M[$Y + $Z]) or s(M[$Y + Z]) is loaded into register X.

The byte is loaded as a signed number between -128 and +127, inclusive."
  :hex "#80")

(def-mmix-description 'LDBU
  :call "LDBU $X,$Y,$Z|Z"
  :category 'loading-data
  :type 'op
  :name "load byte unsigned"
  :description "Byte M[$Y + $Z] or M[$Y + Z] is loaded into register X.

The byte is loaded as an unsigned number between 0 and 255, inclusive."
  :hex "#82")

(def-mmix-description 'LDW
  :call "LDW $X,$Y,$Z|Z"
  :category 'loading-data
  :type 'op
  :name "load wyde"
  :description "Bytes s(M2[$Y + $Z]) or s(M2[$Y + Z]) are loaded into register X.

The wyde is loaded as a signed  number between -32768 and +32767, inclusive. The
notation M2[k] implies that the least significant  bit of the address $Y + $Z or
$Y + Z is ignored and assumed to be 0."
  :hex "#84")

(def-mmix-description 'LDWU
  :call "LDWU $X,$Y,$Z|Z"
  :category 'loading-data
  :type 'op
  :name "load wyde unsigned"
  :description "Bytes M2[$Y + $Z] or M2[$Y + Z] are loaded into register X.

The wyde is loaded as an unsigned number between 0 and 65535, inclusive."
  :hex "#86")

(def-mmix-description 'LDT
  :call "LDT $X,$Y,$Z|Z"
  :category 'loading-data
  :type 'op
  :name "load tetra"
  :description "Bytes s(M4[$Y + $Z]) or s(M4[$Y + Z]) are loaded into register X.

The tetra is loaded as a signed number between -2,147,483,648 and +2,147,483,647
inclusive. The notation M4[k] implies that  the two least significant bit of the
address $Y + $Z or $Y + Z is ignored and assumed to be 0."
  :hex "#88")

(def-mmix-description 'LDTU
  :call "LDTU $X,$Y,$Z|Z"
  :category 'loading-data
  :type 'op
  :name "load tetra unsigned"
  :description "Bytes M4[$Y + $Z] or M4[$Y + Z] are loaded into register X.

The tetra is loaded as an unsigned number between 0 and 4,294,967,296, inclusive."
  :hex "#8A")

(def-mmix-description 'LDO
  :call "LDO $X,$Y,$Z|Z"
  :category 'loading-data
  :type 'op
  :name "load octa"
  :description "Bytes s(M8[$Y + $Z]) or s(M8[$Y + Z]) are loaded into register X.

The notation M4[k]  implies that the three least significant  bit of the address
$Y + $Z or $Y + Z is ignored and assumed to be 0."
  :hex "#8C")

(def-mmix-description 'LDOU
  :call "LDOU $X,$Y,$Z|Z"
  :category 'loading-data
  :type 'op
  :name "load octa unsigned"
  :description "Bytes M8[$Y + $Z] or M8[$Y + Z] are loaded into register X.

There is no difference  between the behavior of LDOU and  LDO, since an octabyte
can be regarded as either signed or  unsigned. LDOU is included in MMIX just for
completeness and consistency."
  :hex "#8E")

(def-mmix-description 'LDA
  :call "LDA $X,$Y,$Z|Z"
  :category 'loading-data
  :type 'alias-op
  :alias "ADDU"
  :name "load address"
  :description "The address $Y + $Z or $Y + Z is loaded into register X.

This instruction is simply a synonym for the `ADDU' instruction."
  :hex "#22")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Storing Data
;;

(def-mmix-description 'STB
  :call "STB $X,$Y,$Z|Z"
  :category 'storing-data
  :type 'op
  :name "store byte"
  :description "The least significant byte of $X is tored into M[$Y+$Z|Z].

The  least signifificant  byte of  register X  is stored  into byte  M[$Y+$Z] or
M[$Y+Z].

* If $X is not between −128 and +127, an integer overﬂow exception occurs."
  :hex "#A0")

(def-mmix-description 'STBU
  :call "STBU $X,$Y,$Z|Z"
  :category 'storing-data
  :type 'op
  :name "store byte unsigned"
  :description "The least significant byte of $X is tored into M[$Y+$Z|Z].

The  least significant  byte  of register  X  is stored  into  byte M[$Y+$Z]  or
M[$Y+Z].  STBU  instructions are the  same as  STB instructions, except  that no
test for overﬂow is made."
  :hex "#A2")

(def-mmix-description 'STW
  :call "STW $X,$Y,$Z|Z"
  :category 'storing-data
  :type 'op
  :name "store wyde"
  :description "The two least significant bytes $X are stored into M2[$Y+$Z|Z].

The two least significant bytes of register $X are stored into bytes M2[$Y+$Z] or
M2[$Y+Z].

* If $X is not between −32768 and +32767, an integer overflow exception occurs."
  :hex "#A4")

(def-mmix-description 'STWU
  :call "STWU $X,$Y,$Z|Z"
  :category 'storing-data
  :type 'op
  :name "store wyde unsigned"
  :description "The two least significant bytes of $X are stored into  M2[$Y+$Z|Z].

The two least significant bytes of register $X are stored into bytes M2[$Y+$Z] or
M2[$Y+Z]. STWU instructions are the same as STW instructions, except that no test
for overflow is made."
  :hex "#A6")

(def-mmix-description 'STT
  :call "STT $X,$Y,$Z|Z"
  :category 'storing-data
  :type 'op
  :name "store tetra"
  :description "The four least significant bytes of $X are stored into M4[$Y+$Z|Z].

The four least significant bytes of register $X are stored into bytes M4[$Y+$Z] or
M4[$Y+Z].

* If $X is not between −2,147,483,648 and +2,147,483,647, an integer overflow
  exception occurs."
  :hex "#A8")

(def-mmix-description 'STTU
  :call "STTU $X,$Y,$Z|Z"
  :category 'storing-data
  :type 'op
  :name "store tetra unsigned"
  :description "The four least significant bytes of $X are stored into M4[$Y+$Z|Z].

The four least significant bytes of register $X are stored into bytes M4[$Y+$Z] or
M4[$Y+Z]. STTU instructions are the same as STT instructions, except that no test
for overflow is made."
  :hex "#AA")

(def-mmix-description 'STO
  :call "STO $X,$Y,$Z|Z"
  :category 'storing-data
  :type 'op
  :name "store octa"
  :description "$X is stored into M8[$Y+$Z|Z].

Register $X is stored into bytes M8[$Y+$Z] or M8[$Y+Z]."
  :hex "#AC")

(def-mmix-description 'STOU
  :call "STOU $X,$Y,$Z|Z"
  :category 'storing-data
  :type 'op
  :name "store octa unsigned"
  :description "$X is stored into M8[$Y+$Z|Z].

Identical to STO $X,$Y,$Z|Z."
  :hex "#AE")

(def-mmix-description 'STSF
  :call "STSF $X,$Y,$Z|Z"
  :category 'storing-data
  :type 'op
  :name "store short float"
  :description "The short float in $X is stored into M4[$Y+$Z|Z].

The short-float contents of register $X, rounded to a 32-bit floating point, is stored
into the four bytes beginning at address M[$Y + $Z] or M[$Y + Z]. Rounding is done
with the current rounding mode. Floating overflow, underflow and inexact exceptions
might occur."
  :hex "#B0")

(def-mmix-description 'STHT
  :call "STHT $X,$Y,$Z|Z"
  :category 'storing-data
  :type 'op
  :name "store high tetra"
  :description "The most significant four bytes of $X are stored into M4[$Y+$Z|Z].

The most significant four bytes of register $X are stored into M4[$Y+$Z] or M4[$Y+Z]."
  :hex "#B2")

(def-mmix-description 'STCO
  :call "STCO $X,$Y,$Z|Z"
  :category 'storing-data
  :type 'op
  :name "store constant octabyte"
  :description "An octabyte whose value is unsigned byte $X is stored into M8[$Y+$Z|Z].

An octabyte whose value is the unsigned byte $X is stored into M8[$Y+$Z] or M8[$Y+Z]."
  :hex "#B4")

(def-mmix-description 'STUNC
  :call "STUNC $X,$Y,$Z|Z"
  :category 'storing-data
  :type 'op
  :name "store octa uncached"
  :description "$X is stored into M8[$Y+$Z|Z], uncached.

This instruction is otherwise identical to STO."
  :hex "#BE")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Setting Registers
;;

(def-mmix-description 'SET
  :call "SET $X,$Y|Y"
  :category 'setting-register
  :type 'alias-op
  :alias "OR or SETL"
  :name "set register"
  :description "Sets register X to register Y.

The case in which SET $X,$Y is equivalent  to OR $X,$Y,0 and sets the register X
to  register Y.  The case  in  which SET  $X,Y (thus  Y  is not  a register)  is
equivalent to SETL $X,Y.")

(def-mmix-description 'SETH
  :call "SETH $X,YZ"
  :category 'setting-register
  :type 'op
  :name "set to high wyde"
  :description "16-bit unsigned YZ is shifted left by 48 bits and set into register X."
  :hex "#E0")

(def-mmix-description 'GET
  :call "GET $X,rZ"
  :category 'setting-register
  :type 'op
  :name "get from special register"
  :description "Register X is set to the special register rZ.

The value of special register rZ is placed in general register $X.

  * An illegal instruction interrupt occurs if Z ≥ 32."
  :hex "#FE")

(def-mmix-description 'GETA
  :call "GETA $X,Label"
  :category 'setting-register
  :type 'op
  :name "get address"
  :description "Compute a relative address and move the result into $X.

The value λ  + 4YZ or λ  + 4(YZ − 2^16)  is placed in register  X. (The assembly
language conventions  of branch  instructions apply. For  example, we  can write
GETA $X,Addr.)"
  :hex "#F4")

(def-mmix-description 'PUT
  :call "PUT X,$Z|Z"
  :category 'setting-register
  :type 'op
  :name "put into special register"
  :description "Put Z into the special register X.

The special register identiﬁed  by X is set to the contents of  register Z or to
the unsigned byte Z itself.

Some changes are not allowed:
  * Bits of rA that are always zero must remain zero;
  * the leading  seven bytes  of rG  and rL must  remain zero,  and rL  must not
    exceed rG;
  * special registers 9–11 (namely rN, rO, and rS) must not change;
  * special registers 8 and  12–18 (namely rC, rI, rK, rQ, rT,  rU, rV, and rTT)
    can be changed only if the privilege bit of rK is zero;
  * and certain  bits of rQ  (depending on  available hardware) might  not allow
    software to  change them from  0 to  1. Moreover, any  bits of rQ  that have
    changed from 0 to  1 since the most recent GET x,rQ will  remain 1 after PUT
    rQ,z.
The PUT command will  not increase rL; it sets rL to the  minimum of the current
value and the new value.  (A program should say SETL $99,0 instead of PUT rL,100
when rL is known to be less than 100.)

Impermissible PUT  commands cause an  illegal instruction interrupt, or  (in the
case of rC, rI, rK, rQ, rT, rU, rV, and rTT) a privileged operation interrupt."
  :hex "#F6")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Integer Arithmetic
;;

(def-mmix-description 'ADD
  :call "ADD $X,$Y,$Z|Z"
  :category 'integer-arithmetic
  :type 'op
  :name "add"
  :description "$X becomes the signed sum of $Y and $Z|Z.

The contents of register X becomes the signed sum of the number in register Y
and the number in register Z or the unsigned byte Z. An integer overflow
exception can occur, as with MUL or SUB, if the result is less than -2^63 or
greater than 2^63 − 1."
  :hex "#20")

(def-mmix-description 'ADDU
  :call "ADDU $X,$Y,$Z|Z"
  :category 'integer-arithmetic
  :type 'op
  :name "add unsigned"
  :description "$X becomes the unsigned sum of $Y and $Z|Z.

The contents of register X becomes ($Y + $Z) mod 2^64 or ($Y + Z) mod 2^64. This
instruction will not trigger an overflow exception. Overflow could be detected
by using the command CMPU ovflo, $X, $Y."
  :hex "#22")

(def-mmix-description '2ADDU
  :call "2ADDU $X,$Y,$Z|Z"
  :category 'integer-arithmetic
  :type 'op
  :name "times 2 and add unsigned"
  :description "$X becomes the unsigned sum of 2$Y and $Z|Z.

The sum (2$Y + $Z) mod 2^64 or (2$Y + Z) mod 2^64 is placed into register X.
This instruction will not trigger an overflow exception."
  :hex "#28")

(def-mmix-description '4ADDU
  :call "4ADDU $X,$Y,$Z|Z"
  :category 'integer-arithmetic
  :type 'op
  :name "times 4 and add unsigned"
  :description "$X becomes the unsigned sum of 4$Y and $Z|Z.

The sum (4$Y + $Z) mod 2^64 or (4$Y + Z) mod 2^64 is placed into register X.
This instruction will not trigger an overflow exception."
  :hex "#2A")

(def-mmix-description '8ADDU
  :call "8ADDU $X,$Y,$Z|Z"
  :category 'integer-arithmetic
  :type 'op
  :name "times 8 and add unsigned"
  :description "$X becomes the unsigned sum of 8$Y and $Z|Z.

The sum (8$Y + $Z) mod 2^64 or (8$Y + Z) mod 2^64 is placed into register X.
This instruction will not trigger an overflow exception."
  :hex "#2C")

(def-mmix-description '16ADDU
  :call "16ADDU $X,$Y,$Z|Z"
  :category 'integer-arithmetic
  :type 'op
  :name "times 16 and add unsigned"
  :description "$X becomes the unsigned sum of 16$Y and $Z|Z.

The sum (16$Y + $Z) mod 2^64 or (16$Y + Z) mod 2^64 is placed into register X.
This instruction will not trigger an overflow exception."
  :hex "#2E")

(def-mmix-description 'CMP
  :call "CMP $X,$Y,$Z|Z"
  :category 'integer-arithmetic
  :type 'op
  :name "compare"
  :description "Set $X to -1|0|+1, depending on the signed comparison of $Y and $Z|Z.

The contents of register X is set to -1 if $Y < $Z, 0 if $Y = $Z, and +1 if
$Y > $Z. The comparison is performed on signed 64-bit values."
  :hex "#30")

(def-mmix-description 'CMPU
  :call "CMPU $X,$Y,$Z|Z"
  :category 'integer-arithmetic
  :type 'op
  :name "compare unsigned"
  :description "Set $X to -1|0|+1, depending on the unsigned comparison of $Y and $Z|Z.

The contents of register X is set to -1 if $Y < $Z, 0 if $Y = $Z, and +1 if
$Y > $Z. The comparison is performed on unsigned 64-bit values."
  :hex "#32")

(def-mmix-description 'SUB
  :call "SUB $X,$Y,$Z|Z"
  :category 'integer-arithmetic
  :type 'op
  :name "subtract"
  :description "$X becomes the signed difference of $Y and $Z|Z.

The difference $Y - $Z or $Y - Z is placed into register X using signed, two’s
complement arithmetic.

*  An integer overflow exception occurs if the difference is >= 2^63 or < -2^63."
  :hex "#24")

(def-mmix-description 'SUBU
  :call "SUBU $X,$Y,$Z|Z"
  :category 'integer-arithmetic
  :type 'op
  :name "subtract unsigned"
  :description "$X becomes the unsigned difference of $Y and $Z|Z.

The difference ($Y - $Z) mod 2^64 or ($Y - Z) mod 2^64 is placed into register X.
These two instructions are the same as SUB $X,$Y,$Z|Z except that no test for
overflow is made."
  :hex "#26")

(def-mmix-description 'NEG
  :call "NEG $X,Y,$Z|Z"
  :category 'integer-arithmetic
  :type 'op
  :name "negate"
  :description "$X becomes the value Y - $Z|Z is placed into register X.

The value Y − $Z or Y − Z is placed into register X using signed, two’s complement
arithmetic.

*  An integer overflow exception occurs if the result is greater than 2^63 - 1.
*  Notice that in this case MMIX works with the immediate constant Y,
   not register Y."
  :hex "#34")

(def-mmix-description 'NEGU
  :call "NEGU $X,Y,$Z|Z"
  :category 'integer-arithmetic
  :type 'op
  :name "negate unsigned"
  :description "$X becomes the value (Y - $Z|Z) mod 2^64.

NEGU instructions are the same as NEG instructions, except that no test for
overflow is made."
  :hex "#36")

(def-mmix-description 'MUL
  :call "MUL $X,$Y,$Z|Z"
  :category 'integer-arithmetic
  :type 'op
  :name "multiply"
  :description "$X becomes the signed product of $Y by $Z|Z.

The  contens of  of register  X  becomes the  signed  product of  the number  in
register Y  and the  number in  register Z or  the unsigned  byte Z.  An integer
overﬂow exception  can occur, as  with ADD  or SUB, if  the result is  less than
−2^63 or greater than 2^63 − 1.

Immediate multiplication by powers  of 2 can be done more  rapidly with the `SL'
instruction."
  :hex "#18")

(def-mmix-description 'MULU
  :call "MULU $X,$Y,$Z|Z"
  :category 'integer-arithmetic
  :type 'op
  :name "multiply unsigned"
  :description "$X becomes the product of the lower 64 bits of $Y by $Z|Z.

The lower  64 bits  of the  unsigned 128-bit  product of  register Y  and either
register Z or  Z are placed in register  X, and the upper 64 bits  are placed in
the special himult register rH.

Immediate multiplication by  powers of 2 can  be done more rapidly  with the SLU
instruction, if the  upper half is not needed. Furthermore,  an instruction like
4ADDU $X,$Y,$Y is faster than MULU $X,$Y,5."
  :hex "#1A")

(def-mmix-description 'DIV
  :call "DIV $X,$Y,$Z|Z"
  :category 'integer-arithmetic
  :type 'op
  :name "divide"
  :description "$X becomes the signed quotient of $Y and $Z|Z.

The signed quotient of the number in  register Y divided by either the number in
register Z or the  unsigned byte Z replaces the contents of  register X, and the
signed remainder is placed in the special remainder register rR.

* If the divisor is zero, an integer divide check exception occurs. In that case
  $X is set to zero and rR is set to $Y.

* If  the  number  −2^63  is  divided   by  −1,  an  integer  overﬂow  exception
  occurs. Otherwise integer overﬂow is impossible."
  :hex "#1C")

(def-mmix-description 'DIVU
  :call "DIVU $X,$Y,$Z|Z"
  :category 'integer-arithmetic
  :type 'op
  :name "divide unsigned"
  :description "$X becomes the quotient of prefixing rD to $Y and $Z|Z.

The unsigned 128-bit  number obtained by preﬁxing the  special dividend register
rD to  the contents of register  Y is divided  either by the unsigned  number in
register Z  or by the unsigned  byte Z, and  the quotient is placed  in register
X. The remainder is placed in the remainder register rR.

* If  rD is  greater than  or equal  to the  divisor (and  in particular  if the
  divisor is zero), then $X is set to rD and rR is set to $Y.

* Unsigned arithmetic never signals an exceptional condition, even when dividing
  by zero."
  :hex "#1E")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Bitwise Operations
;;

(def-mmix-description 'AND
  :call "AND $X,$Y,$Z|Z"
  :category 'bitwise-operation
  :type 'op
  :name "bitwise and"
  :description "Each bit of $Y is anded with the bit of $Z|Z and is placed in $X.

Each bit of register Y is logically anded with the corresponding bit of
register Z or of the constant Z, and the result is placed in register X. In
other words, a bit of register X is set to 1 if and only if the corresponding
bits of the operands are both 1; in symbols, $X = $Y ∧ $Z or $X = $Y ∧ Z.
This means in particular that AND $X,$Y,Z always zeroes out the seven most
significant bytes of register X, because 0s are prefixed to the constant byte Z."
  :hex "#C8")

(def-mmix-description 'OR
  :call "OR $X,$Y,$Z|Z"
  :category 'bitwise-operation
  :type 'op
  :name "bitwise or"
  :description "Each bit of $Y is ored with the bit of $Z|Z, and is placed in $X.

Each bit of register Y is logically ored with the corresponding bit of
register Z or of the constant Z, and the result is placed in register X. In
other words, a bit of register X is set to 0 if and only if the corresponding
bits of the operands are both 0; in symbols, $X = $Y ∨ $Z or $X = $Y ∨ Z.
In the special case Z = 0, the immediate variant of this command simply copies
register Y to register X. The MMIX assembler allows us to write ‘SET $X,$Y’ as
a convenient abbreviation for ‘OR $X,$Y,0’."
  :hex "#C0")

(def-mmix-description 'XOR
  :call "XOR $X,$Y,$Z|Z"
  :category 'bitwise-operation
  :type 'op
  :name "bitwise exclusive-or"
  :description "Each bit of $Y is xored with the bit of $Z|Z, and is placed in $X.

Each bit of register Y is logically xored with the corresponding bit of
register Z or of the constant Z, and the result is placed in register X. In
other words, a bit of register X is set to 0 if and only if the corresponding
bits of the operands are equal; in symbols, $X = $Y ⊕ $Z or $X = $Y ⊕ Z."
  :hex "#C6")

(def-mmix-description 'ANDN
  :call "ANDN $X,$Y,$Z|Z"
  :category 'bitwise-operation
  :type 'op
  :name "bitwise and-not"
  :description "$Y is anded with the complement of $Z or Z, and is placed in $X.

Each bit of register Y is logically anded with the complement of the
corresponding bit of register Z or of the constant Z, and the result is placed
in register X. In other words, a bit of register X is set to 1 if and only if
the corresponding bit of register Y is 1 and the other corresponding bit is 0;
in symbols, $X = $Y \ $Z or $X = $Y \ Z. This is the logical difference
operation; if the operands are bit strings representing sets, we are computing
the elements that lie in one set but not the other."
  :hex "#CA")

(def-mmix-description 'ORN
  :call "ORN $X,$Y,$Z|Z"
  :category 'bitwise-operation
  :type 'op
  :name "bitwise or-not"
  :description "$Y is ored with the complement of $Z or Z, and is placed in $X.

Each bit of register Y is logically ored with the complement of the
corresponding bit of register Z or of the constant Z, and the result is placed
in register X. In other words, a bit of register X is set to 1 if and only if
the corresponding bit of register Y is greater than or equal to the other
corresponding bit; in symbols, $X = $Y ∨ ¬$Z or $X = $Y ∨ ¬Z. This is the
complement of $Z \ $Y or Z \ $Y."
  :hex "#C2")

(def-mmix-description 'NAND
  :call "NAND $X,$Y,$Z|Z"
  :category 'bitwise-operation
  :type 'op
  :name "bitwise not-and"
  :description "$Y is anded with $Z or Z, and the complement is placed in $X.

Each bit of register Y is logically anded with the corresponding bit of register
Z or of the constant Z, and the complement of the result is placed in register
X. In other words, a bit of register X is set to 0 if and only if the
corresponding bits of the operands are both 1; in symbols, $X = ¬($Y ∧ $Z) or
$X = ¬($Y ∧ Z)."
  :hex "#CC")

(def-mmix-description 'NXOR
  :call "NXOR $X,$Y,$Z|Z"
  :category 'bitwise-operation
  :type 'op
  :name "bitwise not-exclusive-or"
  :description "$Y is xored with $Z or Z, and the complement is placed in $X.

Each bit of register Y is logically xored with the corresponding bit of register
Z or of the constant Z, and the complement of the result is placed in register
X. In other words, a bit of register X is set to 1 if and only if the
corresponding bits of the operands are equal; in symbols, $X = ¬($Y ⊕ $Z) or
$X = ¬($Y ⊕ Z)."
  :hex "#CE")

(def-mmix-description 'MUX
  :call "MUX $X,$Y,$Z|Z"
  :category 'bitwise-operation
  :type 'op
  :name "bitwise multiplex"
  :description "Set jth bit of $X to jth bit of $Y or $Z|Z, depending on mask rM.

For each bit position j, the jth bit of register X is set either to bit j of
register Y or to bit j of the other operand, $Z or Z, depending on whether bit j
of the special mask register rM is 1 or 0. If the jth bit of rM is 1, the jth
bit of $X is set to the jth bit of $Y, else it is set to the jth bit of $Z or
Z. In symbols, $X = ($Y ∧ rM) ∨ ($Z ∧ rM) or $X = ($Y ∧ rM) ∨ (Z ∧ rM)."
  :hex "#D8")

(def-mmix-description 'ORH
  :call "ORH $X,YZ"
  :category 'bitwise-operation
  :type 'op
  :name "bitwise or with high wyde"
  :description "16-bit unsigned YZ is shifted left by 48 bits and is ored with X.

The 16-bit unsigned number YZ is shifted left by 48 bits, and ored with register
X. The result is placed into register X."
  :hex "#E8")

(def-mmix-description 'ORMH
  :call "ORMH $X,YZ"
  :category 'bitwise-operation
  :type 'op
  :name "bitwise or with medium high wyde"
  :description "16-bit unsigned YZ is shifted left by 32 bits and is ored with X.

The 16-bit unsigned number YZ is shifted left by 32 bits, and ored with register
X. The result is placed into register X."
  :hex "#E9")

(def-mmix-description 'ORML
  :call "ORML $X,YZ"
  :category 'bitwise-operation
  :type 'op
  :name "bitwise or with medium low wyde"
  :description "16-bit unsigned YZ is shifted left by 16 bits and is ored with X.

The 16-bit unsigned number YZ is shifted left by 16 bits, and ored with register
X. The result is placed into register X."
  :hex "#EA")

(def-mmix-description 'ORL
  :call "ORL $X,YZ"
  :category 'bitwise-operation
  :type 'op
  :name "bitwise or with low wyde"
  :description "16-bit unsigned YZ is ored with register X and placed in X."
  :hex "#EB")

(def-mmix-description 'SL
  :call "SL $X,$Y,$Z|Z"
  :category 'bitwise-operation
  :type 'op
  :name "shift left"
  :description "The bits of Y are shifted left by $Z places and placed in X.

The bits of register Y are shifted left by $Z or Z places, and 0s are shifted in
from the right. The  result is placed in register X. Register Y  is treated as a
signed number, but value of $Z or Z  is treated as an unsigned number. The efect
is the same  as multiplication by 2^$Z  or by 2^Z. An  integer overﬂow exception
occurs if the result is  >= 2^63 or < −2^63 . In particular,  if the value of $Z
or Z is  64 or more, register  X will become entirely zero,  and integer overﬂow
will be signaled unless register Y was zero."
  :hex "#38")

(def-mmix-description 'SLU
  :call "SL $X,$Y,$Z|Z"
  :category 'bitwise-operation
  :type 'op
  :name "shift left unsigned"
  :description "The bits of Y are shifted left by $Z places and placed in X.

The bits of register Y are shifted left by $Z or Z places, and 0s are shifted in
from the right. The result is placed in register X. Both operands are treated as
unsigned numbers. The SLU instructions are equivalent to SL, except that no test
for overﬂow is made."
  :hex "#3A")

(def-mmix-description 'SR
  :call "SR $X,$Y,$Z|Z"
  :category 'bitwise-operation
  :type 'op
  :name "shift right"
  :description "The bits of Y are shifted right by $Z or Z places and placed in X.

The bits of register Y are shifted right by $Z or Z places, and copies of the
leftmost bit (the sign bit) are shifted in from the left. The result is placed
in register X. Register Y is treated as a signed number, but the second operand
is treated as an unsigned number. The effect is the same as division by 2 $Z or
by 2Z and rounding down. In particular, if the second operand is 64 or more,
register X will become zero if $Y was nonnegative, -1 if $Y was negative."
  :hex "#3C")

(def-mmix-description 'SRU
  :call "SRU $X,$Y,$Z|Z"
  :category 'bitwise-operation
  :type 'op
  :name "shift right unsigned"
  :description "The bits of Y are shifted right by $Z or Z places and placed in X.

The bits of register Y are shifted right by $Z or Z places, and 0s are shifted
in from the left. The result is placed in register X. Both operands are treated
as unsigned numbers. The effect is the same as unsigned division of a 64-bit
number by 2^$Z or by 2^Z. If the second operand is 64 or more, register X will
become entirely zero."
  :hex "#3E")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Floating Point Arithmetic
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Jumps and Branches
;;


(def-mmix-description 'JMP
  :call "JMP Label"
  :category 'jump
  :type 'op
  :name "jump"
  :description "Jump unconditionally to Label.

A  JMP command  treats bytes  X, Y,  and Z  as an  unsigned 24-bit  integer XYZ,
usually computer by the assembler. It  allows a program to transfer control from
location λ to any location between λ  − 67,108,864 and λ + 67,108,860 inclusive,
using relative addressing  as in the B  and PB commands. For  even bigger jumps,
the GO instruction can be used."
  :hex "#F0")

(def-mmix-description 'BN
  :call "BN $X,Label"
  :category 'branch
  :type 'op
  :name "branch if negative"
  :description "Branch to Label if register $X is negative.

The signed contents of register $X is tested. If it is negative, the program
jumps to the address specified by Label. The assembler computes the necessary
relative offset to the Label from the current instruction's location, which
can be referenced with the '@' symbol."
  :hex "#40")

(def-mmix-description 'BZ
  :call "BZ $X,Label"
  :category 'branch
  :type 'op
  :name "branch if zero"
  :description "Branch to Label if register $X is zero.

The signed contents of register $X is tested. If it is zero, the program
jumps to the address specified by Label. The assembler computes the necessary
relative offset to the Label from the current instruction's location, which
can be referenced with the '@' symbol."
  :hex "#42")

(def-mmix-description 'BP
  :call "BP $X,Label"
  :category 'branch
  :type 'op
  :name "branch if positive"
  :description "Branch to Label if register $X is positive.

The signed contents of register $X is tested. If it is positive (and not zero),
the program jumps to the address specified by Label. The assembler computes the
necessary relative offset to the Label from the current instruction's location,
which can be referenced with the '@' symbol."
  :hex "#44")

(def-mmix-description 'BOD
  :call "BOD $X,Label"
  :category 'branch
  :type 'op
  :name "branch if odd"
  :description "Branch to Label if register $X is odd.

The signed contents of register $X is tested. If it is odd, the program
jumps to the address specified by Label. The assembler computes the necessary
relative offset to the Label from the current instruction's location, which
can be referenced with the '@' symbol."
  :hex "#46")

(def-mmix-description 'BNN
  :call "BNN $X,Label"
  :category 'branch
  :type 'op
  :name "branch if non-negative"
  :description "Branch to Label if register $X is non-negative.

The signed contents of register $X is tested. If it is non-negative (zero or
positive), the program jumps to the address specified by Label. The assembler
computes the necessary relative offset to the Label from the current
instruction's location, which can be referenced with the '@' symbol."
  :hex "#48")

(def-mmix-description 'BNZ
  :call "BNZ $X,Label"
  :category 'branch
  :type 'op
  :name "branch if non-zero"
  :description "Branch to Label if register $X is non-zero.

The signed contents of register $X is tested. If it is not zero, the program
jumps to the address specified by Label. The assembler computes the necessary
relative offset to the Label from the current instruction's location, which
can be referenced with the '@' symbol."
  :hex "#4a")

(def-mmix-description 'BNP
  :call "BNP $X,Label"
  :category 'branch
  :type 'op
  :name "branch if non-positive"
  :description "Branch to Label if register $X is non-positive.

The signed contents of register $X is tested. If it is non-positive (zero or negative),
the program jumps to the address specified by Label. The assembler computes the necessary
relative offset to the Label from the current instruction's location, which
can be referenced with the '@' symbol."
  :hex "#4c")

(def-mmix-description 'BEV
  :call "BEV $X,Label"
  :category 'branch
  :type 'op
  :name "branch if even"
  :description "Branch to Label if register $X is even.

The signed contents of register $X is tested. If it is even, the program
jumps to the address specified by Label. The assembler computes the necessary
relative offset to the Label from the current instruction's location, which
can be referenced with the '@' symbol."
  :hex "#4e")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Subroutines
;;


(def-mmix-description 'PUSHJ
  :call "PUSHJ $X,Label"
  :category 'subroutine
  :type 'op
  :name "push registers and jump"
  :description "Transfer control to the adress specified by Label.

Transfer control  to the adress specified  by Label, a 16  bit relative address,
similar to  a `JMP' instruction.  The address  of the instruciton  following the
`PUSHJ' is  placed in register  `rJ' which is used  by the `POP'  instruction to
return to address `rJ + 4*YZ' (`YZ' is typically 0).

TODO: Improve as my understanding improves."
  :hex "#F2")

(def-mmix-description 'POP
  :call "POP X,YZ"
  :category 'subroutine
  :type 'op
  :name "pop registers and return from subroutine"
  :description "Pop the registers and return from the last subroutine.

This command  preserves X of the  current local registers, undoes  the effect of
the most recent `PUSHJ' or `PUSHGO', and jumps to the instruction in M4[4YZ + rJ].
If X >  0, the value of  $(X − 1) goes  into the hole position  where `PUSHJ' or
`PUSHGO' stored the number of registers previously pushed.

TODO: Improve as my understanding improves."
  :hex "#F8")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Calling the Operating System
;;

(def-mmix-description 'TRAP
  :call "TRAP X,Y,Z"
  :category 'interrupts
  :type 'op
  :name "trap"
  :description "Interrupt and transfer control to an operating system handler.

The  X, Y  and Z  fields of  TRAP have  special significance  predefined by  the
operating system kernel. For example:
  * If XYZ = 0, the user process should terminate.
  * TRAP 0,Halt,0 is typically written to make the main programm end.
See also `TRIP' and `RESUME'."
  :hex "#00")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The mode authors favorite
;;

(def-mmix-description 'SWYM
  :call "SWYM X,Y,Z"
  :category 'other
  :type 'op
  :name "sympathize with your machinery"
  :description "This instruction does nothing.

The X, Y, and Z fields are ignored. Its purpose is to let a programmer express
sympathy for the machinery, which might be working too hard. On a pipelined
implementation, it might briefly give the machine a chance to catch up. A `SWYM`
can also be used as a breakpoint that is easy to spot."
  :hex "#FD")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Registers
;;


(def-mmix-description 'rA
  :type 'register
  :name "arithmetic status register"
  :code 21)

(def-mmix-description 'rB
  :type 'register
  :name "bootstrap register"
  :code 00)

(def-mmix-description 'rC
  :type 'register
  :name "continuation register"
  :code 08)

(def-mmix-description 'rD
  :type 'register
  :name "dividend register"
  :code 01)

(def-mmix-description 'rE
  :type 'register
  :name "epsilon register"
  :code 02)

(def-mmix-description 'rF
  :type 'register
  :name "failure location register"
  :code 22)

(def-mmix-description 'rG
  :type 'register
  :name "global threshold register"
  :code 19)

(def-mmix-description 'rH
  :type 'register
  :name "himult register"
  :code 03)

(def-mmix-description 'rI
  :type 'register
  :name "interval counter"
  :code 12)

(def-mmix-description 'rJ
  :type 'register
  :name "return-jump register"
  :code 04)

(def-mmix-description 'rK
  :type 'register
  :name "interrupt mask register"
  :code 15)

(def-mmix-description 'rL
  :type 'register
  :name "local threshold register"
  :code 20)

(def-mmix-description 'rM
  :type 'register
  :name "multiplex mask register"
  :code 05)

(def-mmix-description 'rN
  :type 'register
  :name "serial number"
  :code 09)

(def-mmix-description 'rO
  :type 'register
  :name "register stack offset"
  :code 10)

(def-mmix-description 'rP
  :type 'register
  :name "prediction register"
  :code 23)

(def-mmix-description 'rQ
  :type 'register
  :name "interrupt request register"
  :code 16)

(def-mmix-description 'rR
  :type 'register
  :name "remainder register"
  :code 06)

(def-mmix-description 'rS
  :type 'register
  :name "register stack pointer"
  :code 11)

(def-mmix-description 'rT
  :type 'register
  :name "trap address register"
  :code 13)

(def-mmix-description 'rU
  :type 'register
  :name "usage counter"
  :code 17)

(def-mmix-description 'rV
  :type 'register
  :name "virtual translation register"
  :code 18)

(def-mmix-description 'rW
  :type 'register
  :name "where-interrupted register (user)"
  :code 24)

(def-mmix-description 'rX
  :type 'register
  :name "execution register (user)"
  :code 25)

(def-mmix-description 'rY
  :type 'register
  :name "Y operand (user)"
  :code 26)

(def-mmix-description 'rZ
:type 'register
:name "Z operand (user)"
:code 27)

(def-mmix-description 'rBB
:type 'register
:name "bootstrap register (kernel)"
:code 07)

(def-mmix-description 'rTT
:type 'register
:name "dynamic trap address register"
:code 14)

(def-mmix-description 'rWW
:type 'register
:name "where-interrupted register (kernel)"
:code 28)

(def-mmix-description 'rXX
:type 'register
:name "execution register (kernel)"
:code 29)

(def-mmix-description 'rYY
:type 'register
:name "Y operand (kernel)"
:code 30)

(def-mmix-description 'rZZ
:type 'register
:name "Z operand (kernel)"
:code 31)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Code
;;


(defun mmix-description-symbol-name (description)
  "Return DESCRIPTION's symbol as a string."
  (symbol-name (mmix-description-symbol description)))

(defun mmix-describe-summary (description)
  "Format DESCRIPTION's summary."
  (let ((symbol-name (mmix-description-symbol-name description)))
    (cl-case (mmix-description-type description)
      ('op (format "%s is a MMIX opcode\n\n" symbol-name))
      ('alias-op (format "%s is a MMIX alias for %s\n\n"
			 symbol-name
			 (mmix-description-alias description)))
      ('pseudo-op (format "%s is a MMIX pseudo opcode\n\n" symbol-name))
      ('register (format "%s is a MMIX register\n\n" symbol-name)))))

(defun mmix-describe-heading (heading)
  "Format the string HEADING."
  (format "%s\n" heading))

(defun mmix-describe-signature (description)
  "Format DESCRIPTION's signature."
  (format "%s\n\n" (mmix-description-call description)))

(defun mmix-describe-category (description)
  "Format DESCRIPTION's signature."
  (format "%s\n\n" (alist-get (mmix-description-category description)
			      mmix-description-categories-alist
			      "No valid category.")))

(defun mmix-describe-description (description)
  "Format DESCRIPTION's description."
  (format "%s\n\n" (mmix-description-description description)))

(defun mmix-describe-name (description)
  "Format DESCRIPTION's name."
  (format "%s\n\n" (mmix-description-name description)))

(defun mmix-describe-hex (description)
  "Format DESCRIPTION's hexadecimal value."
  (format "%s\n\n" (mmix-description-hex description)))

(defun mmix-describe-register-1line (description)
  "Format DESCRIPTION to display in one line."
  (format "%-3s %-35s %2s\n"
	  (mmix-description-symbol description)
	  (mmix-description-name description)
	  (mmix-description-code description)))

(defun mmix-describe-instruction-1line (description)
  "Format instruction DESCRIPTION to display in one line."
  (format "%-4s %-8s %-35s\n"
	  (mmix-description-hex description)
	  (mmix-description-symbol description)
	  (mmix-description-name description)))


;;;###autoload
(defun mmix-describe (symbol)
  "Display the documentation of SYMBOL."
  (interactive
   (list
    (let* ((current-symbol (current-word))
	   (have-default (member current-symbol mmix-description-symbol-name-list))
	   (prompt (concat "MMIX Symbol"
			   (when have-default (concat " (default " current-symbol ")"))
			   ": ")))
    (completing-read prompt
		     mmix-description-symbol-name-list
		     nil
		     t
		     nil
		     nil
		     current-symbol))))
  (let* ((symbol (intern-soft symbol))
	 (description (gethash symbol mmix-description-table)))
    (with-output-to-temp-buffer (buffer-name (get-buffer-create "*Help*"))
      (princ (mmix-describe-summary description))
      (when (mmix-description-call description)
	(princ (mmix-describe-heading "Signature"))
	(princ (mmix-describe-signature description)))
      (when (mmix-description-category description)
	(princ (mmix-describe-heading "Category"))
	(princ (mmix-describe-category description)))
      (when (mmix-description-name description)
	(princ (mmix-describe-heading "Name"))
	(princ (mmix-describe-name description)))
      (when (mmix-description-description description)
	(princ (mmix-describe-heading "Documentation"))
	(princ (mmix-describe-description description)))
      (when (mmix-description-hex description)
	(princ (mmix-describe-heading "Hexadecimal value"))
	(princ (mmix-describe-hex description))))))

;;;###autoload
(defun mmix-describe-registers ()
  "Display all the special registers in a buffer.

Source: URL `https://www-cs-faculty.stanford.edu/~knuth/mmop.html'."
  (interactive)
  (with-output-to-temp-buffer (buffer-name (get-buffer-create "*Help*"))
    (dolist (register-description
	     (seq-filter (lambda (element)
			   (equal (mmix-description-type element) 'register))
			 (hash-table-values mmix-description-table)))
      (princ (mmix-describe-register-1line register-description)))))

;;;###autoload
(defun mmix-summarize-instructions ()
  "Display all the instructions in a help buffer.

Source: URL `https://www-cs-faculty.stanford.edu/~knuth/mmop.html'."
  (interactive)
  (with-output-to-temp-buffer (buffer-name (get-buffer-create "*Help*"))
    (dolist (instruction-description
	     (sort (seq-filter (lambda (element)
				 (equal (mmix-description-type element) 'op))
			       (hash-table-values mmix-description-table))
		   (lambda (d1 d2) (string< (mmix-description-symbol d1)
				       (mmix-description-symbol d2)))))
      (princ (mmix-describe-instruction-1line instruction-description)))))


(provide 'mmix-describe)

;;; mmix-describe.el ends here
