#  The example comes courtesy of Mark and Will Tapley. It finds sets of three integers which satisfy the Pythagorian property of x2+y2=z2.
# https://www.khanacademy.org/math/recreational-math/vi-hart/vi-cool-stuff/v/what-was-up-with-pythagoras

var S = 0
var S2 = 0
var L = 0
var L2 = 0
var H = 0
var H2 = 0

var SQLIM = 32
var SQIN = 0
var SQCNT = 0
var SQOUT = 0

bootstrap

label S_Loop
CLA $S
STO $SQIN
ADD 00
STO $L
JMP $SQacc
CLA $SQOUT
STO $L2
CLA $S
SFT 01
STO $H2
ADD $H2
ADD $H2
ADD $H2
ADD $S
STO $H
JMP $SQacc
CLA $SQOUT
STO $H2

label L_Loop
CLA $S2
ADD $L2
SUB $H2
TAC $Inc_L
SUB 00
TAC $PrintTr
CLA $H
ADD 00
STO $H
STO $SQIN
SUB $SQLIM
TAC $Next_H
JMP $Next_S

label Next_H
JMP $SQmem
CLA $SQOUT
STO $H2
JMP $L_Loop

label PrintTr
OUT $S
OUT $L
OUT $H

label Inc_L
CLA $L
ADD 00
STO $L
JMP $SQacc
CLA $SQOUT
STO $L2
CLA $L
ADD $L
SUB $S2
TAC $L_Loop

label Next_S
CLA $S
ADD 00
STO $S
SUB $SQLIM
TAC $S_Loop
HRS

label SQacc
STO $SQIN

label SQmem
CLA $SQIN
SUB $SQIN
STO $SQOUT
SUB $SQIN
TAC $SQpos
STO $SQIN

label SQpos
CLA $SQIN
SUB $SQLIM
TAC $SQgood
HRS

label SQgood
CLA $SQIN
ADD 00
STO $SQCNT

label SQloop
CLA $SQCNT
SUB 00
STO $SQCNT
CLA $SQOUT
ADD $SQIN
STO $SQOUT
CLA 00
SUB $SQCNT
TAC $SQloop
RTS

end