# Powers of 2 Example

var n = 0
var cntr = 9

bootstrap
CLA
JMP $aprint
label loop
STO $n
CLA $cntr
SUB
TAC $exit
STO $cntr
CLA $n
JMP $double
JMP $aprint
JMP $loop
label exit
HRS
label ret
JMP 0

label aprint
STO 86
CLA 99
STO $ret
OUT 86
CLA 86
JMP $ret

label double
STO 96
CLA 99
STO $ret
CLA 96
ADD 96
JMP $ret

end
