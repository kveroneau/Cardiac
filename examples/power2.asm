# Powers of 2 Example

var n = 0
var cntr = 9
var a = 0
var b = 0

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
STO $a
CLA 99
STO $ret
OUT $a
CLA $a
JMP $ret

label double
STO $b
CLA 99
STO $ret
CLA $b
ADD $b
JMP $ret

end
