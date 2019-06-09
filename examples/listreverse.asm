# List Reveral example

var storer = 600
var loader = 100
var tos = 89
var acc = 0
var n1 = 0
var n2 = 0

bootstrap
INP $n1
CLA $n1
STO $n2
label rdlp
CLA $n2
SUB
TAC $wrlp
STO $n2
INP $acc
CLA $tos
ADD $storer
STO $stapsh
CLA $tos
SUB
STO $tos
CLA $acc
label stapsh
STO 0
JMP $rdlp
label wrlp
CLA $n1
SUB
TAC $done
STO $n1
CLA $tos
ADD
STO $tos
ADD $loader
STO $stapop
label stapop
CLA 0
JMP $aprint
JMP $wrlp
label done
HRS

label aprint
STO 96
CLA 99
STO $aexit
OUT 96
CLA 96
label aexit
JMP 0

end
