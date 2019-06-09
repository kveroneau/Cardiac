# Stacks

var storer = 601
var loader = 100
var tos = 89
var acc = 0

bootstrap
CLA
ADD
JMP $push
SUB
JMP $pop
HRS
label ret
JMP 0

# Stack push routine
label push
STO $acc
CLA 99
STO $ret
CLA $tos
SUB
STO $tos
ADD $storer
STO $stapsh
CLA $acc
label stapsh
STO 0
JMP $ret

# Stack pop routine
label pop
CLA 99
STO $ret
CLA $tos
ADD
STO $tos
ADD $loader
STO $stapop
label stapop
CLA 0
JMP $ret

end
