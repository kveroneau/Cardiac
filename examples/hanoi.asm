# Towers of Hanoi
# Will request 2 inputs when run, they are as follows:
# Input 1: Number of disks: Try 3
# Input 2: Column ordering: Try 0

var tos = 96
var loader = 100
var storer = 600
var r2ld = *r2
var r2 = 1
var zero = 0
var five = 5
var four = 4
var three = 3
var two = 2
var t1 = 0
var refsav = 0
var pshsav = 0

bootstrap
INP 98
INP 97
JMP $tower
HRS

label ret
JMP 0

label tower
CLA 99
JMP $push
CLA $three
JMP $stkref
SUB
TAC $towdone
JMP $push
CLA $three
JMP $stkref
STO $t1
OUT $t1
CLA $three
JMP $stkref
ADD $r2ld
STO $t2
label t2
CLA 00
JMP $push
JMP $tower
JMP $pop
JMP $pop
label towdone
JMP $pop
STO $towret
label towret
JMP 00

label stkref
STO $refsav
CLA 99
STO $refret
CLA $refsav
ADD $tos
ADD $loader
STO $ref
label ref
CLA 00
label refret
JMP 00

label pop
CLA 99
STO $popret
CLA $tos
ADD
STO $tos
ADD $loader
STO $popa
label popa
CLA 00
label popret
JMP 0

label push
STO $pshsav
CLA 99
STO $pshret
CLA $tos
ADD $storer
STO $psha
CLA $tos
SUB
STO $tos
CLA $pshsav
label psha
STO 00
label pshret
JMP 0

end

