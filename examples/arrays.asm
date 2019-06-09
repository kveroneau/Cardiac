# Array Indexing

var storer = 600
var loader = 100
var base = 42
var index = 0
var acc = 0

bootloader
CLA $storer
ADD $loader
ADD $base
JMP $setarray
CLA $index
ADD
STO $index
JMP $getarray
HRS

# Array storing routine
label setarray
STO $acc
CLA 99
STO $ret0
CLA $storer
ADD $base
ADD $index
STO $arrstor
CLA $acc
label arrstor
STO 00
label ret0
JMP 0

# Array loading routine
label getarray
CLA 99
STO $ret1
CLA $loader
ADD $base
ADD $index
STO $arrload
label arrload
CLA 00
label ret1
JMP 0

end
