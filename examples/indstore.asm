# Indirect stores

var storer = 600
var ptr = 42
var acc = 0

bootstrap
STO $acc
CLA $storer
ADD $ptr
STO $indstor
CLA $acc
label indstor
STO 00
HRS
end
