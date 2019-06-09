# Indirect loads example

var loader = 100
var ptr = 42

bootstrap
CLA $loader
ADD $ptr
STO $indload
label indload
CLA 00
HRS
end

