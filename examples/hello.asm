# A Simple Hello World program for the Cardiac!
# This is a test program to test the ASCII button in my Simulator.

var stdout = 500
var strlen = 13

bootloader

label loop
CLA $stdout
ADD $strptr
STO $outstr
label outstr
data 000
CLA $strlen
SUB 00
TAC $end
STO $strlen
CLA $strptr
ADD 00
STO $strptr
JMP $loop
label end
HRS

label strptr
INP $string
label string
data 72,101,108,108,111,32,67,97,114,100,105,97,99,33

end
