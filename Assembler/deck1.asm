# This deck1.txt converted over to ASM format.
# Lines which start with a hash are comments.

# Here we declare our variables in memory for use.
var count = 0
var i = 9

# This is needed to add the special Cardiac bootstrap code.
bootloader

# Program begins here.
CLA
STO $count
label loop
CLA $i
TAC $endloop
OUT $count
CLA $count
ADD
STO $count
CLA $i
SUB
STO $i
JMP $loop
label endloop
HRS

# End of program.
end
