L12 :

li $s2, 10
li $s0, 20
L2 :

li $s1, 1
blt $s2, $s0, L4
b L5
L5 :

li $s1, 0
L4 :

beqz $s1, L1
b L3
L3 :

addiu $s1, $s2, 1
move $s2, $s1
b L2
L1 :

li $s2, 0
b L11
L11 :

