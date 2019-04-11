L8 :

li $s3, 10
li $s2, 20
li $s1, 0
L2 :

add $s0, $s3, $s2
move $s3, $s0
li $s0, 10
beq $s1, $s0, L1
b L3
L3 :

addiu $s0, $s1, 1
move $s1, $s0
L1 :

li $s3, 0
b L7
L7 :

