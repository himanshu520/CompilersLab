main:
sw $fp, 0($sp)
move $fp, $sp
addiu $sp, $sp, 36

L8 :

li $s2, 10
sw $s2, 4($s3)
li $s1, 20
li $s2, 30
li $s2, 50
li $s0, 0
L2 :

lw $s2, 4($s3)
add $s2, $s2, $s1
sw $s2, 4($s3)
li $s1, 10
beq $s0, $s1, L1
b L3
L3 :

addiu $s1, $s0, 1
move $s0, $s1
L1 :

li $s4, 0
b L7
L7 :


move $sp, $fp
lw $fp, 0($sp)
jr $ra

