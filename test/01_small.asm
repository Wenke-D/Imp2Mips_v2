.text
	beqz $a0, init_end
	lw $a0, 0($a1)
	jal atoi
init_end:
	subi $sp, $sp, 4
	sw $v0, 0($sp)
	jal main
	li $v0, 10
	syscall
main:
	subi $sp, $sp, 4
	sw $fp, 0($sp)
	subi $sp, $sp, 4
	sw $ra, 0($sp)
	addi $fp, $sp, 4
	addi $sp, $sp, -4
	li $t2, 1
	la $t1, x
	sw $t2, 0($t1)
	li $t2, 2
	la $t1, y
	sw $t2, 0($t1)
	la $t2, y
	lw $t2, 0($t2)
	la $t3, x
	lw $t3, 0($t3)
	la $t4, y
	lw $t4, 0($t4)
	li $t5, 3
	mul $t4, $t4, $t5
	add $t3, $t3, $t4
	add $t2, $t2, $t3
	sw $t2, -8($fp)
	lw $t2, -8($fp)
	li $t3, 48
	add $t2, $t2, $t3
	move $a0, $t2
	li $v0, 11
	syscall
	li $t0, 0
	addi $sp, $fp, -4
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	lw $fp, 0($sp)
	addi $sp, $sp, 4
	jr $ra
#built-in atoi
atoi:
	li $v0, 0
atoi_loop:
	lbu $t0, 0($a0)
	beqz $t0, atoi_end
	addi $t0, $t0, -48
	bltz $t0, atoi_error
	bge $t0, 10, atoi_error
	mul $v0, $v0, 10
	add $v0, $v0, $t0
	addi $a0, $a0, 1
	b atoi_loop
atoi_error:
	li $v0, 10
	syscall
atoi_end:
	jr $ra
.data
x:
	.word 0
y:
	.word 0
