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
	li $t2, 1
	li $t3, 1
	li $t4, 1
	li $t5, 1
	li $t6, 1
	li $t7, 1
	li $t0, 1
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	li $t0, 1
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	li $t0, 1
	subi $sp, $sp, 4
	sw $t0, 0($sp)
#__________save $t2 ~ $t7
	subi $sp, $sp, 4
	sw $t7, 0($sp)
	subi $sp, $sp, 4
	sw $t6, 0($sp)
	subi $sp, $sp, 4
	sw $t5, 0($sp)
	subi $sp, $sp, 4
	sw $t4, 0($sp)
	subi $sp, $sp, 4
	sw $t3, 0($sp)
	subi $sp, $sp, 4
	sw $t2, 0($sp)
#__________end_save
	la $t2, y
	lw $t2, 0($t2)
	subi $sp, $sp, 4
	sw $t2, 0($sp)
	la $t2, x
	lw $t2, 0($t2)
	subi $sp, $sp, 4
	sw $t2, 0($sp)
	jal add
	addi $sp, $sp, 8
	subi $sp, $sp, 4
	sw $t2, 0($sp)
#__________restore $t2 ~ $t7
	lw $t2, 0($sp)
	addi $sp, $sp, 4
	lw $t3, 0($sp)
	addi $sp, $sp, 4
	lw $t4, 0($sp)
	addi $sp, $sp, 4
	lw $t5, 0($sp)
	addi $sp, $sp, 4
	lw $t6, 0($sp)
	addi $sp, $sp, 4
	lw $t7, 0($sp)
	addi $sp, $sp, 4
#__________end_restore
	la $t0, y
	lw $t0, 0($t0)
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	li $t0, 3
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	lw $t1, 0($sp)
	addi $sp, $sp, 4
	lw $t0, 0($sp)
	addi $sp, $sp, 4
	mul $t0, $t0, $t1
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	lw $t1, 0($sp)
	addi $sp, $sp, 4
	lw $t0, 0($sp)
	addi $sp, $sp, 4
	add $t0, $t0, $t1
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	lw $t1, 0($sp)
	addi $sp, $sp, 4
	lw $t0, 0($sp)
	addi $sp, $sp, 4
	add $t0, $t0, $t1
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	lw $t1, 0($sp)
	addi $sp, $sp, 4
	lw $t0, 0($sp)
	addi $sp, $sp, 4
	add $t0, $t0, $t1
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	lw $t1, 0($sp)
	addi $sp, $sp, 4
	lw $t0, 0($sp)
	addi $sp, $sp, 4
	add $t0, $t0, $t1
	subi $sp, $sp, 4
	sw $t0, 0($sp)
	lw $t1, 0($sp)
	addi $sp, $sp, 4
	add $t7, $t7, $t1
	add $t6, $t6, $t7
	add $t5, $t5, $t6
	add $t4, $t4, $t5
	add $t3, $t3, $t4
	add $t2, $t2, $t3
	sw $t2, -8($fp)
	lw $t2, -8($fp)
	li $t3, 39
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
add:
	subi $sp, $sp, 4
	sw $fp, 0($sp)
	subi $sp, $sp, 4
	sw $ra, 0($sp)
	addi $fp, $sp, 4
	addi $sp, $sp, 0
	lw $t2, 8($fp)
	lw $t3, 4($fp)
	add $t2, $t2, $t3
	addi $sp, $fp, -4
	lw $ra, 0($sp)
	addi $sp, $sp, 4
	lw $fp, 0($sp)
	addi $sp, $sp, 4
	jr $ra
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
