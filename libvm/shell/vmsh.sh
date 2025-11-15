#!/bin/sh
#
# Copyright (c) 2025 PCC Project
#
# Shell Script VM Executor
#
# A minimal VM implementation in POSIX shell for bootstrapping
# on systems without a C compiler. Executes text-format bytecode.
#
# Usage: vmsh.sh program.vms

# VM state
r0=0 r1=0 r2=0 r3=0 r4=0 r5=0 r6=0 r7=0
r8=0 r9=0 r10=0 r11=0 r12=0 r13=0 sp=0 fp=0
pc=0
flag_z=0 flag_n=0 flag_c=0 flag_v=0
running=1

# Memory (associative array simulation)
mem_size=65536
stack_base=$((mem_size - 16384))
sp=$stack_base
fp=$stack_base

# Program storage
prog_lines=""
prog_count=0

# Helper functions

get_reg() {
	case "$1" in
		r0) echo $r0 ;;
		r1) echo $r1 ;;
		r2) echo $r2 ;;
		r3) echo $r3 ;;
		r4) echo $r4 ;;
		r5) echo $r5 ;;
		r6) echo $r6 ;;
		r7) echo $r7 ;;
		r8) echo $r8 ;;
		r9) echo $r9 ;;
		r10) echo $r10 ;;
		r11) echo $r11 ;;
		r12) echo $r12 ;;
		r13) echo $r13 ;;
		sp|r14) echo $sp ;;
		fp|r15) echo $fp ;;
		*) echo 0 ;;
	esac
}

set_reg() {
	case "$1" in
		r0) r0=$2 ;;
		r1) r1=$2 ;;
		r2) r2=$2 ;;
		r3) r3=$2 ;;
		r4) r4=$2 ;;
		r5) r5=$2 ;;
		r6) r6=$2 ;;
		r7) r7=$2 ;;
		r8) r8=$2 ;;
		r9) r9=$2 ;;
		r10) r10=$2 ;;
		r11) r11=$2 ;;
		r12) r12=$2 ;;
		r13) r13=$2 ;;
		sp|r14) sp=$2 ;;
		fp|r15) fp=$2 ;;
	esac
}

update_flags() {
	val=$1
	if [ "$val" -eq 0 ]; then
		flag_z=1
	else
		flag_z=0
	fi

	if [ "$val" -lt 0 ]; then
		flag_n=1
	else
		flag_n=0
	fi
}

# Memory operations (simplified - only supports small addresses)
mem_read() {
	addr=$1
	var_name="mem_$addr"
	eval "val=\${$var_name:-0}"
	echo $val
}

mem_write() {
	addr=$1
	value=$2
	var_name="mem_$addr"
	eval "$var_name=$value"
}

# Stack operations
stack_push() {
	sp=$((sp - 4))
	mem_write $sp $1
}

stack_pop() {
	val=$(mem_read $sp)
	sp=$((sp + 4))
	echo $val
}

# Load program
load_program() {
	file=$1
	line_num=0

	while IFS= read -r line || [ -n "$line" ]; do
		# Remove leading/trailing whitespace
		line=$(echo "$line" | sed 's/^[ \t]*//;s/[ \t]*$//')

		# Skip comments and empty lines
		case "$line" in
			''|'#'*|';'*) continue ;;
		esac

		# Store line
		var_name="prog_line_$line_num"
		eval "$var_name=\"$line\""
		line_num=$((line_num + 1))
	done < "$file"

	prog_count=$line_num
}

# Get program line
get_prog_line() {
	line_num=$1
	var_name="prog_line_$line_num"
	eval "echo \"\$$var_name\""
}

# Execute instruction
execute() {
	instr="$1"

	# Parse instruction
	opcode=$(echo "$instr" | awk '{print $1}')

	case "$opcode" in
		nop)
			;;

		halt)
			running=0
			;;

		ldi)
			# ldi rd, imm
			rd=$(echo "$instr" | awk '{print $2}' | tr -d ',')
			imm=$(echo "$instr" | awk '{print $3}')
			set_reg "$rd" "$imm"
			;;

		mov)
			# mov rd, rs
			rd=$(echo "$instr" | awk '{print $2}' | tr -d ',')
			rs=$(echo "$instr" | awk '{print $3}')
			val=$(get_reg "$rs")
			set_reg "$rd" "$val"
			;;

		add)
			# add rd, rs1, rs2
			rd=$(echo "$instr" | awk '{print $2}' | tr -d ',')
			rs1=$(echo "$instr" | awk '{print $3}' | tr -d ',')
			rs2=$(echo "$instr" | awk '{print $4}')
			val1=$(get_reg "$rs1")
			val2=$(get_reg "$rs2")
			result=$((val1 + val2))
			set_reg "$rd" "$result"
			update_flags $result
			;;

		sub)
			# sub rd, rs1, rs2
			rd=$(echo "$instr" | awk '{print $2}' | tr -d ',')
			rs1=$(echo "$instr" | awk '{print $3}' | tr -d ',')
			rs2=$(echo "$instr" | awk '{print $4}')
			val1=$(get_reg "$rs1")
			val2=$(get_reg "$rs2")
			result=$((val1 - val2))
			set_reg "$rd" "$result"
			update_flags $result
			;;

		mul)
			# mul rd, rs1, rs2
			rd=$(echo "$instr" | awk '{print $2}' | tr -d ',')
			rs1=$(echo "$instr" | awk '{print $3}' | tr -d ',')
			rs2=$(echo "$instr" | awk '{print $4}')
			val1=$(get_reg "$rs1")
			val2=$(get_reg "$rs2")
			result=$((val1 * val2))
			set_reg "$rd" "$result"
			update_flags $result
			;;

		div)
			# div rd, rs1, rs2
			rd=$(echo "$instr" | awk '{print $2}' | tr -d ',')
			rs1=$(echo "$instr" | awk '{print $3}' | tr -d ',')
			rs2=$(echo "$instr" | awk '{print $4}')
			val1=$(get_reg "$rs1")
			val2=$(get_reg "$rs2")
			if [ "$val2" -ne 0 ]; then
				result=$((val1 / val2))
				set_reg "$rd" "$result"
				update_flags $result
			else
				echo "Error: Division by zero" >&2
				exit 1
			fi
			;;

		addi)
			# addi rd, rs, imm
			rd=$(echo "$instr" | awk '{print $2}' | tr -d ',')
			rs=$(echo "$instr" | awk '{print $3}' | tr -d ',')
			imm=$(echo "$instr" | awk '{print $4}')
			val=$(get_reg "$rs")
			result=$((val + imm))
			set_reg "$rd" "$result"
			update_flags $result
			;;

		subi)
			# subi rd, rs, imm
			rd=$(echo "$instr" | awk '{print $2}' | tr -d ',')
			rs=$(echo "$instr" | awk '{print $3}' | tr -d ',')
			imm=$(echo "$instr" | awk '{print $4}')
			val=$(get_reg "$rs")
			result=$((val - imm))
			set_reg "$rd" "$result"
			update_flags $result
			;;

		cmp)
			# cmp rs1, rs2
			rs1=$(echo "$instr" | awk '{print $2}' | tr -d ',')
			rs2=$(echo "$instr" | awk '{print $3}')
			val1=$(get_reg "$rs1")
			val2=$(get_reg "$rs2")
			result=$((val1 - val2))
			update_flags $result
			;;

		jmp)
			# jmp addr
			addr=$(echo "$instr" | awk '{print $2}')
			pc=$addr
			return
			;;

		jz)
			# jz addr
			addr=$(echo "$instr" | awk '{print $2}')
			if [ "$flag_z" -eq 1 ]; then
				pc=$addr
				return
			fi
			;;

		jnz)
			# jnz addr
			addr=$(echo "$instr" | awk '{print $2}')
			if [ "$flag_z" -eq 0 ]; then
				pc=$addr
				return
			fi
			;;

		jl)
			# jl addr (jump if less - negative flag set)
			addr=$(echo "$instr" | awk '{print $2}')
			if [ "$flag_n" -eq 1 ]; then
				pc=$addr
				return
			fi
			;;

		jg)
			# jg addr (jump if greater - not zero and not negative)
			addr=$(echo "$instr" | awk '{print $2}')
			if [ "$flag_z" -eq 0 ] && [ "$flag_n" -eq 0 ]; then
				pc=$addr
				return
			fi
			;;

		call)
			# call addr
			addr=$(echo "$instr" | awk '{print $2}')
			next_pc=$((pc + 1))
			stack_push $next_pc
			pc=$addr
			return
			;;

		ret)
			# ret
			pc=$(stack_pop)
			return
			;;

		push)
			# push rs
			rs=$(echo "$instr" | awk '{print $2}')
			val=$(get_reg "$rs")
			stack_push $val
			;;

		pop)
			# pop rd
			rd=$(echo "$instr" | awk '{print $2}')
			val=$(stack_pop)
			set_reg "$rd" "$val"
			;;

		print)
			# print rs
			rs=$(echo "$instr" | awk '{print $2}')
			val=$(get_reg "$rs")
			echo "$val"
			;;

		printc)
			# printc rs (print as character)
			rs=$(echo "$instr" | awk '{print $2}')
			val=$(get_reg "$rs")
			printf "\\$(printf '%03o' $val)"
			;;

		*)
			echo "Error: Unknown opcode '$opcode'" >&2
			exit 1
			;;
	esac

	# Increment PC
	pc=$((pc + 1))
}

# Main execution loop
run_vm() {
	while [ "$running" -eq 1 ] && [ "$pc" -lt "$prog_count" ]; do
		instr=$(get_prog_line $pc)

		# Skip empty lines and labels
		case "$instr" in
			''|*:)
				pc=$((pc + 1))
				continue
				;;
		esac

		execute "$instr"
	done
}

# Main
if [ $# -lt 1 ]; then
	echo "Usage: $0 program.vms" >&2
	exit 1
fi

program_file=$1

if [ ! -f "$program_file" ]; then
	echo "Error: File not found: $program_file" >&2
	exit 1
fi

load_program "$program_file"
run_vm

exit $r0
