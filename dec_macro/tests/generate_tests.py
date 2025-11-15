#!/usr/bin/env python3
"""
Generate individual test files for each DEC MACRO instruction
Creates one test file per instruction for PDP-10, PDP-11, and VAX
"""

import os

# PDP-10 instruction tests
pdp10_tests = [
    # Move operations
    ("movei", "MOVEI", "1,12345", "Move Immediate", "AC1 = 12345"),
    ("movem", "MOVEM", "1,DATA", "Move to Memory", "DATA = AC1", "MOVEI 1,999"),
    ("moves", "MOVES", "1,DATA", "Move to Self", "AC1 = DATA, DATA = AC1", "MOVEI 1,777"),
    ("movn", "MOVN", "2,1", "Move Negative", "AC2 = -AC1", "MOVEI 1,100"),
    ("movm", "MOVM", "2,1", "Move Magnitude", "AC2 = |AC1|", "MOVEI 1,-50"),
    ("movs", "MOVS", "2,1", "Move Swapped", "AC2 = swapped halves of AC1", "MOVEI 1,0123456"),

    # Half-word operations (18-bit)
    ("hll", "HLL", "1,DATA", "Half Left to Left", "AC1 left = DATA left", "MOVEI 1,0777777\nMOVEI 3,0123456\nMOVEM 3,DATA"),
    ("hrl", "HRL", "1,DATA", "Half Right to Left", "AC1 left = DATA right", "MOVEI 1,0\nMOVEI 3,0123456\nMOVEM 3,DATA"),
    ("hrr", "HRR", "1,DATA", "Half Right to Right", "AC1 right = DATA right", "MOVEI 1,0777777000000\nMOVEI 3,0765432\nMOVEM 3,DATA"),
    ("hlr", "HLR", "1,DATA", "Half Left to Right", "AC1 right = DATA left", "MOVEI 1,0\nMOVEI 3,0123456\nMOVEM 3,DATA"),
    ("hlli", "HLLI", "1,0123", "Half Left to Left Immediate", "AC1 left = 0123"),
    ("hrri", "HRRI", "1,0456", "Half Right to Right Immediate", "AC1 right = 0456"),

    # Arithmetic operations
    ("add", "ADD", "1,2", "Add", "AC1 = AC1 + AC2 = 150", "MOVEI 1,100\nMOVEI 2,50"),
    ("addi", "ADDI", "1,75", "Add Immediate", "AC1 = AC1 + 75 = 100", "MOVEI 1,25"),
    ("sub", "SUB", "1,2", "Subtract", "AC1 = AC1 - AC2 = 50", "MOVEI 1,200\nMOVEI 2,150"),
    ("subi", "SUBI", "1,100", "Subtract Immediate", "AC1 = AC1 - 100 = 200", "MOVEI 1,300"),
    ("mul", "MUL", "1,2", "Multiply", "AC1 = AC1 * AC2 = 120", "MOVEI 1,12\nMOVEI 2,10"),
    ("muli", "MULI", "1,8", "Multiply Immediate", "AC1 = AC1 * 8 = 120", "MOVEI 1,15"),
    ("div", "DIV", "1,2", "Divide", "AC1 = AC1 / AC2 = 30", "MOVEI 1,360\nMOVEI 2,12"),
    ("divi", "DIVI", "1,16", "Divide Immediate", "AC1 = AC1 / 16 = 30", "MOVEI 1,480"),

    # Shift and rotate
    ("lsh", "LSH", "1,8", "Logical Shift", "AC1 = AC1 << 8 = 256", "MOVEI 1,1"),
    ("ash", "ASH", "1,4", "Arithmetic Shift", "AC1 = AC1 << 4 = 256", "MOVEI 1,16"),

    # Logical operations
    ("and", "AND", "1,2", "Logical AND", "AC1 = AC1 & AC2", "MOVEI 1,0777777\nMOVEI 2,0707070"),
    ("andi", "ANDI", "1,0777000", "AND Immediate", "AC1 = AC1 & 0777000", "MOVEI 1,0123456"),
    ("ior", "IOR", "1,2", "Inclusive OR", "AC1 = AC1 | AC2", "MOVEI 1,0707000\nMOVEI 2,0070707"),
    ("iori", "IORI", "1,0007070", "IOR Immediate", "AC1 = AC1 | 0007070", "MOVEI 1,0100200"),
    ("xor", "XOR", "1,2", "Exclusive OR", "AC1 = AC1 ^ AC2", "MOVEI 1,0123456\nMOVEI 2,0707070"),
    ("xori", "XORI", "1,0777777", "XOR Immediate", "AC1 = AC1 ^ 0777777 = 0", "MOVEI 1,0777777"),

    # Set operations
    ("seto", "SETO", "1,0", "Set to Ones", "AC1 = -1 (all 1s)"),
    ("setz", "SETZ", "1,0", "Set to Zero", "AC1 = 0"),

    # Stack operations
    ("push", "PUSH", "17,1", "Push", "Push AC1 onto stack", "MOVEI 1,0777"),
    ("pop", "POP", "17,2", "Pop", "Pop into AC2"),
]

# PDP-11 instruction tests
pdp11_tests = [
    # Single operand
    ("clr", "CLR", "R0", "Clear", "R0 = 0, Z=1"),
    ("com", "COM", "R0", "Complement", "R0 = ~R0", "MOV #12345,R0"),
    ("inc", "INC", "R0", "Increment", "R0 = R0 + 1 = 101", "MOV #100,R0"),
    ("dec", "DEC", "R0", "Decrement", "R0 = R0 - 1 = 99", "MOV #100,R0"),
    ("neg", "NEG", "R0", "Negate", "R0 = -R0 = -100", "MOV #100,R0"),
    ("tst", "TST", "R0", "Test", "Flags set, N=1, Z=0", "MOV #-5,R0"),
    ("asr", "ASR", "R0", "Arithmetic Shift Right", "R0 = R0 >> 1 = -128", "MOV #-256,R0"),
    ("asl", "ASL", "R0", "Arithmetic Shift Left", "R0 = R0 << 1 = 200", "MOV #100,R0"),
    ("ror", "ROR", "R0", "Rotate Right", "R0 rotated right", "MOV #100,R0"),
    ("rol", "ROL", "R0", "Rotate Left", "R0 rotated left", "MOV #100,R0"),
    ("swab", "SWAB", "R0", "Swap Bytes", "Bytes swapped", "MOV #01234,R0"),

    # Single operand byte
    ("clrb", "CLRB", "R0", "Clear Byte", "R0 byte = 0, Z=1"),
    ("comb", "COMB", "R0", "Complement Byte", "R0 byte = ~byte", "MOV #255,R0"),
    ("incb", "INCB", "R0", "Increment Byte", "R0 byte = 51", "MOV #50,R0"),
    ("decb", "DECB", "R0", "Decrement Byte", "R0 byte = 49", "MOV #50,R0"),
    ("negb", "NEGB", "R0", "Negate Byte", "R0 byte = -byte", "MOV #50,R0"),
    ("tstb", "TSTB", "R0", "Test Byte", "Flags set", "MOV #200,R0"),

    # Double operand
    ("mov", "MOV", "#100,R0", "Move", "R0 = 100"),
    ("cmp", "CMP", "R0,#100", "Compare", "Flags: Z=1", "MOV #100,R0"),
    ("add", "ADD", "#50,R0", "Add", "R0 = R0 + 50 = 150", "MOV #100,R0"),
    ("sub", "SUB", "#25,R0", "Subtract", "R0 = R0 - 25 = 75", "MOV #100,R0"),
    ("bit", "BIT", "#0100000,R0", "Bit Test", "Test bit 15", "MOV #0177777,R0"),
    ("bic", "BIC", "#0100000,R0", "Bit Clear", "Clear bit 15", "MOV #0177777,R0"),
    ("bis", "BIS", "#0100000,R0", "Bit Set", "Set bit 15", "MOV #0,R0"),
    ("xor", "XOR", "R1,R0", "XOR", "R0 = R0 ^ R1", "MOV #0177777,R0\nMOV #0100000,R1"),

    # Branch
    ("br", "BR", "LBL\nLBL:", "Branch", "Branch to LBL"),
    ("bne", "BNE", "LBL\nLBL:", "Branch Not Equal", "Branch if Z=0", "CMP #1,#2"),
    ("beq", "BEQ", "LBL\nLBL:", "Branch Equal", "Branch if Z=1", "CMP #5,#5"),
    ("bpl", "BPL", "LBL\nLBL:", "Branch Plus", "Branch if N=0", "TST #10"),
    ("bmi", "BMI", "LBL\nLBL:", "Branch Minus", "Branch if N=1", "TST #-10"),
    ("bcc", "BCC", "LBL\nLBL:", "Branch Carry Clear", "Branch if C=0", "CLC"),
    ("bcs", "BCS", "LBL\nLBL:", "Branch Carry Set", "Branch if C=1", "SEC"),

    # Condition codes
    ("clc", "CLC", "", "Clear Carry", "C = 0"),
    ("clv", "CLV", "", "Clear Overflow", "V = 0"),
    ("clz", "CLZ", "", "Clear Zero", "Z = 0"),
    ("cln", "CLN", "", "Clear Negative", "N = 0"),
    ("sec", "SEC", "", "Set Carry", "C = 1"),
    ("sev", "SEV", "", "Set Overflow", "V = 1"),
    ("sez", "SEZ", "", "Set Zero", "Z = 1"),
    ("sen", "SEN", "", "Set Negative", "N = 1"),

    # Misc
    ("nop", "NOP", "", "No Operation", "No effect"),
    ("halt", "HALT", "", "Halt", "Halt processor"),
]

# VAX instruction tests
vax_tests = [
    # Byte operations
    ("movb", "MOVB", "#100,R0", "Move Byte", "R0 = 100 (8-bit)"),
    ("addb", "ADDB", "#25,R0", "Add Byte", "R0 = R0 + 25 = 75", "MOVB #50,R0"),
    ("subb", "SUBB", "#25,R0", "Subtract Byte", "R0 = R0 - 25 = 25", "MOVB #50,R0"),
    ("mulb", "MULB", "#2,R0", "Multiply Byte", "R0 = R0 * 2 = 100", "MOVB #50,R0"),
    ("divb", "DIVB", "#5,R0", "Divide Byte", "R0 = R0 / 5 = 20", "MOVB #100,R0"),
    ("cmpb", "CMPB", "#50,#50", "Compare Byte", "Z=1 (equal)"),
    ("tstb", "TSTB", "#-10", "Test Byte", "N=1, Z=0"),
    ("incb", "INCB", "R0", "Increment Byte", "R0 = 100", "MOVB #99,R0"),
    ("decb", "DECB", "R0", "Decrement Byte", "R0 = 99", "MOVB #100,R0"),
    ("clrb", "CLRB", "R0", "Clear Byte", "R0 = 0, Z=1"),

    # Word operations
    ("movw", "MOVW", "#12345,R0", "Move Word", "R0 = 12345 (16-bit)"),
    ("addw", "ADDW", "#500,R0", "Add Word", "R0 = 1500", "MOVW #1000,R0"),
    ("subw", "SUBW", "#500,R0", "Subtract Word", "R0 = 500", "MOVW #1000,R0"),
    ("mulw", "MULW", "#10,R0", "Multiply Word", "R0 = 1000", "MOVW #100,R0"),
    ("divw", "DIVW", "#2,R0", "Divide Word", "R0 = 500", "MOVW #1000,R0"),
    ("cmpw", "CMPW", "#12345,#12345", "Compare Word", "Z=1"),
    ("tstw", "TSTW", "#-1000", "Test Word", "N=1"),
    ("incw", "INCW", "R0", "Increment Word", "R0 = 1001", "MOVW #1000,R0"),
    ("decw", "DECW", "R0", "Decrement Word", "R0 = 999", "MOVW #1000,R0"),
    ("clrw", "CLRW", "R0", "Clear Word", "R0 = 0, Z=1"),

    # Long operations
    ("movl", "MOVL", "#1234567,R0", "Move Long", "R0 = 1234567 (32-bit)"),
    ("addl", "ADDL", "#500000,R0", "Add Long", "R0 = 1500000", "MOVL #1000000,R0"),
    ("subl", "SUBL", "#500000,R0", "Subtract Long", "R0 = 500000", "MOVL #1000000,R0"),
    ("mull", "MULL", "#5,R0", "Multiply Long", "R0 = 500000", "MOVL #100000,R0"),
    ("divl", "DIVL", "#10,R0", "Divide Long", "R0 = 100000", "MOVL #1000000,R0"),
    ("cmpl", "CMPL", "#1234567,#1234567", "Compare Long", "Z=1"),
    ("tstl", "TSTL", "#-1000000", "Test Long", "N=1"),
    ("incl", "INCL", "R0", "Increment Long", "R0 = 1000000", "MOVL #999999,R0"),
    ("decl", "DECL", "R0", "Decrement Long", "R0 = 999999", "MOVL #1000000,R0"),
    ("clrl", "CLRL", "R0", "Clear Long", "R0 = 0, Z=1"),

    # Logical long
    ("bicl", "BICL", "#0xF0000000,R0", "Bit Clear Long", "High 4 bits cleared", "MOVL #0xFFFFFFFF,R0"),
    ("bisl", "BISL", "#0xF0000000,R0", "Bit Set Long", "High 4 bits set", "MOVL #0,R0"),
    ("xorl", "XORL", "#0xFFFFFFFF,R0", "XOR Long", "R0 = ~R0", "MOVL #0xAAAAAAAA,R0"),

    # Branch
    ("brb", "BRB", "LBL\nLBL:", "Branch", "Branch to LBL"),
    ("brw", "BRW", "LBL\nLBL:", "Branch Word", "Branch to LBL"),
    ("beql", "BEQL", "LBL\nLBL:", "Branch Equal", "Branch if Z=1", "CMPL #5,#5"),
    ("bneq", "BNEQ", "LBL\nLBL:", "Branch Not Equal", "Branch if Z=0", "CMPL #5,#6"),
    ("blss", "BLSS", "LBL\nLBL:", "Branch Less", "Branch if N=1", "CMPL #-5,#0"),
    ("bgtr", "BGTR", "LBL\nLBL:", "Branch Greater", "Branch if N=0,Z=0", "CMPL #10,#5"),

    # Misc
    ("nop", "NOP", "", "No Operation", "No effect"),
    ("halt", "HALT", "", "Halt", "Halt processor"),
]

def generate_test_file(arch, name, inst, args, desc, expected, setup=""):
    """Generate a single test file"""
    filename = f"{arch}/test_{name}.mac"

    content = f"""; Test {inst} - {desc} ({arch.upper()})
; Expected: {expected}

\t.PSECT\tCODE

START:
"""

    if setup:
        for line in setup.split('\n'):
            content += f"\t{line}\n"

    if args:
        content += f"\t{inst}\t{args}\n"
    else:
        content += f"\t{inst}\n"

    content += f"""\tHALT

DATA:\t.WORD\t0

\t.END
"""

    return filename, content

def main():
    os.makedirs("pdp10", exist_ok=True)
    os.makedirs("pdp11", exist_ok=True)
    os.makedirs("vax", exist_ok=True)

    count = 0

    # Generate PDP-10 tests
    for test_data in pdp10_tests:
        if len(test_data) == 5:
            name, inst, args, desc, expected = test_data
            setup = ""
        else:
            name, inst, args, desc, expected, setup = test_data

        filename, content = generate_test_file("pdp10", name, inst, args, desc, expected, setup)
        with open(filename, 'w') as f:
            f.write(content)
        count += 1

    # Generate PDP-11 tests
    for test_data in pdp11_tests:
        if len(test_data) == 5:
            name, inst, args, desc, expected = test_data
            setup = ""
        else:
            name, inst, args, desc, expected, setup = test_data

        filename, content = generate_test_file("pdp11", name, inst, args, desc, expected, setup)
        with open(filename, 'w') as f:
            f.write(content)
        count += 1

    # Generate VAX tests
    for test_data in vax_tests:
        if len(test_data) == 5:
            name, inst, args, desc, expected = test_data
            setup = ""
        else:
            name, inst, args, desc, expected, setup = test_data

        filename, content = generate_test_file("vax", name, inst, args, desc, expected, setup)
        with open(filename, 'w') as f:
            f.write(content)
        count += 1

    print(f"Generated {count} individual test files")
    print(f"  PDP-10: {len(pdp10_tests)} tests")
    print(f"  PDP-11: {len(pdp11_tests)} tests")
    print(f"  VAX: {len(vax_tests)} tests")

if __name__ == "__main__":
    main()
