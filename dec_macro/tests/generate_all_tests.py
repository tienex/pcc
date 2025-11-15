#!/usr/bin/env python3
"""
Generate comprehensive test files for ALL DEC MACRO instructions.
Parses codegen.c instruction table to ensure complete coverage.
"""

import os
import re
import sys

def parse_instruction_table(codegen_path):
    """Parse the instruction table from codegen.c."""
    instructions = []
    current_arch = None
    current_category = None

    with open(codegen_path, 'r') as f:
        content = f.read()

    # Find the instruction table
    table_start = content.find('static const insn_table_entry_t insn_table[] = {')
    if table_start == -1:
        print("ERROR: Could not find instruction table")
        return []

    # Extract table content
    table_end = content.find('};', table_start)
    table_content = content[table_start:table_end]

    # Parse each instruction entry
    lines = table_content.split('\n')
    for line in lines:
        # Detect architecture from comments
        if '/* PDP-10' in line or '/* PDP-6' in line:
            current_arch = 'pdp10'
        elif '/* PDP-11' in line:
            current_arch = 'pdp11'
        elif '/* VAX' in line:
            current_arch = 'vax'

        # Extract category
        if '/*' in line and '*/' in line:
            comment = line[line.find('/*')+2:line.find('*/')].strip()
            if not comment.startswith('0x'):  # Not an opcode comment
                current_category = comment

        # Parse instruction entry: { "NAME", handler, opcode }
        match = re.search(r'\{\s*"([A-Z0-9]+)"', line)
        if match:
            name = match.group(1)

            # Infer architecture if not set
            if current_arch is None:
                if name.endswith(('B2', 'B3', 'W2', 'W3', 'L2', 'L3', 'Q', 'O')):
                    current_arch = 'vax'
                elif name.endswith(('I', 'M', 'S', 'B')) or name in ['MOVEI', 'MOVEM', 'MOVES']:
                    current_arch = 'pdp10'
                else:
                    current_arch = 'pdp11'  # Default

            instructions.append({
                'name': name,
                'arch': current_arch,
                'category': current_category or 'General'
            })

    return instructions

def get_instruction_test_data(inst_name, arch):
    """Get test data for a specific instruction."""

    # PDP-10 instruction patterns
    pdp10_patterns = {
        # Move operations
        'MOVE': ('2,DATA', 'Move', 'AC2 = value from DATA', 'MOVEI 1,12345\nMOVEM 1,DATA'),
        'MOVEI': ('1,12345', 'Move Immediate', 'AC1 = 12345', ''),
        'MOVEM': ('1,DATA', 'Move to Memory', 'DATA = AC1', 'MOVEI 1,777'),
        'MOVES': ('1,DATA', 'Move to Self', 'AC1 = AC1, DATA = AC1', 'MOVEI 1,555'),
        'MOVM': ('1,2', 'Move Magnitude', 'AC1 = |AC2|', 'MOVEI 2,-100'),
        'MOVN': ('1,2', 'Move Negated', 'AC1 = -AC2', 'MOVEI 2,123'),
        'MOVS': ('1,2', 'Move Swapped', 'AC1 = swapped halves of AC2', 'MOVEI 2,0123456'),

        # Arithmetic
        'ADD': ('1,2', 'Add', 'AC1 = AC1 + AC2, CC updated', 'MOVEI 1,100\nMOVEI 2,50'),
        'ADDI': ('1,100', 'Add Immediate', 'AC1 = AC1 + 100, CC updated', 'MOVEI 1,50'),
        'ADDM': ('1,DATA', 'Add to Memory', 'DATA = DATA + AC1, CC updated', 'MOVEI 1,25\nMOVEI 2,75\nMOVEM 2,DATA'),
        'SUB': ('1,2', 'Subtract', 'AC1 = AC1 - AC2, CC updated', 'MOVEI 1,100\nMOVEI 2,30'),
        'SUBI': ('1,50', 'Subtract Immediate', 'AC1 = AC1 - 50, CC updated', 'MOVEI 1,100'),
        'SUBM': ('1,DATA', 'Subtract to Memory', 'DATA = DATA - AC1, CC updated', 'MOVEI 1,20\nMOVEI 2,100\nMOVEM 2,DATA'),
        'MUL': ('1,2', 'Multiply', 'AC1 = AC1 * AC2, CC updated', 'MOVEI 1,10\nMOVEI 2,5'),
        'MULI': ('1,10', 'Multiply Immediate', 'AC1 = AC1 * 10, CC updated', 'MOVEI 1,5'),
        'MULM': ('1,DATA', 'Multiply to Memory', 'DATA = DATA * AC1, CC updated', 'MOVEI 1,3\nMOVEI 2,10\nMOVEM 2,DATA'),
        'DIV': ('1,2', 'Divide', 'AC1 = AC1 / AC2, AC2 = remainder, CC updated', 'MOVEI 1,100\nMOVEI 2,7'),
        'DIVI': ('1,8', 'Divide Immediate', 'AC1 = AC1 / 8, AC2 = remainder, CC updated', 'MOVEI 1,64'),
        'DIVM': ('1,DATA', 'Divide to Memory', 'DATA = DATA / AC1, CC updated', 'MOVEI 1,5\nMOVEI 2,50\nMOVEM 2,DATA'),

        # Half-word operations
        'HLL': ('1,2', 'Half Left to Left', 'AC1[0:17] = AC2[0:17], AC1[18:35] unchanged', 'MOVEI 1,0123456\nMOVEI 2,0777000'),
        'HLLI': ('1,0777000', 'Half Left to Left Immediate', 'AC1[0:17] = immediate, AC1[18:35] unchanged', 'MOVEI 1,0123456'),
        'HLLM': ('1,DATA', 'Half Left to Left to Memory', 'DATA[0:17] = AC1[0:17], DATA[18:35] unchanged', 'MOVEI 1,0777000'),
        'HLLS': ('1,DATA', 'Half Left to Left to Self', 'AC1[0:17] = DATA[0:17], AC1[18:35] unchanged', 'MOVEI 1,0123456\nMOVEI 2,0777000\nMOVEM 2,DATA'),
        'HRL': ('1,2', 'Half Right to Left', 'AC1[0:17] = AC2[18:35], AC1[18:35] unchanged', 'MOVEI 1,0123456\nMOVEI 2,0777'),
        'HRLI': ('1,0777', 'Half Right to Left Immediate', 'AC1[0:17] = immediate, AC1[18:35] unchanged', 'MOVEI 1,0123456'),
        'HRLM': ('1,DATA', 'Half Right to Left to Memory', 'DATA[0:17] = AC1[18:35], DATA[18:35] unchanged', 'MOVEI 1,0777'),
        'HRLS': ('1,DATA', 'Half Right to Left to Self', 'AC1[0:17] = DATA[18:35], AC1[18:35] unchanged', 'MOVEI 1,0123456\nMOVEI 2,0777\nMOVEM 2,DATA'),
        'HRR': ('1,2', 'Half Right to Right', 'AC1[18:35] = AC2[18:35], AC1[0:17] unchanged', 'MOVEI 1,0777000\nMOVEI 2,0123'),
        'HRRI': ('1,0123', 'Half Right to Right Immediate', 'AC1[18:35] = immediate, AC1[0:17] unchanged', 'MOVEI 1,0777000'),
        'HRRM': ('1,DATA', 'Half Right to Right to Memory', 'DATA[18:35] = AC1[18:35], DATA[0:17] unchanged', 'MOVEI 1,0123'),
        'HRRS': ('1,DATA', 'Half Right to Right to Self', 'AC1[18:35] = DATA[18:35], AC1[0:17] unchanged', 'MOVEI 1,0777000\nMOVEI 2,0123\nMOVEM 2,DATA'),
        'HLR': ('1,2', 'Half Left to Right', 'AC1[18:35] = AC2[0:17], AC1[0:17] unchanged', 'MOVEI 1,0123\nMOVEI 2,0777000'),
        'HLRI': ('1,0777000', 'Half Left to Right Immediate', 'AC1[18:35] = immediate, AC1[0:17] unchanged', 'MOVEI 1,0123'),
        'HLRM': ('1,DATA', 'Half Left to Right to Memory', 'DATA[18:35] = AC1[0:17], DATA[0:17] unchanged', 'MOVEI 1,0777000'),
        'HLRS': ('1,DATA', 'Half Left to Right to Self', 'AC1[18:35] = DATA[0:17], AC1[0:17] unchanged', 'MOVEI 1,0123\nMOVEI 2,0777000\nMOVEM 2,DATA'),

        # Logical operations
        'AND': ('1,2', 'Logical AND', 'AC1 = AC1 & AC2, CC updated', 'MOVEI 1,0777777\nMOVEI 2,0707070'),
        'ANDI': ('1,0777000', 'AND Immediate', 'AC1 = AC1 & immediate, CC updated', 'MOVEI 1,0123456'),
        'ANDM': ('1,DATA', 'AND to Memory', 'DATA = DATA & AC1, CC updated', 'MOVEI 1,0707070\nMOVEI 2,0777777\nMOVEM 2,DATA'),
        'IOR': ('1,2', 'Inclusive OR', 'AC1 = AC1 | AC2, CC updated', 'MOVEI 1,0707070\nMOVEI 2,0070707'),
        'IORI': ('1,0007070', 'IOR Immediate', 'AC1 = AC1 | immediate, CC updated', 'MOVEI 1,0100200'),
        'IORM': ('1,DATA', 'IOR to Memory', 'DATA = DATA | AC1, CC updated', 'MOVEI 1,0070707\nMOVEI 2,0707070\nMOVEM 2,DATA'),
        'XOR': ('1,2', 'Exclusive OR', 'AC1 = AC1 ^ AC2, CC updated', 'MOVEI 1,0123456\nMOVEI 2,0777777'),
        'XORI': ('1,0777777', 'XOR Immediate', 'AC1 = AC1 ^ immediate, CC updated', 'MOVEI 1,0777777'),
        'XORM': ('1,DATA', 'XOR to Memory', 'DATA = DATA ^ AC1, CC updated', 'MOVEI 1,0777777\nMOVEI 2,0123456\nMOVEM 2,DATA'),
        'EQV': ('1,2', 'Equivalence (XNOR)', 'AC1 = ~(AC1 ^ AC2), CC updated', 'MOVEI 1,0707070\nMOVEI 2,0707070'),
        'EQVI': ('1,0707070', 'Equivalence Immediate', 'AC1 = ~(AC1 ^ immediate), CC updated', 'MOVEI 1,0707070'),
        'EQVM': ('1,DATA', 'Equivalence to Memory', 'DATA = ~(DATA ^ AC1), CC updated', 'MOVEI 1,0707070\nMOVEI 2,0707070\nMOVEM 2,DATA'),

        # AND with complement operations
        'ANDCA': ('1,2', 'AND Complement AC', 'AC1 = ~AC1 & AC2, CC updated', 'MOVEI 1,0777000\nMOVEI 2,0777777'),
        'ANDCAI': ('1,0777777', 'AND Complement AC Immediate', 'AC1 = ~AC1 & immediate, CC updated', 'MOVEI 1,0777000'),
        'ANDCAM': ('1,DATA', 'AND Complement AC to Memory', 'DATA = ~AC1 & DATA, CC updated', 'MOVEI 1,0777000\nMOVEI 2,0777777\nMOVEM 2,DATA'),
        'ANDCM': ('1,2', 'AND Complement Memory', 'AC1 = AC1 & ~AC2, CC updated', 'MOVEI 1,0777777\nMOVEI 2,0707070'),
        'ANDCMI': ('1,0707070', 'AND Complement Memory Immediate', 'AC1 = AC1 & ~immediate, CC updated', 'MOVEI 1,0777777'),
        'ANDCMM': ('1,DATA', 'AND Complement Memory to Memory', 'DATA = DATA & ~AC1, CC updated', 'MOVEI 1,0707070\nMOVEI 2,0777777\nMOVEM 2,DATA'),

        # Set operations
        'SETO': ('1,0', 'Set to Ones', 'AC1 = -1 (all 1s)', ''),
        'SETOI': ('1,0', 'Set to Ones Immediate', 'AC1 = -1 (all 1s)', ''),
        'SETOM': ('DATA', 'Set to Ones to Memory', 'DATA = -1 (all 1s)', ''),
        'SETZ': ('1,0', 'Set to Zero', 'AC1 = 0, Z=1', ''),
        'SETZI': ('1,0', 'Set to Zero Immediate', 'AC1 = 0, Z=1', ''),
        'SETZM': ('DATA', 'Set to Zero to Memory', 'DATA = 0, Z=1', ''),
        'SETCA': ('1,1', 'Set Complement AC', 'AC1 = ~AC1, CC updated', 'MOVEI 1,0123456'),
        'SETCAI': ('1,1', 'Set Complement AC Immediate', 'AC1 = ~AC1, CC updated', 'MOVEI 1,0123456'),
        'SETCAM': ('1,DATA', 'Set Complement AC to Memory', 'DATA = ~AC1, CC updated', 'MOVEI 1,0123456'),
        'SETCM': ('1,DATA', 'Set Complement Memory', 'AC1 = ~DATA, CC updated', 'MOVEI 2,0123456\nMOVEM 2,DATA'),
        'SETCMI': ('1,DATA', 'Set Complement Memory Immediate', 'AC1 = ~DATA, CC updated', 'MOVEI 2,0123456\nMOVEM 2,DATA'),
        'SETCMM': ('1,DATA', 'Set Complement Memory to Memory', 'DATA = ~DATA, CC updated', 'MOVEI 2,0123456\nMOVEM 2,DATA'),

        # Shift/Rotate
        'ASH': ('1,2', 'Arithmetic Shift', 'AC1 = AC1 << AC2 (or >> if negative), CC updated', 'MOVEI 1,1\nMOVEI 2,3'),
        'ASHC': ('1,2', 'Arithmetic Shift Combined', 'AC1:AC2 = AC1:AC2 << operand, CC updated', 'MOVEI 1,1\nMOVEI 2,0\nMOVEI 3,4'),
        'LSHC': ('1,2', 'Logical Shift Combined', 'AC1:AC2 = AC1:AC2 << operand (logical), CC updated', 'MOVEI 1,1\nMOVEI 2,0\nMOVEI 3,4'),
        'ROTC': ('1,2', 'Rotate Combined', 'AC1:AC2 rotated, CC updated', 'MOVEI 1,0123456\nMOVEI 2,0654321\nMOVEI 3,4'),
        'ROT': ('1,2', 'Rotate', 'AC1 rotated, CC updated', 'MOVEI 1,0123456\nMOVEI 2,4'),
        'LSH': ('1,2', 'Logical Shift', 'AC1 = AC1 << AC2 (logical), CC updated', 'MOVEI 1,1\nMOVEI 2,3'),

        # Skip operations
        'SKIP': ('', 'Skip', 'Skip next instruction', ''),
        'SKIPA': ('', 'Skip Always', 'Skip next instruction always', ''),
        'SKIPE': ('1', 'Skip if Equal', 'Skip if AC1 = 0', 'MOVEI 1,0'),
        'SKIPN': ('1', 'Skip if Not Equal', 'Skip if AC1 != 0', 'MOVEI 1,5'),
        'SKIPL': ('1', 'Skip if Less', 'Skip if AC1 < 0', 'MOVEI 1,-10'),
        'SKIPG': ('1', 'Skip if Greater', 'Skip if AC1 > 0', 'MOVEI 1,25'),
        'SKIPLE': ('1', 'Skip if Less or Equal', 'Skip if AC1 <= 0', 'MOVEI 1,0'),
        'SKIPGE': ('1', 'Skip if Greater or Equal', 'Skip if AC1 >= 0', 'MOVEI 1,1'),

        # Jump operations
        'JUMP': ('LABEL', 'Jump', 'Jump to LABEL', ''),
        'JUMPA': ('LABEL', 'Jump Always', 'Jump to LABEL always', ''),
        'JUMPE': ('1,LABEL', 'Jump if Equal', 'Jump if AC1 = 0', 'MOVEI 1,0'),
        'JUMPN': ('1,LABEL', 'Jump if Not Equal', 'Jump if AC1 != 0', 'MOVEI 1,1'),
        'JUMPL': ('1,LABEL', 'Jump if Less', 'Jump if AC1 < 0', 'MOVEI 1,-5'),
        'JUMPG': ('1,LABEL', 'Jump if Greater', 'Jump if AC1 > 0', 'MOVEI 1,10'),
        'JUMPLE': ('1,LABEL', 'Jump if Less or Equal', 'Jump if AC1 <= 0', 'MOVEI 1,0'),
        'JUMPGE': ('1,LABEL', 'Jump if Greater or Equal', 'Jump if AC1 >= 0', 'MOVEI 1,1'),

        # Add One and Jump
        'AOJ': ('1,LABEL', 'Add One and Jump', 'AC1++, jump to LABEL', 'MOVEI 1,5'),
        'AOJA': ('1,LABEL', 'Add One and Jump Always', 'AC1++, jump always', 'MOVEI 1,10'),
        'AOJE': ('1,LABEL', 'Add One and Jump if Equal', 'AC1++, jump if result = 0', 'MOVEI 1,-1'),
        'AOJN': ('1,LABEL', 'Add One and Jump if Not Equal', 'AC1++, jump if result != 0', 'MOVEI 1,1'),
        'AOJL': ('1,LABEL', 'Add One and Jump if Less', 'AC1++, jump if result < 0', 'MOVEI 1,-10'),
        'AOJG': ('1,LABEL', 'Add One and Jump if Greater', 'AC1++, jump if result > 0', 'MOVEI 1,1'),
        'AOJLE': ('1,LABEL', 'Add One and Jump if Less or Equal', 'AC1++, jump if result <= 0', 'MOVEI 1,-1'),
        'AOJGE': ('1,LABEL', 'Add One and Jump if Greater or Equal', 'AC1++, jump if result >= 0', 'MOVEI 1,-1'),

        # Add One to memory and Skip
        'AOS': ('DATA', 'Add One to memory and Skip', 'DATA++, skip if result != 0', 'MOVEI 1,5\nMOVEM 1,DATA'),
        'AOSA': ('DATA', 'Add One to memory and Skip Always', 'DATA++, skip always', 'MOVEI 1,10\nMOVEM 1,DATA'),
        'AOSE': ('DATA', 'Add One to memory and Skip if Equal', 'DATA++, skip if result = 0', 'MOVEI 1,-1\nMOVEM 1,DATA'),
        'AOSN': ('DATA', 'Add One to memory and Skip if Not Equal', 'DATA++, skip if result != 0', 'MOVEI 1,1\nMOVEM 1,DATA'),
        'AOSL': ('DATA', 'Add One to memory and Skip if Less', 'DATA++, skip if result < 0', 'MOVEI 1,-10\nMOVEM 1,DATA'),
        'AOSG': ('DATA', 'Add One to memory and Skip if Greater', 'DATA++, skip if result > 0', 'MOVEI 1,1\nMOVEM 1,DATA'),
        'AOSLE': ('DATA', 'Add One to memory and Skip if Less or Equal', 'DATA++, skip if result <= 0', 'MOVEI 1,-1\nMOVEM 1,DATA'),
        'AOSGE': ('DATA', 'Add One to memory and Skip if Greater or Equal', 'DATA++, skip if result >= 0', 'MOVEI 1,-1\nMOVEM 1,DATA'),

        # Subtract One and Jump
        'SOJ': ('2,LABEL', 'Subtract One and Jump', 'AC2--, jump to LABEL', 'MOVEI 2,10'),
        'SOJA': ('2,LABEL', 'Subtract One and Jump Always', 'AC2--, jump always', 'MOVEI 2,20'),
        'SOJE': ('2,LABEL', 'Subtract One and Jump if Equal', 'AC2--, jump if result = 0', 'MOVEI 2,1'),
        'SOJN': ('2,LABEL', 'Subtract One and Jump if Not Equal', 'AC2--, jump if result != 0', 'MOVEI 2,10'),
        'SOJL': ('2,LABEL', 'Subtract One and Jump if Less', 'AC2--, jump if result < 0', 'MOVEI 2,0'),
        'SOJG': ('2,LABEL', 'Subtract One and Jump if Greater', 'AC2--, jump if result > 0', 'MOVEI 2,5'),
        'SOJLE': ('2,LABEL', 'Subtract One and Jump if Less or Equal', 'AC2--, jump if result <= 0', 'MOVEI 2,1'),
        'SOJGE': ('2,LABEL', 'Subtract One and Jump if Greater or Equal', 'AC2--, jump if result >= 0', 'MOVEI 2,1'),

        # Subtract One from memory and Skip
        'SOS': ('DATA', 'Subtract One from memory and Skip', 'DATA--, skip if result != 0', 'MOVEI 1,3\nMOVEM 1,DATA'),
        'SOSA': ('DATA', 'Subtract One from memory and Skip Always', 'DATA--, skip always', 'MOVEI 1,10\nMOVEM 1,DATA'),
        'SOSE': ('DATA', 'Subtract One from memory and Skip if Equal', 'DATA--, skip if result = 0', 'MOVEI 1,1\nMOVEM 1,DATA'),
        'SOSN': ('DATA', 'Subtract One from memory and Skip if Not Equal', 'DATA--, skip if result != 0', 'MOVEI 1,5\nMOVEM 1,DATA'),
        'SOSL': ('DATA', 'Subtract One from memory and Skip if Less', 'DATA--, skip if result < 0', 'MOVEI 1,0\nMOVEM 1,DATA'),
        'SOSG': ('DATA', 'Subtract One from memory and Skip if Greater', 'DATA--, skip if result > 0', 'MOVEI 1,5\nMOVEM 1,DATA'),
        'SOSLE': ('DATA', 'Subtract One from memory and Skip if Less or Equal', 'DATA--, skip if result <= 0', 'MOVEI 1,1\nMOVEM 1,DATA'),
        'SOSGE': ('DATA', 'Subtract One from memory and Skip if Greater or Equal', 'DATA--, skip if result >= 0', 'MOVEI 1,1\nMOVEM 1,DATA'),

        # Compare operations
        'CAI': ('1,100', 'Compare AC Immediate', 'Compare AC1 with 100, set CC', 'MOVEI 1,100'),
        'CAIL': ('1,50', 'Compare AC Immediate, skip if Less', 'Compare AC1 with 50, skip if AC1 < 50', 'MOVEI 1,25'),
        'CAIE': ('1,100', 'Compare AC Immediate, skip if Equal', 'Compare AC1 with 100, skip if AC1 = 100', 'MOVEI 1,100'),
        'CAILE': ('1,50', 'Compare AC Immediate, skip if Less or Equal', 'Compare AC1 with 50, skip if AC1 <= 50', 'MOVEI 1,50'),
        'CAIA': ('1,100', 'Compare AC Immediate Always', 'Compare AC1 with 100, skip always', 'MOVEI 1,100'),
        'CAIGE': ('1,50', 'Compare AC Immediate, skip if Greater or Equal', 'Compare AC1 with 50, skip if AC1 >= 50', 'MOVEI 1,75'),
        'CAIN': ('1,100', 'Compare AC Immediate, skip if Not equal', 'Compare AC1 with 100, skip if AC1 != 100', 'MOVEI 1,50'),
        'CAIG': ('1,50', 'Compare AC Immediate, skip if Greater', 'Compare AC1 with 50, skip if AC1 > 50', 'MOVEI 1,100'),
        'CAM': ('1,DATA', 'Compare AC with Memory', 'Compare AC1 with DATA, set CC', 'MOVEI 1,100\nMOVEI 2,100\nMOVEM 2,DATA'),
        'CAML': ('1,DATA', 'Compare AC with Memory, skip if Less', 'Compare AC1 with DATA, skip if AC1 < DATA', 'MOVEI 1,25\nMOVEI 2,50\nMOVEM 2,DATA'),
        'CAME': ('1,DATA', 'Compare AC with Memory, skip if Equal', 'Compare AC1 with DATA, skip if AC1 = DATA', 'MOVEI 1,100\nMOVEI 2,100\nMOVEM 2,DATA'),
        'CAMLE': ('1,DATA', 'Compare AC with Memory, skip if Less or Equal', 'Compare AC1 with DATA, skip if AC1 <= DATA', 'MOVEI 1,50\nMOVEI 2,50\nMOVEM 2,DATA'),
        'CAMA': ('1,DATA', 'Compare AC with Memory Always', 'Compare AC1 with DATA, skip always', 'MOVEI 1,100\nMOVEI 2,100\nMOVEM 2,DATA'),
        'CAMGE': ('1,DATA', 'Compare AC with Memory, skip if Greater or Equal', 'Compare AC1 with DATA, skip if AC1 >= DATA', 'MOVEI 1,75\nMOVEI 2,50\nMOVEM 2,DATA'),
        'CAMN': ('1,DATA', 'Compare AC with Memory, skip if Not equal', 'Compare AC1 with DATA, skip if AC1 != DATA', 'MOVEI 1,100\nMOVEI 2,50\nMOVEM 2,DATA'),
        'CAMG': ('1,DATA', 'Compare AC with Memory, skip if Greater', 'Compare AC1 with DATA, skip if AC1 > DATA', 'MOVEI 1,100\nMOVEI 2,50\nMOVEM 2,DATA'),

        # Floating point
        'FAD': ('1,DATA', 'Floating Add', 'AC1 = AC1 + DATA (float), CC updated', 'MOVEI 1,0200000000000\nMOVEI 2,0200000000000\nMOVEM 2,DATA'),
        'FADL': ('1,DATA', 'Floating Add Long', 'AC1:AC2 = AC1:AC2 + DATA (long float), CC updated', 'MOVEI 1,0200000000000\nMOVEI 2,0\nMOVEI 3,0200000000000\nMOVEM 3,DATA'),
        'FADM': ('1,DATA', 'Floating Add to Memory', 'DATA = DATA + AC1 (float), CC updated', 'MOVEI 1,0200000000000\nMOVEI 2,0200000000000\nMOVEM 2,DATA'),
        'FADB': ('1,DATA', 'Floating Add Both', 'AC1 = AC1 + DATA, DATA = result (float), CC updated', 'MOVEI 1,0200000000000\nMOVEI 2,0200000000000\nMOVEM 2,DATA'),
        'FSB': ('1,DATA', 'Floating Subtract', 'AC1 = AC1 - DATA (float), CC updated', 'MOVEI 1,0400000000000\nMOVEI 2,0200000000000\nMOVEM 2,DATA'),
        'FMP': ('1,DATA', 'Floating Multiply', 'AC1 = AC1 * DATA (float), CC updated', 'MOVEI 1,0200000000000\nMOVEI 2,0201000000000\nMOVEM 2,DATA'),
        'FDV': ('1,DATA', 'Floating Divide', 'AC1 = AC1 / DATA (float), CC updated', 'MOVEI 1,0400000000000\nMOVEI 2,0200000000000\nMOVEM 2,DATA'),

        # Byte operations
        'LDB': ('2,STRING', 'Load Byte', 'AC2 = byte from STRING, CC updated', 'MOVEI 1,BYTEPTR'),
        'ILDB': ('2,STRING', 'Increment and Load Byte', 'Increment byte pointer, AC2 = byte, CC updated', 'MOVEI 1,BYTEPTR'),
        'DPB': ('2,DEST', 'Deposit Byte', 'Deposit AC2 into DEST at byte position', 'MOVEI 2,0777\nMOVEI 1,BYTEPTR'),
        'IDPB': ('2,DEST', 'Increment and Deposit Byte', 'Deposit AC2, increment byte pointer', 'MOVEI 2,0101\nMOVEI 1,BYTEPTR'),
        'IBP': ('1', 'Increment Byte Pointer', 'Increment byte pointer in AC1', 'MOVEI 1,BYTEPTR'),

        # Stack and I/O
        'PUSH': ('1,2', 'Push', 'Push AC2 onto stack, decrement AC1', 'MOVEI 1,STACK\nMOVEI 2,12345'),
        'POP': ('1,2', 'Pop', 'Pop from stack into AC2, increment AC1', 'MOVEI 1,STACK'),
        'PUSHJ': ('1,SUBR', 'Push Jump', 'Push PC, jump to SUBR', 'MOVEI 1,STACK'),
        'POPJ': ('1,0', 'Pop Jump', 'Pop return address, jump', 'MOVEI 1,STACK'),

        # I/O and special
        'HALT': ('', 'Halt', 'Halt processor', ''),
        'JRST': ('LABEL', 'Jump and Restore', 'Jump to LABEL', ''),
        'JSR': ('SUBR', 'Jump to Subroutine', 'Jump to subroutine at SUBR', ''),
        'JSP': ('1,SUBR', 'Jump and Save PC', 'Save PC in AC1, jump to SUBR', ''),
        'JSA': ('1,SUBR', 'Jump and Save AC', 'Save PC in memory, jump to SUBR', ''),
        'JRA': ('1,0', 'Jump and Restore AC', 'Restore AC and jump', ''),
    }

    # Check for pattern match (handle variants)
    base_inst = inst_name
    for suffix in ['I', 'M', 'S', 'B', 'L', 'R', 'RI', 'RM', 'RB']:
        if inst_name.endswith(suffix) and inst_name[:-len(suffix)] in pdp10_patterns:
            base_inst = inst_name[:-len(suffix)]
            break

    if inst_name in pdp10_patterns:
        return pdp10_patterns[inst_name]
    elif base_inst in pdp10_patterns:
        # Use base pattern but adjust name
        pattern = pdp10_patterns[base_inst]
        desc = pattern[1]
        if inst_name.endswith('I'):
            desc += ' Immediate'
        elif inst_name.endswith('M'):
            desc += ' to Memory'
        elif inst_name.endswith('S'):
            desc += ' to Self'
        elif inst_name.endswith('B'):
            desc += ' Both'
        return pattern[:1] + (desc,) + pattern[2:]
    else:
        # Generic pattern
        return ('', inst_name, 'Operation ' + inst_name, '')

def generate_pdp10_test(inst_name, args, desc, expected, setup):
    """Generate a PDP-10 test file."""
    content = f"""; Test {inst_name} - {desc} (PDP-10)
; Expected: {expected}

\t.PSECT\tCODE

START:
"""
    if setup:
        for line in setup.split('\n'):
            if line.strip():
                content += f"\t{line}\n"

    # Add instruction
    if args:
        content += f"\t{inst_name}\t{args}\n"
    else:
        content += f"\t{inst_name}\n"

    # Add label for jump instructions if needed
    if 'LABEL' in args or 'LABEL' in inst_name:
        content += "\tMOVEI\t1,999\n"
        content += "LABEL:\tMOVEI\t1,200\n"

    content += "\tHALT\n\n"
    content += "DATA:\t.WORD\t0\n\n"
    content += "\t.END\n"
    return content

def generate_pdp11_test(inst_name):
    """Generate a PDP-11 test file."""
    content = f"""; Test {inst_name} - {inst_name} operation (PDP-11)

\t.PSECT\tCODE

START:
\t{inst_name}\tR1,R2
\tHALT

DATA:\t.WORD\t0

\t.END
"""
    return content

def generate_vax_test(inst_name):
    """Generate a VAX test file."""
    # Determine operand count from suffix
    if inst_name.endswith('2'):
        operands = "R1,R2"
    elif inst_name.endswith('3'):
        operands = "R1,R2,R3"
    else:
        operands = "R1,R2"

    content = f"""; Test {inst_name} - {inst_name} operation (VAX)

\t.PSECT\tCODE

START:
\t{inst_name}\t{operands}
\tHALT

DATA:\t.WORD\t0

\t.END
"""
    return content

def main():
    # Parse instruction table
    codegen_path = '../mcom/codegen.c'
    if not os.path.exists(codegen_path):
        print(f"ERROR: {codegen_path} not found")
        return 1

    print("Parsing instruction table from codegen.c...")
    instructions = parse_instruction_table(codegen_path)
    print(f"Found {len(instructions)} instructions")

    # Count by architecture
    arch_counts = {'pdp10': 0, 'pdp11': 0, 'vax': 0}
    for inst in instructions:
        arch_counts[inst['arch']] = arch_counts.get(inst['arch'], 0) + 1

    print(f"  PDP-10: {arch_counts.get('pdp10', 0)} instructions")
    print(f"  PDP-11: {arch_counts.get('pdp11', 0)} instructions")
    print(f"  VAX: {arch_counts.get('vax', 0)} instructions")
    print()

    # Create test directories
    for arch in ['pdp10', 'pdp11', 'vax']:
        os.makedirs(arch, exist_ok=True)

    # Generate test files
    generated = 0
    skipped = 0

    for inst in instructions:
        name = inst['name'].lower()
        arch = inst['arch']
        test_file = f"{arch}/test_{name}.mac"

        if os.path.exists(test_file):
            skipped += 1
            continue

        # Generate test content
        if arch == 'pdp10':
            args, desc, expected, setup = get_instruction_test_data(inst['name'], arch)
            content = generate_pdp10_test(inst['name'], args, desc, expected, setup)
        elif arch == 'pdp11':
            content = generate_pdp11_test(inst['name'])
        elif arch == 'vax':
            content = generate_vax_test(inst['name'])
        else:
            continue

        # Write test file
        with open(test_file, 'w') as f:
            f.write(content)
        generated += 1

    print(f"Generated {generated} new test files")
    print(f"Skipped {skipped} existing test files")
    print(f"Total test files: {generated + skipped}")

    return 0

if __name__ == '__main__':
    sys.exit(main())
