@echo off
rem Copyright (c) 2025 PCC Project
rem
rem Batch File VM Executor
rem
rem A minimal VM implementation in Windows batch for bootstrapping
rem on DOS/Windows systems without a C compiler.
rem
rem Usage: vmbat.bat program.vms

setlocal enabledelayedexpansion

rem Initialize registers
set r0=0
set r1=0
set r2=0
set r3=0
set r4=0
set r5=0
set r6=0
set r7=0
set r8=0
set r9=0
set r10=0
set r11=0
set r12=0
set r13=0
set sp=0
set fp=0
set pc=0

rem Flags
set flag_z=0
set flag_n=0

rem Program storage
set prog_count=0
set running=1

rem Check arguments
if "%1"=="" (
	echo Usage: %0 program.vms
	exit /b 1
)

if not exist "%1" (
	echo Error: File not found: %1
	exit /b 1
)

rem Load program
set line_num=0
for /f "usebackq delims=" %%a in ("%1") do (
	set "line=%%a"
	rem Skip empty lines and comments
	if not "!line!"=="" (
		if not "!line:~0,1!"=="#" (
			if not "!line:~0,1!"==";" (
				set "prog_!line_num!=!line!"
				set /a line_num+=1
			)
		)
	)
)
set prog_count=%line_num%

rem Main execution loop
:execute_loop
if %running%==0 goto end
if %pc% GEQ %prog_count% goto end

rem Get instruction
set "instr=!prog_%pc%!"

rem Skip labels
echo !instr! | findstr /r ".*:" >nul
if %errorlevel%==0 (
	set /a pc+=1
	goto execute_loop
)

rem Parse opcode (first token)
for /f "tokens=1,2,3,4 delims= ," %%a in ("!instr!") do (
	set opcode=%%a
	set arg1=%%b
	set arg2=%%c
	set arg3=%%d
)

rem Execute instruction
if "!opcode!"=="nop" goto next_instr
if "!opcode!"=="halt" (
	set running=0
	goto end
)

if "!opcode!"=="ldi" (
	set "!arg1!=!arg2!"
	goto next_instr
)

if "!opcode!"=="mov" (
	set "val=!%arg2%!"
	set "!arg1!=!val!"
	goto next_instr
)

if "!opcode!"=="add" (
	set "val1=!%arg2%!"
	set "val2=!%arg3%!"
	set /a "result=!val1! + !val2!"
	set "!arg1!=!result!"
	call :update_flags !result!
	goto next_instr
)

if "!opcode!"=="sub" (
	set "val1=!%arg2%!"
	set "val2=!%arg3%!"
	set /a "result=!val1! - !val2!"
	set "!arg1!=!result!"
	call :update_flags !result!
	goto next_instr
)

if "!opcode!"=="mul" (
	set "val1=!%arg2%!"
	set "val2=!%arg3%!"
	set /a "result=!val1! * !val2!"
	set "!arg1!=!result!"
	call :update_flags !result!
	goto next_instr
)

if "!opcode!"=="div" (
	set "val1=!%arg2%!"
	set "val2=!%arg3%!"
	if !val2!==0 (
		echo Error: Division by zero
		exit /b 1
	)
	set /a "result=!val1! / !val2!"
	set "!arg1!=!result!"
	call :update_flags !result!
	goto next_instr
)

if "!opcode!"=="addi" (
	set "val=!%arg2%!"
	set /a "result=!val! + !arg3!"
	set "!arg1!=!result!"
	call :update_flags !result!
	goto next_instr
)

if "!opcode!"=="subi" (
	set "val=!%arg2%!"
	set /a "result=!val! - !arg3!"
	set "!arg1!=!result!"
	call :update_flags !result!
	goto next_instr
)

if "!opcode!"=="cmp" (
	set "val1=!%arg1%!"
	set "val2=!%arg2%!"
	set /a "result=!val1! - !val2!"
	call :update_flags !result!
	goto next_instr
)

if "!opcode!"=="jmp" (
	set pc=!arg1!
	goto execute_loop
)

if "!opcode!"=="jz" (
	if !flag_z!==1 (
		set pc=!arg1!
		goto execute_loop
	)
	goto next_instr
)

if "!opcode!"=="jnz" (
	if !flag_z!==0 (
		set pc=!arg1!
		goto execute_loop
	)
	goto next_instr
)

if "!opcode!"=="jl" (
	if !flag_n!==1 (
		set pc=!arg1!
		goto execute_loop
	)
	goto next_instr
)

if "!opcode!"=="jg" (
	if !flag_z!==0 (
		if !flag_n!==0 (
			set pc=!arg1!
			goto execute_loop
		)
	)
	goto next_instr
)

if "!opcode!"=="print" (
	set "val=!%arg1%!"
	echo !val!
	goto next_instr
)

echo Error: Unknown opcode: !opcode!
exit /b 1

:next_instr
set /a pc+=1
goto execute_loop

:update_flags
set val=%1
if %val%==0 (
	set flag_z=1
) else (
	set flag_z=0
)
if %val% LSS 0 (
	set flag_n=1
) else (
	set flag_n=0
)
exit /b 0

:end
exit /b %r0%
