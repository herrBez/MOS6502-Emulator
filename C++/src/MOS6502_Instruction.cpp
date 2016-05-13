#include "../header/MOS6502.hpp"



/** Branch functions: use signed byte in order to move with an offset */
int MOS_6502::getRelativeOffset(int position){
	unsigned char offset = memory[position];
	int real_offset = offset;
	if(offset >= 0x80){
		//Negativo
		real_offset = -0x80 + (offset & 0x7F); 
	}
	return real_offset;
}






/** This instruction affects the accumulator; sets the carry flag
when the sum of a binary add exceeds 255 or when the sum of a decimal
add exceeds 99, otherwise carry is reset.
The overflow flag is set
when the sign or bit 7 is changed due to the result exceeding +127
or -128, otherwise overflow is reset.
The negative flag is set if
the accumulator result contains bit 7 on, otherwise the negative
flag is reset.
The zero flag is set if the accumulator result is 0,
otherwise the zero flag is reset.*/

void MOS_6502::ADCIMM(){
	unsigned short limit = get_flag(FLAG_DECIMAL_MODE) == 1?100:0x100;
	unsigned short tmp = A + memory[PC++] + CARRY;
	clear_bit(P, FLAG_CARRY);
	reset_negative_and_zero_flags();
	if(tmp >= limit)
		set_bit(P, FLAG_CARRY);//Add carry
	A = (tmp % limit);
	set_negative_and_zero_flags_if_neccessary();
}
/**This instruction affects the accumulator; sets the zero flag
if the result in the accumulator is 0, otherwise resets the zero flag
sets the negative flag if the result in the accumulator has bit 7 on,
otherwise resets the negative flag.*/
void MOS_6502::ANDIMM(){
	A &= memory[PC++];
	if(A == 0)
		set_flag(FLAG_ZERO);	
	else 
		clear_flag(FLAG_ZERO);
	if(A >= 0x80)
		set_flag(FLAG_NEGATIVE);
	else
		clear_flag(FLAG_NEGATIVE);
}

/** Clear carry flag */
void MOS_6502::CLC(){
	clear_flag(FLAG_CARRY);
}

void MOS_6502::CLD(){
	clear_flag(FLAG_DECIMAL_MODE);
}

/** Clear interrupt result status */
void MOS_6502::CLI(){
	clear_flag(FLAG_IRQ_DISABLE);
}


/** Clear overflow flag */
void MOS_6502::CLV(){
	clear_flag(FLAG_OVERFLOW);
}


/** CPX functions */
/*The CPX instruction does not affect any register in the machine;
it also does not affect the overflow flag.
It causes the carry to be
set on if the absolute value of the index register X is equal to or
greater than the data from memory.
If the value of the memory is
greater than the content of the index register X, carry is reset.
If the results of the subtraction contain a bit 7, then the N flag
is set, if not, it is reset.
If the value in memory is equal to the
value in index register X, the Z flag is set, otherwise it is reset.*/

void MOS_6502::CPXHelp(unsigned char mem){
	unsigned char tmp = X - mem;
	if(X >= mem)
		set_flag(FLAG_CARRY);
	else
		clear_flag(FLAG_CARRY);
	if(get_bit(tmp, 7) == 1)
		set_flag(FLAG_NEGATIVE);
	else 
		clear_flag(FLAG_NEGATIVE);
}
void MOS_6502::CPXIMM(){
	CPXHelp(memory[PC++]);
}
void MOS_6502::CPXZP(){
	CPXHelp(memory[memory[PC++]]);
}

void MOS_6502::CPXABS(){
	unsigned short address = memory[PC] << 8 | memory[PC+1];
	PC += 2;
	CPXHelp(memory[address]);
}

/** CPY functions */
void MOS_6502::CPYHelp(unsigned char mem){
	unsigned char tmp = X - mem;
	if(X >= mem)
		set_flag(FLAG_CARRY);
	else
		clear_flag(FLAG_CARRY);
	if(get_bit(tmp, 7) == 1)
		set_flag(FLAG_NEGATIVE);
	else 
		clear_flag(FLAG_NEGATIVE);
}
void MOS_6502::CPYIMM(){
	CPXHelp(memory[PC++]);
}
void MOS_6502::CPYZP(){
	CPXHelp(memory[memory[PC++]]);
}

void MOS_6502::CPYABS(){
	unsigned short address = memory[PC] << 8 | memory[PC+1];
	PC += 2;
	CPXHelp(memory[address]);
}

/** Decrement functions */
/*
 DEX does not affect the carry or overflow flag, 
 it sets the N flag if it has bit 7 on as a result of the decrement,
 * otherwise it resets the N flag;
 * sets the Z flag if X is a 0 as a result of the decrement, otherwise
 * it resets the Z flag
 
 */
 /* decrement index register X */
void MOS_6502::DEX(){
	X--;
	refresh_negative_and_zero_flags_on_register(X);
}
/* decrement index register Y */
void MOS_6502::DEY(){
	Y--;
	refresh_negative_and_zero_flags_on_register(Y);
}

void MOS_6502::DECZP(){
	
}

void MOS_6502::DECZPX(){
}
void MOS_6502::DECABS(){
}
void MOS_6502::DECABSX(){
}



/** Increment functions */
/*
 INX does not affect the carry or overflow flags;
 it sets the N flag if the result of the increment has a one in bit 7, otherwise resets N; 
 sets the Z flag if the result of the increment is 0, otherwise it resets the Z flag.*/
void MOS_6502::INX(){
	X++;
	refresh_negative_and_zero_flags_on_register(X);
}

void MOS_6502::INY(){
	Y++;
	refresh_negative_and_zero_flags_on_register(Y);
}

/**
 This instruction affects the accumulator; sets the zero flag
if the result in the accumulator is 0, otherwise resets the zero flag;
sets the negative flag if the result in the accumulator has bit 7 on,
otherwise resets the negative flag.*/

void MOS_6502::ORAIMM(){
	A |= memory[PC++];
	refresh_negative_and_zero_flags_on_register(A);
}

void MOS_6502::ORAZP(){
	A |= memory[memory[PC++]];
	refresh_negative_and_zero_flags_on_register(A);
}
void MOS_6502::ORAZPX(){
	
}
void MOS_6502::ORABS(){
	unsigned short address = (memory[PC] << 8) | (memory[PC+1]);
	PC += 2;
	A |= memory[address];
	refresh_negative_and_zero_flags_on_register(A);
}

/**********************
 *  Stack operations 
 **********************/
/* Push accumulator on stack */
void MOS_6502::PHA(){
	memory[S--] = A;
	if(S < 0x100){
		fprintf(stderr, "[PHA]: Out of stack space\n");
		PC = lastByte + 1;
	}
}


void MOS_6502::PHP(){
	memory[S--] = P;
	if(S < 0x100) {
		fprintf(stderr,"[PHP:] Out of stack space\n");
		PC = lastByte + 1;
	}
}

void MOS_6502::PLA(){
	A = memory[S++];
	if(S > 0x1FF) {
		fprintf(stderr,"[PULL:] Stack too full\n");
		PC = lastByte + 1;
	}
}

void MOS_6502::PLP(){
	P = memory[S++];
	if(S > 0x1FF) {
		fprintf(stderr,"[PULL:] Stack too full\n");
		PC = lastByte + 1;
	}
}

/********************
 *  Set functions 
 ********************/
void MOS_6502::SEC(){
	set_flag(FLAG_CARRY);
}

void MOS_6502::SED(){
	set_flag(FLAG_DECIMAL_MODE);
}

void MOS_6502::SEI(){
	set_flag(FLAG_IRQ_DISABLE);
}
/*TAX only affects the index register X, does not affect the
  carry or overflow flags.
  * The N flag is set if the resultant value in the index register X has bit 7 on, otherwise N is reset.
	The Z bit is set if the content of the register X is 0 as a result of the opera­tion, otherwise it is reset.
TAX is a single byte instruction and its addressing mode is Implied.*/
void MOS_6502::TAX(){
	X = A;
	refresh_negative_and_zero_flags_on_register(X);
}

void MOS_6502::TXA(){
	A = X;
	refresh_negative_and_zero_flags_on_register(A);
}

void MOS_6502::TAY(){
	Y = A;
	refresh_negative_and_zero_flags_on_register(Y);
}

void MOS_6502::TYA(){
	A = Y;
	refresh_negative_and_zero_flags_on_register(A);
}

void MOS_6502::TSX(){
	X = memory[S];
	refresh_negative_and_zero_flags_on_register(X);
}

/** */
void MOS_6502::ExitOnUnrecognizedInstruction(){
	fprintf(stderr, "Unrecognized Instruction %d. Exit ...\n", memory[PC]);
	PC = lastByte; /* In order to exit gracefully */
}
