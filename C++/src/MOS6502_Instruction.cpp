#include "../header/MOS6502.hpp"



/** Branch functions: use signed byte in order to move with an offset */
inline int MOS_6502::getRelativeOffset(int position){
	unsigned char offset = memory[position];
	int real_offset = offset;
	if(offset >= 0x80){
		//Negativo
		real_offset = -0x80 + (offset & 0x7F); 
	}
	return real_offset;
}




/******************************************************
 * 
 * ADC functions: Add memory to accumulator with carry
 * 
 ******************************************************/

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


/******************************************************
 * 
 * AND functions: AND memory with accumulator
 * 
 ******************************************************/


/**This instruction affects the accumulator; sets the zero flag
if the result in the accumulator is 0, otherwise resets the zero flag
sets the negative flag if the result in the accumulator has bit 7 on,
otherwise resets the negative flag.*/
inline void MOS_6502::AND(unsigned char mem){
	A &= mem;
	refresh_negative_and_zero_flags_on_register(A);
}

void MOS_6502::ANDIMM(){
	AND(memory[PC++]);
}

void MOS_6502::ANDZP(){
	AND(memory[memory[PC++]]);
}

void MOS_6502::ANDZPX(){
	AND(memory[memory[PC++] + X]);
}
void MOS_6502::ANDABS(){
	unsigned short address = (memory[PC] << 8) | memory[PC+1];
	PC += 2;
	AND(memory[address]);
}

void MOS_6502::ANDABSX(){
	unsigned short address = memory[PC] << 8 | memory[PC+1];
	PC += 2;
	address += X;
	AND(memory[address]);
}	
void MOS_6502::ANDABSY(){
	unsigned short address = (memory[PC] << 8) | memory[PC+1];
	PC += 2;
	address += Y;
	AND(memory[address]);
}
void MOS_6502::AND$ZPX() {
	unsigned short tmp = memory[PC++] + X;
	unsigned short address = (memory[tmp] << 8) + memory[tmp+1];
	AND(memory[address]);
}
void MOS_6502::ANDZPY(){
	unsigned short tmp = memory[PC++] + Y;
	unsigned short address = (memory[tmp] << 8) + memory[tmp+1];
	AND(memory[address]);
}


/******************************************************
 * 
 * Clear functions
 * 
 ******************************************************/

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



/******************************************************
 * 
 * CPX functions: Compare memory and Index X
 * 
 ******************************************************/

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

/******************************************************
 * 
 * CPY functions: Compare memory and Index Y
 * 
 ******************************************************/
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

/******************************************************
 * 
 * Decrement functions:
 * 
 ******************************************************/
void MOS_6502::DECZP(){
	refresh_negative_and_zero_flags_on_register(--memory[PC++]);
}

void MOS_6502::DECZPX(){
	refresh_negative_and_zero_flags_on_register(--memory[X]);
}
void MOS_6502::DECABS(){
	unsigned short address = memory[PC] << 8 | memory[PC+1];
	PC += 2;
	refresh_negative_and_zero_flags_on_register(--memory[address]);
}
void MOS_6502::DECABSX(){
	unsigned short address = memory[PC] << 8 | memory[PC+1];
	PC += 2;
	refresh_negative_and_zero_flags_on_register(--memory[address + X]);
}

void MOS_6502::DEX(){
	X--;
	refresh_negative_and_zero_flags_on_register(X);
}

void MOS_6502::DEY(){
	Y--;
	refresh_negative_and_zero_flags_on_register(Y);
}




/******************************************************
 * 
 * Increment functions:
 * 
 ******************************************************/
void MOS_6502::INCZP(){
	refresh_negative_and_zero_flags_on_register(++memory[PC++]);
}

void MOS_6502::INCZPX(){
	refresh_negative_and_zero_flags_on_register(++memory[X]);
}
void MOS_6502::INCABS(){
	unsigned short address = memory[PC] << 8 | memory[PC+1];
	PC += 2;
	refresh_negative_and_zero_flags_on_register(++memory[address]);
}
void MOS_6502::INCABSX(){
	unsigned short address = memory[PC] << 8 | memory[PC+1];
	PC += 2;
	refresh_negative_and_zero_flags_on_register(++memory[address + X]);
}

/*
 INX does not affect the carry or overflow flags;
 it sets the N flag if the result of the increment has a one in bit 7, otherwise resets N; 
 sets the Z flag if the result of the increment is 0, otherwise it resets the Z flag.*/
void MOS_6502::INCX(){
	X++;
	refresh_negative_and_zero_flags_on_register(X);
}

void MOS_6502::INCY(){
	Y++;
	refresh_negative_and_zero_flags_on_register(Y);
}

/*********************************
 * NOP - Let's waste some time ;)
 ********************************/
void MOS_6502::NOP(){
	int tmp = A;
	A += rand();
	A = tmp;
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


/******************************************************
 * 
 * Stack functions
 * 
 ******************************************************/
/* Push accumulator on stack */
void MOS_6502::PHA(){
	memory[S--] = A;
	
	if(S < 0x100){
		fprintf(stderr, "[PHA]: Out of stack space\n");
		PC = lastByte + 1;
	}
}

/* PUSH */
void MOS_6502::PHP(){
	memory[S--] = P;
	if(S < 0x100) {
		fprintf(stderr,"[PHP:] Out of stack space\n");
		PC = lastByte + 1;
	}
}

/* PULL A */
void MOS_6502::PLA(){
	A = memory[++S];
	if(S > 0x1FF) {
		fprintf(stderr,"[PULL:] Stack empty\n");
		PC = lastByte + 1;
	}
}

/* Pull P */
void MOS_6502::PLP(){
	P = memory[++S];
	if(S > 0x1FF) {
		fprintf(stderr,"[PULL:] Stack empty.\n");
		PC = lastByte + 1;
	}
}


/******************************************************
 * 
 * Set functions:
 * 
 ******************************************************/
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
