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
	
	
}
/* decrement index register Y */
void MOS_6502::DEY(){
	
	
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
	refresh_negative_and_zero_flags();
}

void MOS_6502::INY(){
	Y++;
	refresh_negative_and_zero_flags();
}

/**
 This instruction affects the accumulator; sets the zero flag
if the result in the accumulator is 0, otherwise resets the zero flag;
sets the negative flag if the result in the accumulator has bit 7 on,
otherwise resets the negative flag.*/

void MOS_6502::ORAIMM(){
	A |= memory[PC++];
	refresh_negative_and_zero_flags();
}

void MOS_6502::ORAZP(){
	A |= memory[memory[PC++]];
	refresh_negative_and_zero_flags();
}
void MOS_6502::ORAZPX(){
	
}
void MOS_6502::ORABS(){
	unsigned short address = (memory[PC] << 8) | (memory[PC+1]);
	PC += 2;
	A |= memory[address];
	refresh_negative_and_zero_flags();
}

/** Set Carry Flag to 1 */
void MOS_6502::SEC(){
	set_flag(FLAG_CARRY);
}

void MOS_6502::SED(){
	set_flag(FLAG_DECIMAL_MODE);
}

void MOS_6502::SEI(){
	set_flag(FLAG_IRQ_DISABLE);
}

/** */
void MOS_6502::ExitOnUnrecognizedInstruction(){
	fprintf(stderr, "Unrecognized Instruction %d. Exit ...\n", memory[PC]);
	PC = lastByte; /* In order to exit gracefully */
}
