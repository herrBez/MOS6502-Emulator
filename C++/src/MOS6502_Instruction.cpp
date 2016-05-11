#include "../header/MOS6502.hpp"

/** Using macro it should be faster than using a function call */
#define CARRY get_bit(P, FLAG_CARRY)

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






/** */
void MOS_6502::ADCIMM(){
	unsigned short tmp = A + memory[PC++] + CARRY;
	clear_bit(P, FLAG_CARRY);
	reset_negative_and_zero_flags();
	if(tmp > 0xFF)
		set_bit(P, FLAG_CARRY);//Add carry
	
	A = (tmp % 0x100);
	set_negative_and_zero_flags_if_neccessary();
}
/** */
void MOS_6502::ADCZP(){
	unsigned short tmp = A + memory[memory[PC++]] + CARRY;
	clear_bit(P, FLAG_CARRY);
	if(tmp > 0xFF)
		set_bit(P, FLAG_CARRY); 
	A = (tmp % 0x100);
}




/** Branch on carry clear */
void MOS_6502::BCC(){
	if(P &= 0x01 == 0x1){ //Carry not clear
		PC++;
		return;
	}	
	PC += getRelativeOffset(PC) + 1;
}
/** Branch on carry set */
void MOS_6502::BBS(){
	if(P &= 0x01 == 0x0){ //Carry set
		PC += 2;
		return;
	}
	PC += getRelativeOffset(PC) + 1;
}

/** Branch on result zero --> which result?*/
void MOS_6502::BEQ(){
	if((P & 0x02) == 0x0){// Z Flag is zero  (Last result is zero??)
		PC += 2;
		return;
	}
	PC +=getRelativeOffset(PC) + 1;
}

/** Clear carry flag */
void MOS_6502::CLC(){
	clear_bit(P, FLAG_CARRY);
	PC++;
}

/** Clear overflow flag */
void MOS_6502::CLV(){
	clear_bit(P, FLAG_OVERFLOW);
	PC++;
}
/** LDA SET */
/** Load Accumulator with memory immediate*/
void MOS_6502::LDAIMM(){
	reset_negative_and_zero_flags();
	A = memory[PC++];
	set_negative_and_zero_flags_if_neccessary();
}

/** Set Carry Flag to 1 */
void MOS_6502::SEC(){
	set_bit(P, FLAG_CARRY);
	++PC;
}

/** */
void MOS_6502::ExitOnUnrecognizedInstruction(){
	fprintf(stderr, "Unrecognized Instruction %d. Exit ...\n", memory[PC]);
	PC = lastByte; /* In order to exit gracefully */
}
