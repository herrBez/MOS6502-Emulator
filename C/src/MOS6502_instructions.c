#include "../header/MOS6502.h"



/** Branch functions: use signed byte in order to move with an offset */
int getRelativeOffset(int position){
	unsigned char offset = m->memory[position];
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

void ADCIMM(){
	unsigned short limit = get_bit(m->P, FLAG_DECIMAL_MODE) == 1?100:0x100;
	unsigned short tmp = m->A + m->memory[m->PC++] + CARRY;
	clear_flag(FLAG_CARRY);
	reset_negative_and_zero_flags();
	if(tmp >= limit)
		set_flag(FLAG_CARRY);//Add carry
	
	m->A = (tmp % limit);
	set_negative_and_zero_flags_if_neccessary();
}


/** Clear carry flag */
void CLC(){
	clear_flag(FLAG_CARRY);
}

void CLD(){
	clear_flag(FLAG_DECIMAL_MODE);
}

/** Clear interrupt result status */
void CLI(){
	clear_flag(FLAG_IRQ_DISABLE);
}


/** Clear overflow flag */
void CLV(){
	clear_flag(FLAG_OVERFLOW);
}




/** Set Carry Flag to 1 */
void SEC(){
	set_flag(FLAG_CARRY);
}

void SED(){
	set_flag(FLAG_DECIMAL_MODE);
}

void SEI(){
	set_flag(FLAG_IRQ_DISABLE);
}

/** */
void ExitOnUnrecognizedInstruction(){
	fprintf(stderr, "Unrecognized Instruction %d. Exit ...\n", m->memory[m->PC]);
	m->PC = m->lastByte; /* In order to exit gracefully */
}
