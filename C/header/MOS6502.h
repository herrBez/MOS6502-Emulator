#ifndef __MOS_6502__H__
#define __MOS_6502__H__
#include <stdio.h>
#include <stdlib.h>

typedef struct _MOS6502 { 
		unsigned char A;		// Accumulator
		unsigned char X;		// Index Register X
		unsigned char Y;		// Index Register Y
		unsigned short PC;		// Program Counter
		unsigned char S;		// Maybe I have to add another bit?
		unsigned char P; 		// Process status register
		unsigned char * memory; // Memory
		unsigned short lastByte; // Last instruction loaded 
		void (*instructions[0x100])(void);
}MOS6502;


extern MOS6502 * m;
void create_MOS_6502(int);
void free_MOS_6502();
void loadGameIntoMemory(unsigned char * game, int length);
void emulateCycle();

void mainLoop();






/*Function prototypes */
int getRelativeOffset(int position);
void ADCIMM();
void CLC();
void CLD();
/** Clear interrupt result status */
void CLI();
/** Clear overflow flag */
void CLV();
/** Set Carry Flag to 1 */
void SEC();
void SED();
void SEI();
/** */
void ExitOnUnrecognizedInstruction();

#ifdef DEBUG
void mainLoopDebug();

void printStatus();

void printMemory();
#endif	
/** Defining the flags corresponding to the bit */
/** Defining the flags corresponding to the bit */
#define FLAG_NEGATIVE 7 //Negative Flag
#define FLAG_OVERFLOW 6 //Overflow 
#define FLAG_EXPANSION 5 //Expansion
#define FLAG_BRK_COMMAND 4 //BRK COMMAND
#define FLAG_DECIMAL_MODE 3 //Decimal Mode 
#define FLAG_IRQ_DISABLE 2 //IRQ Disable
#define FLAG_ZERO 1 //Zero flag
#define FLAG_CARRY 0 //Carry 1 if true


#define set_bit(number, x)\
	 number |= (1 << x);
#define clear_bit(number, x)\
	number &=  ~(1 << x);
#define toggle_bit(number, x)\
	number ^= (1 << x);
	
#define get_bit(number, x)\
	((number >> x) & 1)	

#define set_flag(x)\
	set_bit(m->P, x)
#define clear_flag(x)\
	clear_bit(m->P, x)
#define toggle_flag(x)\
	toggle_bit(m->P, x)

#define reset_negative_and_zero_flags()\
	clear_bit(m->P,FLAG_NEGATIVE)\
	clear_bit(m->P,FLAG_ZERO)
	
#define set_negative_and_zero_flags_if_neccessary()\
	if(m->A == 0x0)\
		set_bit(m->P,FLAG_ZERO)\
	if(m->A >= 0x80)\
		set_bit(m->P,FLAG_NEGATIVE)

/** Using macro it should be faster than using a function call */
#define CARRY get_bit(m->P, FLAG_CARRY)

#endif

