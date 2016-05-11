#ifndef __MOS_6502__H__
#define __MOS_6502__H__
#include <stdio.h>
#include <stdlib.h>

/**
 * 
 * 
 */
class MOS_6502{
	private: 
		unsigned char A;		// Accumulator
		unsigned char X;		// Index Register X
		unsigned char Y;		// Index Register Y
		unsigned short PC;		// Program Counter
		unsigned char S;		// Maybe I have to add another bit?
		unsigned char P; 		// Process status register
		unsigned char * memory; // Memory
		unsigned short lastByte; // Last instruction loaded 
		void (MOS_6502::*instructions[0x100])(void);
		
	
	public:
		MOS_6502(int);
		~MOS_6502();
		void loadGameIntoMemory(unsigned char * game, int length);
		void mainLoop();
		#ifdef DEBUG
		void mainLoopDebug();
		void printStatus();
		void printMemory();
		#endif
	
	
	private:
		void emulateCycle();
		int getRelativeOffset(int position);
		/** FUNCTIONS */
		void ExitOnUnrecognizedInstruction();
		void ADCIMM();
		
		
		
		void CLC();
		void CLD();
		void CLI();
		void CLV();
		
		
		/* Set carry flag */
		void SEC();
		void SED();
		void SEI();
		
	

};

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
	set_bit(P, x)
#define clear_flag(x)\
	clear_bit(P, x)
#define toggle_flag(x)\
	toggle_bit(P, x)

#define reset_negative_and_zero_flags()\
	clear_bit(P,FLAG_NEGATIVE)\
	clear_bit(P,FLAG_ZERO)
	
#define set_negative_and_zero_flags_if_neccessary()\
	if(A == 0x0)\
		set_bit(P,FLAG_ZERO)\
	if(A >= 0x80)\
		set_bit(P,FLAG_NEGATIVE)

/** Using macro it should be faster than using a function call */
#define CARRY get_bit(P, FLAG_CARRY)

#endif
