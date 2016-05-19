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
		unsigned short S;		// From address 0x1FF to 0x100
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
		inline int getRelativeOffset(int position);
		inline unsigned short fetchAddressIMM();
		inline unsigned short fetchAddressZP();
		inline unsigned short fetchAddressZPX();
		inline unsigned short fetchAddressABS();
		inline unsigned short fetchAddressABSX();
		inline unsigned short fetchAddressABSY();
		inline unsigned short fetchAddress$ZPX();
		inline unsigned short fetchAddressZPY();
		/*******************
		 *  FUNCTIONS 
		 *******************/
		void ExitOnUnrecognizedInstruction();
		/* ADC functions */
		inline void ADC(unsigned char mem);
		void ADCIMM();
		void ADCZP();
		void ADCZPX();
		void ADCABS();
		void ADCABSX();
		void ADCABSY();
		void ADC$ZPX(); //ADC(ZP,X)
		void ADCZPY();
		
		/* AND functions */
		inline void AND(unsigned char mem);
		void ANDIMM();
		void ANDZP();
		void ANDZPX();
		void ANDABS();
		void ANDABSX();
		void ANDABSY();
		void AND$ZPX(); //ADC(ZP,X)
		void ANDZPY();
		
		/* ASL functions */
		inline void ASL(unsigned char * mem);
		void ASLA();
		void ASLZP();
		void ASLZPX();
		void ASLABS();
		void ASLABSX();
				
		/* Clear flags */
		void CLC();
		void CLD();
		void CLI();
		void CLV();
		
		/* Compare functions with X*/
		inline void CPXHelp(unsigned char mem);
		void CPXIMM();
		void CPXZP();
		void CPXABS();
		
		/* Compare functions with Y */
		inline void CPYHelp(unsigned char mem);
		void CPYIMM();
		void CPYZP();
		void CPYABS();
		
		/* Decrement functions */
		void DEX();
		void DEY();
		void DECZP();
		void DECZPX();
		void DECABS();
		void DECABSX();
		/* Exclusive OR functions */
		inline void EOR(unsigned char mem);
		void EORIMM();
		void EORZP();
		void EORZPX();
		void EORABS();
		void EORABSX();
		void EORABSY();
		void EOR$ZPX();
		void EORZPY();
		
		/* Increment functions */
		void INCZP();
		void INCZPX();
		void INCABS();
		void INCABSX();
		void INCX();
		void INCY();
		
		/* Load Accumulator with memory functions*/
		inline void LDA(unsigned char mem);
		void LDAIMM();
		void LDAZP();
		void LDAZPX();
		void LDAABS();
		void LDAABSX();
		void LDAABSY();
		void LDA$ZPX();
		void LDAZPY();
		
		/* Load X with memory functions*/
		inline void LDX(unsigned char mem);
		void LDXIMM();
		void LDXZP();
		void LDXABS();
		void LDXABSY();
		void LDXZPY();
		
		inline void LDY(unsigned char mem);
		void LDYIMM();
		void LDYZP();
		void LDYZPX();
		void LDYABS();
		void LDYABSX();
		/* NOP function */
		void NOP();
		
		/* OR with accumulator functions */
		inline void OR(unsigned char mem);
		void ORAIMM();
		void ORAZP();
		void ORAZPX();
		void ORAABS();
		void ORAABSX();
		void ORAABSY();
		void ORA$ZPX();
		void ORAZPY();
		
		
		/* Push on or Pull from stack the process status flag P/Accumulator*/
		void PHA();
		void PHP();
		void PLA();
		void PLP();
		
		/* Set flags */
		void SEC();
		void SED();
		void SEI();
		
		/* Transfer Accumulator to index X, Y */
		void TAX();
		void TAY();
		/* Transfer index X, Y to Accumulator */
		void TXA();
		void TYA();
		
		/* Transfer stack pointer to index X */
		void TSX();
		
	

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
	 number |= (1 << x)
#define clear_bit(number, x)\
	number &=  ~(1 << x)
#define toggle_bit(number, x)\
	number ^= (1 << x);
#define get_bit(number, x)\
	((number >> x) & 1)	

#define set_flag(x)\
	set_bit(P, x)
#define get_flag(x)\
	get_bit(P, x)
#define clear_flag(x)\
	clear_bit(P, x)
#define toggle_flag(x)\
	toggle_bit(P, x)

#define reset_negative_and_zero_flags()\
	clear_bit(P,FLAG_NEGATIVE);\
	clear_bit(P,FLAG_ZERO)
	
#define set_negative_and_zero_flags_if_neccessary()\
	if(A == 0x0)\
		set_bit(P,FLAG_ZERO);\
	if(A >= 0x80)\
		set_bit(P,FLAG_NEGATIVE)

#define refresh_negative_and_zero_flags_on_register(reg)\
	if(reg == 0)\
		set_flag(FLAG_ZERO);\
	else \
		clear_flag(FLAG_ZERO);\
	if(reg >= 0x80)\
		set_flag(FLAG_NEGATIVE);\
	else \
		clear_flag(FLAG_NEGATIVE);

/** Using macro it should be faster than using a function call */
#define CARRY get_bit(P, FLAG_CARRY)

#endif
