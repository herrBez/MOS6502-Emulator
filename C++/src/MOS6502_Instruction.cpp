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

inline unsigned short MOS_6502::fetchAddressIMM(){
	return PC++;
}

inline unsigned short MOS_6502::fetchAddressZP(){
	return memory[PC++];
}
inline unsigned short MOS_6502::fetchAddressZPX(){
	return memory[PC++] + X;
}
inline unsigned short MOS_6502::fetchAddressABS(){
	unsigned short address = (memory[PC] << 8) | memory[PC+1];
	PC += 2;
	return address;
}
inline unsigned short MOS_6502::fetchAddressABSX(){
	return fetchAddressABS() + X;
}
inline unsigned short MOS_6502::fetchAddressABSY(){
	return fetchAddressABS() + Y;
}
inline unsigned short MOS_6502::fetchAddress$ZPX(){
	unsigned short tmp = memory[PC++] + X;
	return (memory[tmp] << 8) + memory[tmp+1];
}
inline unsigned short MOS_6502::fetchAddressZPY(){
	unsigned short tmp = memory[PC++] + Y;
	return (memory[tmp] << 8) + memory[tmp+1];
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

inline void MOS_6502::ADC(unsigned char mem) {
	unsigned short limit = get_flag(FLAG_DECIMAL_MODE) == 1?100:0x100;
	unsigned short tmp = A + mem + CARRY;
	clear_bit(P, FLAG_CARRY);
	reset_negative_and_zero_flags();
	if(tmp >= limit)
		set_bit(P, FLAG_CARRY);//Add carry
	A = (tmp % limit);
	refresh_negative_and_zero_flags_on_register(A);
}

void MOS_6502::ADCIMM(){
	ADC(memory[PC++]);
}

void MOS_6502::ADCZP(){
	ADC(memory[fetchAddressZP()]);
}

void MOS_6502::ADCZPX(){
	AND(memory[fetchAddressZPX()]);
}
void MOS_6502::ADCABS(){
	AND(memory[fetchAddressABS()]);
}

void MOS_6502::ADCABSX(){
	ADC(memory[fetchAddressZPX()]);
}	
void MOS_6502::ADCABSY(){
	ADC(memory[fetchAddressABSY()]);
}
void MOS_6502::ADC$ZPX() {
	ADC(memory[fetchAddress$ZPX()]);
}
void MOS_6502::ADCZPY(){
	ADC(memory[fetchAddressZPY()]);
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
	AND(memory[fetchAddressZP()]);
}

void MOS_6502::ANDZPX(){
	AND(memory[fetchAddressZPX()]);
}
void MOS_6502::ANDABS(){
	AND(memory[fetchAddressABS()]);
}

void MOS_6502::ANDABSX(){
	AND(memory[fetchAddressABSX()]);
}	
void MOS_6502::ANDABSY(){
	AND(memory[fetchAddressABSY()]);
}
void MOS_6502::AND$ZPX() {
	AND(memory[fetchAddress$ZPX()]);
}
void MOS_6502::ANDZPY(){
	AND(memory[fetchAddressZPY()]);
}

/******************************************************
 * 
 * ASL Shift left one bit (Memory or accumulator )
 * 
 ******************************************************/
inline void MOS_6502::ASL(unsigned char * mem){
	if(*mem >= 0x80)
		set_flag(FLAG_CARRY);
	*mem = *mem << 1;
	refresh_negative_and_zero_flags_on_register(*mem);
}
void MOS_6502::ASLA(){
	ASL(&A);
}

void MOS_6502::ASLZP(){
	ASL(&memory[fetchAddressZP()]);
}
void MOS_6502::ASLZPX(){
	ASL(&memory[fetchAddressZPX()]);
}
void MOS_6502::ASLABS(){
	ASL(&memory[fetchAddressABS()]);
	
}
void MOS_6502::ASLABSX(){
	ASL(&memory[fetchAddressABSX()]);
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
	CPXHelp(memory[fetchAddressZP()]);
}

void MOS_6502::CPXABS(){
	CPXHelp(memory[fetchAddressABS()]);
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
	CPXHelp(memory[fetchAddressZP()]);
}

void MOS_6502::CPYABS(){
	CPXHelp(memory[fetchAddressABS()]);
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
	refresh_negative_and_zero_flags_on_register(--X);
}

void MOS_6502::DEY(){
	refresh_negative_and_zero_flags_on_register(--Y);
}
/******************************************************
 * 
 * XOR functions:
 * 
 ******************************************************/
 
inline void MOS_6502::EOR(unsigned char mem){
	A ^= mem;
	refresh_negative_and_zero_flags_on_register(A);
}
void MOS_6502::EORIMM(){
	EOR(memory[PC++]);
}

void MOS_6502::EORZP(){
	EOR(memory[fetchAddressZP()]);
}
void MOS_6502::EORZPX(){
	EOR(memory[fetchAddressZPX()]);	
}
void MOS_6502::EORABS(){
	EOR(memory[fetchAddressABS()]);
}

void MOS_6502::EORABSX(){
	EOR(memory[fetchAddressABSX()]);
}
void MOS_6502::EORABSY() {
	EOR(memory[fetchAddressABSY()]);
}
void MOS_6502::EOR$ZPX(){
	EOR(memory[fetchAddress$ZPX()]);
}
void MOS_6502::EORZPY(){
	EOR(memory[fetchAddressZPY()]);
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
 * LDA - Load Accumulator with memory
 ********************************/
void MOS_6502::LDA(unsigned char mem){
	A = mem;
	refresh_negative_and_zero_flags_on_register(A);
}
void MOS_6502::LDAIMM(){
	LDA(memory[PC++]);
}
void MOS_6502::LDAZP(){
	LDA(memory[fetchAddressZP()]);
}
void MOS_6502::LDAZPX(){
	LDA(memory[fetchAddressZPX()]);
}
void MOS_6502::LDAABS(){
	LDA(memory[fetchAddressABS()]);
}
void MOS_6502::LDAABSX(){
	LDA(memory[fetchAddressABSX()]);
}
void MOS_6502::LDAABSY(){
	LDA(memory[fetchAddressABSY()]);
}
void MOS_6502::LDA$ZPX(){
	LDA(memory[fetchAddress$ZPX()]);
}
void MOS_6502::LDAZPY(){
	LDA(memory[fetchAddressZPY()]);
}

/*********************************
 * LDX - Load X with memory
 ********************************/
inline void MOS_6502::LDX(unsigned char mem){
	X = mem;
	refresh_negative_and_zero_flags_on_register(X);
}
void MOS_6502::LDXIMM(){
	LDX(memory[PC++]);
}
void MOS_6502::LDXZP(){
	LDX(memory[fetchAddressZP()]);
}
void MOS_6502::LDXABS(){
	LDX(memory[fetchAddressABS()]);
}
void MOS_6502::LDXABSY(){
	LDX(memory[fetchAddressABSY()]);
}
void MOS_6502::LDXZPY(){
	LDX(memory[fetchAddressZPY()]);
}

/*********************************
 * LDX - Load Y with memory
 ********************************/

inline void MOS_6502::LDY(unsigned char mem){
	Y = mem;
	refresh_negative_and_zero_flags_on_register(Y);
}
void MOS_6502::LDYIMM(){
	LDY(memory[PC++]);
}
void MOS_6502::LDYZP(){
	LDY(memory[fetchAddressZP()]);
}
void MOS_6502::LDYABS(){
	LDY(memory[fetchAddressABS()]);
}
void MOS_6502::LDYABSX(){
	LDY(memory[fetchAddressABSX()]);
}
void MOS_6502::LDYZPX(){
	LDY(memory[fetchAddressZPX()]);
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
inline void MOS_6502::OR(unsigned char mem){
	A |= mem;
	refresh_negative_and_zero_flags_on_register(A);
}

void MOS_6502::ORAIMM(){
	OR(memory[fetchAddressIMM()]);
}

void MOS_6502::ORAZP(){
	OR(memory[fetchAddressZP()]);
}
void MOS_6502::ORAZPX(){
	OR(memory[fetchAddressZPX()]);
}
void MOS_6502::ORAABS(){
	OR(memory[fetchAddressABS()]);
}

void MOS_6502::ORAABSX(){
	OR(memory[fetchAddressABSX()]);
}

void MOS_6502::ORAABSY(){
	OR(memory[fetchAddressABSY()]);
}

void MOS_6502::ORA$ZPX(){
	OR(memory[fetchAddress$ZPX()]);
}
void MOS_6502::ORAZPY(){
	OR(memory[fetchAddressZPY()]);
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
 * ROL functions:
 * 
 ******************************************************/
inline void MOS_6502::ROL(unsigned char * mem){
	if(get_bit(*mem, 7))
		set_flag(FLAG_CARRY);
	*mem = *mem << 1;
	refresh_negative_and_zero_flags_on_register(*mem);
}
void MOS_6502::ROLA(){
	ROL(&A);
}
void MOS_6502::ROLZP(){
	ROL(&memory[fetchAddressZP()]);
}
void MOS_6502::ROLZPX(){
		ROL(&memory[fetchAddressZPX()]);
}

void MOS_6502::ROLABS(){
		ROL(&memory[fetchAddressABS()]);
}
void MOS_6502::ROLABSX(){
		ROL(&memory[fetchAddressABSX()]);
}


/******************************************************
 * 
 * ROR functions:
 * 
 ******************************************************/
inline void MOS_6502::ROR(unsigned char * mem){
	unsigned char input_carry = CARRY;
	put_flag(FLAG_CARRY, get_bit(*mem, 0));
	*mem = ((*mem) >> 1);
	*mem |= (input_carry << 7);
	refresh_negative_and_zero_flags_on_register(*mem);
}
void MOS_6502::RORA(){
	ROR(&A);
}
void MOS_6502::RORZP(){
	ROR(&memory[fetchAddressZP()]);
}
void MOS_6502::RORZPX(){
	ROR(&memory[fetchAddressZPX()]);
}

void MOS_6502::RORABS(){
	ROR(&memory[fetchAddressABS()]);
}
void MOS_6502::RORABSX(){
	ROR(&memory[fetchAddressABSX()]);
}
/******************************************************
 * 
 * SBC functions:
 * 
 ******************************************************/
inline void MOS_6502::SBC(unsigned char mem){
	unsigned char twos_complement_of_mem = mem ^ 0xFF;
	twos_complement_of_mem += CARRY;
	unsigned short tmp = A + twos_complement_of_mem;
	if(tmp > 0xFF) {
		set_flag(FLAG_CARRY);
	}
	else {
		clear_flag(FLAG_CARRY);
	}
	A = (unsigned char) (tmp % 0x100);
	if(A >= 127 || ((char)A) < -127) {
		set_flag(FLAG_OVERFLOW);
	}
	else
		clear_flag(FLAG_OVERFLOW);
	refresh_negative_and_zero_flags_on_register(A);
		
}
void MOS_6502::SBCIMM(){
	SBC(memory[fetchAddressIMM()]);
}
void MOS_6502::SBCZP() {
	SBC(memory[fetchAddressZP()]);
}
void MOS_6502::SBCZPX() {
	SBC(memory[fetchAddressZPX()]);
}
void MOS_6502::SBCABS(){
	SBC(memory[fetchAddressABS()]);
}
void MOS_6502::SBCABSX(){
	SBC(memory[fetchAddressABSX()]);
}
void MOS_6502::SBCABSY(){
	SBC(memory[fetchAddressABSY()]);
}
void MOS_6502::SBC$ZPX() {
	SBC(memory[fetchAddress$ZPX()]);
}
void MOS_6502::SBCZPY(){
	SBC(memory[fetchAddressZPY()]);
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
	fprintf(stderr, "Unrecognized (or not yet supported ) Instruction %d. Exit ...\n", memory[PC]);
	PC = lastByte; /* In order to exit gracefully */
}
