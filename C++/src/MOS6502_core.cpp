#include "../header/MOS6502.hpp"

MOS_6502::MOS_6502(int memory_size) {
	A = 0x0; //Accumulator
	X = 0x0;
	Y = 0x0;
	PC = 0x600; // Start position of the programm
	S = 0x1FF;
	P = 0x0;
	memory = (unsigned char *) malloc(sizeof(unsigned char) * memory_size);
	for(int i = 0x0; i < 0x100; i++)
		instructions[i] = &MOS_6502::ExitOnUnrecognizedInstruction;
	/******************************
	 *  Add Accumulator with carry
	 ******************************/
	instructions[0x69] = &MOS_6502::ADCIMM;
	instructions[0x65] = &MOS_6502::ADCZP;
	instructions[0x75] = &MOS_6502::ADCZPX;
	instructions[0x6D] = &MOS_6502::ADCABS;
	instructions[0x7D] = &MOS_6502::ADCABSX;
	instructions[0x79] = &MOS_6502::ADCABSY;
	instructions[0x61] = &MOS_6502::ADC$ZPX;
	instructions[0x71] = &MOS_6502::ADCZPY;  
	/********************************
	 *  AND memory with accumulator
	 ********************************/
	instructions[0x29] = &MOS_6502::ANDIMM;
	instructions[0x25] = &MOS_6502::ANDZP;
	instructions[0x35] = &MOS_6502::ANDZPX;
	instructions[0x2D] = &MOS_6502::ANDABS;
	instructions[0x3D] = &MOS_6502::ANDABSX;
	instructions[0x39] = &MOS_6502::ANDABSY;
	instructions[0x21] = &MOS_6502::AND$ZPX;
	instructions[0x31] = &MOS_6502::ANDZPY;  
	/*********************************
	 * ASL Arithmetic shift left
	 *********************************/
	instructions[0x0A] = &MOS_6502::ASLA;
	instructions[0x06] = &MOS_6502::ASLZP;
	instructions[0x16] = &MOS_6502::ASLZPX;
	instructions[0x0E] = &MOS_6502::ASLABS;
	instructions[0x1E] = &MOS_6502::ASLABSX;
	/************************
	 *  Clear function
	 ************************/
	instructions[0x18] = &MOS_6502::CLC;
	instructions[0xD8] = &MOS_6502::CLD;
	instructions[0x58] = &MOS_6502::CLI;
	instructions[0xB8] = &MOS_6502::CLV;
	
	/*************************
	 *  Decrement functions
	 *************************/
	instructions[0xC6] = &MOS_6502::DECZP;
	instructions[0xD6] = &MOS_6502::DECZPX;
	instructions[0xCE] = &MOS_6502::DECABS;
	instructions[0xDE] = &MOS_6502::DECABSX;
	instructions[0xCA] = &MOS_6502::DEX;
	instructions[0x88] = &MOS_6502::DEY;
	
	/*************************
	 *  Increment functions
	 *************************/
	instructions[0xE6] = &MOS_6502::INCZP;
	instructions[0xF6] = &MOS_6502::INCZPX;
	instructions[0xEE] = &MOS_6502::INCABS;
	instructions[0xFE] = &MOS_6502::INCABSX;
	instructions[0xE8] = &MOS_6502::INCX;
	instructions[0xC8] = &MOS_6502::INCY;
	
	/****************************
	 * NOP
	 ****************************/
	instructions[0xEA] = &MOS_6502::NOP;
	/*************************
	 *  Set functions
	 *************************/
	instructions[0x38] = &MOS_6502::SEC;
	instructions[0xF8] = &MOS_6502::SED;
	instructions[0x78] = &MOS_6502::SEI;
	
	/*************************
	 * Basic Stack functions 
	 *************************/
	instructions[0x48] = &MOS_6502::PHA;
	instructions[0x08] = &MOS_6502::PHP;
	instructions[0x68] = &MOS_6502::PLA;
	instructions[0x28] = &MOS_6502::PLP;
	
}



MOS_6502::~MOS_6502(){
	free(memory);
	#ifdef DEBUG
	printf("\nCleaning up...\n");
	#endif
}



void MOS_6502::loadGameIntoMemory(unsigned char * game, int length){
	int i = PC; // Start address
	for(int j = 0; j < length; i++, j++){
		memory[i] = game[j];
	}
	lastByte = i;
}


void MOS_6502::emulateCycle(){
	unsigned char opcode = memory[PC++];
	(*this.*instructions[opcode])();
}

void MOS_6502::mainLoop(){
	while(PC < lastByte){
		emulateCycle();
	}
}


#ifdef DEBUG
void MOS_6502::mainLoopDebug(){
	while(PC < lastByte){
		printStatus();
		emulateCycle();
		char c;
		scanf("%c", &c);
		while((c=getc(stdin)) != EOF && c != '\n');
	}
}

void MOS_6502::printStatus(){
	printf("----------------------------\n");
	printf("A\t%4.02X\n", A);
	printf("X\t%4.02X\n", X);
	printf("Y\t%4.02X\n", Y);
	printf("PC\t%4.04X\n", PC);
	printf("S\t%4.02X\n", S);
	printf("P\t%01X|%01X|%01X|%01X|%01X|%01X|%01X|%01X\n", get_bit(P,7), get_bit(P,6), get_bit(P,5), get_bit(P,4), get_bit(P,3), get_bit(P,2), get_bit(P,1), get_bit(P,0));
	printf("Next Instruction(%X): %X\n", PC, memory[PC]);
	printMemory();
	/* Print stack -1 to max*/
	int start = S + 0x2;
	if(start > 0x1FF)
		start = 0x1FF;
	printf("Stack:\n");
	for(int i = start; i >= S - 0x2; i--){
		printf("M[0x%03X] 0x%02X %s\n", i, memory[i], i==S?"<--":" ");
	}
	printf("----------------------------\n");
}

void MOS_6502::printMemory(){ 
	for(int i = PC-2; i < PC+3; i++){
		printf("M[0x%03X] %02X %s\n", i, memory[i], i==PC?"<--":" ");
	}
}
#endif

