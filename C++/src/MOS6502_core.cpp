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
	instructions[0x69] = &MOS_6502::ADCIMM;
	instructions[0x18] = &MOS_6502::CLC;
	instructions[0xD8] = &MOS_6502::CLD;
	instructions[0x58] = &MOS_6502::CLI;
	instructions[0xB8] = &MOS_6502::CLV;
	instructions[0x38] = &MOS_6502::SEC;
	instructions[0xF8] = &MOS_6502::SED;
	instructions[0x78] = &MOS_6502::SEI;
	instructions[0x48] = &MOS_6502::PHA;
	instructions[0x08] = &MOS_6502::PHP;
	instructions[0x68] = &MOS_6502::PLA;
	instructions[0x28] = &MOS_6502::PLP;
	
	#ifdef DEBUG
	#endif
}



MOS_6502::~MOS_6502(){
	free(memory);
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

