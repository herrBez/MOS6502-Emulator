#include "../header/MOS6502.h"

void create_MOS_6502(int memory_size) {
	m = malloc(sizeof(MOS6502));
	m->A = 0x0; //Accumulator
	m->X = 0x0;
	m->Y = 0x0;
	m->PC = 0x600; // Start position of the programm
	m->S = 0x0;
	m->P = 0x0;
	m->memory = (unsigned char *) malloc(sizeof(unsigned char) * memory_size);
	int i;
	for(i = 0x0; i < 0x100; i++)
		m->instructions[i] = &ExitOnUnrecognizedInstruction;
	m->instructions[0x69] = &ADCIMM;
	m->instructions[0x18] = &CLC;
	m->instructions[0xD8] = &CLD;
	m->instructions[0x58] = &CLI;
	m->instructions[0xB8] = &CLV;
	m->instructions[0x38] = &SEC;
	m->instructions[0xF8] = &SED;
	m->instructions[0x78] = &SEI;
	
	#ifdef DEBUG
	#endif
}



void free_MOS_6502(){
	free(m->memory);
	free(m);
}



void loadGameIntoMemory(unsigned char * game, int length){
	int i = m->PC; // Start address
	int j;
	for(j = 0; j < length; i++, j++){
		m->memory[i] = game[j];
	}
	m->lastByte = i;
}


void emulateCycle(){
	unsigned char opcode = m->memory[m->PC++];
	(*m->instructions[opcode])();
}

void mainLoop(){
	while(m->PC < m->lastByte){
		emulateCycle();
	}
}


#ifdef DEBUG
void mainLoopDebug(){
	while(m->PC < m->lastByte){
		printStatus();
		emulateCycle();
		char c;
		scanf("%c", &c);
		while((c=getc(stdin)) != EOF && c != '\n');
	}
}

void printStatus(){
	printf("------------\n");
	printf("A\t%4.02X\n", m->A);
	printf("X\t%4.02X\n", m->X);
	printf("Y\t%4.02X\n", m->Y);
	printf("PC\t%4.04X\n", m->PC);
	printf("S\t%4.02X\n", m->S);
	printf("P\t%01X|%01X|%01X|%01X|%01X|%01X|%01X|%01X\n", get_bit(m->P,7), get_bit(m->P,6), get_bit(m->P,5), get_bit(m->P,4), get_bit(m->P,3), get_bit(m->P,2), get_bit(m->P,1), get_bit(m->P,0));
	printf("Next Instruction(%X): %X\n", m->PC, m->memory[m->PC]);
	printMemory();
	printf("------------\n");
}

void printMemory(){ 
	int i;
	for(i = 0x600; i < m->lastByte; i++){
		printf("M[0x%03X] %02X\n", i, m->memory[i]);
	}
}
#endif

