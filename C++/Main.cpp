#include "./header/MOS6502.hpp"


int main(int argc, char * argv[]){
	int length = 16;
	unsigned char * game = (unsigned char*) malloc(sizeof(unsigned char) * length);
	int i = 0;
	game[i++] = 0x38;
	game[i++] = 0xF8;
	game[i++] = 0x78;
	game[i++] = 0x18;
	game[i++] = 0x08;
	game[i++] = 0xD8;
	game[i++] = 0x58;
	game[i++] = 0xB8;
	game[i++] = 0x78;
	game[i++] = 0x08;
	game[i++] = 0xE8;
	game[i++] = 0x88;
	game[i++] = 0xEA;
	game[i++] = 0xEA;
	game[i++] = 0XEA;
	
	
	MOS_6502 m = MOS_6502(0x1000);
	m.loadGameIntoMemory(game, length);
	#ifndef DEBUG
	m.mainLoop();
	#else 
	m.mainLoopDebug();
	m.printStatus();
	#endif
	free(game);
	return EXIT_SUCCESS;
}
