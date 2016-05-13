#include "./header/MOS6502.hpp"


int main(int argc, char * argv[]){
	int length = 9;
	unsigned char * game = (unsigned char*) malloc(sizeof(unsigned char) * length);
	game[0] = 0x38;
	game[1] = 0xF8;
	game[2] = 0x78;
	game[3] = 0x18;
	game[4] = 0x08;
	game[5] = 0xD8;
	game[6] = 0x58;
	game[7] = 0xB8;
	game[8] = 0x08;
	
	
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
