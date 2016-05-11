#include "./header/MOS6502.hpp"


int main(int argc, char * argv[]){
	unsigned char * game = (unsigned char*) malloc(sizeof(unsigned char) * 6);
	game[0] = 0x69;
	game[1] = 0xFF;
	game[2] = 0x69;
	game[3] = 0x10;
	game[4] = 0x69;
	game[5] = 0x10;
	
	MOS_6502 m = MOS_6502(0x1000);
	m.loadGameIntoMemory(game, 6);
	#ifndef DEBUG
	m.mainLoop();
	#else 
	m.mainLoopDebug();
	m.printStatus();
	m.printMemory();
	#endif
	free(game);
	return EXIT_SUCCESS;
}
