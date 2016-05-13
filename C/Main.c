#include "./header/MOS6502.h"

MOS6502 * m;


int main(int argc, char * argv[]){
	int length = 7;
	unsigned char * game = (unsigned char*) malloc(sizeof(unsigned char) * length);
	game[0] = 0x38;
	game[1] = 0xF8;
	game[2] = 0x78;
	game[3] = 0x18;
	game[4] = 0xD8;
	game[5] = 0x58;
	game[6] = 0xB8;
	
	
	create_MOS_6502(0x1000);
	loadGameIntoMemory(game, length);
	#ifndef DEBUG
	mainLoop();
	#else 
	mainLoopDebug();
	printStatus();
	#endif
	free(game);
	return EXIT_SUCCESS;
}
