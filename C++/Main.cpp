#include "./header/MOS6502.hpp"
#include "./header/utilities.hpp"
void printGame(unsigned char * game, int game_length){
	for(int i = 0; i < game_length; i++){
		printf("GAME[0x%X] = %X\n", i, game[i]);
	}
}

int main(int argc, char * argv[]){
	unsigned char * game;
	int game_length;
	if(argc > 1) {
		game_length = parseFriendlyFile(argv[1]);
		game = loadGameFromFriendlyFile(game_length, argv[1]);
		printGame(game, game_length);
	}
	else {
		game_length = 16;
		game = (unsigned char*) malloc(sizeof(unsigned char) * game_length);
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
		game[i++] = 0x68;
		game[i++] = 0x68;
		game[i++] = 0X68;
	
	}
	MOS_6502 m = MOS_6502(0x1000);
	m.loadGameIntoMemory(game, game_length);
	#ifndef DEBUG
	m.mainLoop();
	#else 
	m.mainLoopDebug();
	m.printStatus();
	#endif
	free(game);
	return EXIT_SUCCESS;
}
