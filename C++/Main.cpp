#include "./header/MOS6502.hpp"



int parseFriendlyFile(char * filename){
	FILE * fp = fopen(filename, "r");
	if(fp == NULL)
		perror("Could not open file");
	char * buf = (char*)malloc(sizeof(char) * 32);
	char * cleaned_buf = (char *) malloc(sizeof(char) * 32);
	int len;
	int counter = 0;
	int line_number = 1;
	printf("Starting importing file '%s'.. \n", filename);
	while(fgets(buf, 32, fp)) {
		if(buf[0] != ';') {
			sscanf(buf, "%[^\n]", cleaned_buf); //the result is string terminates with the sequence \n\0
			printf("Line %d: read'%s' \n",line_number, cleaned_buf);
			len = strlen(cleaned_buf);
			if(len % 2 == 1) {
				fprintf(stderr, "Line %d: the instruction length must be a multiple of 2\n", line_number);
			} 
			if(len/2 > 3)
				fprintf(stderr, "Line %d: the instruction is too long \n", line_number);
			counter += (len/2);
			
		}
		line_number++;
	}
	printf("You must allocate %d bytes", counter);
	return counter;
 }
 
unsigned char * loadGameFromFriendlyFile(int game_length, char * filename){
	FILE * fp = fopen(filename, "r");
	if(fp == NULL)
		perror("Could not open file");
	char * buf = (char*)malloc(sizeof(char) * 32);
	char * cleaned_buf = (char *) malloc(sizeof(char) * 32);
	int len;
	int line_number = 1;
	printf("Starting importing file '%s'.. \n", filename);
	int j = 0;
	unsigned char * game = (unsigned char*) malloc(sizeof(unsigned char) * game_length);
	while(fgets(buf, 32, fp)) {
		if(buf[0] != ';') {
			sscanf(buf, "%[^\n]", cleaned_buf); //the result is string terminates with the sequence \n\0
			printf("Line %d: read'%s' \n",line_number, cleaned_buf);
			len = strlen(cleaned_buf);
			char toConvert[5];
			toConvert[0] = '0';
			toConvert[1] = 'x';
			toConvert[5] = '\0';	
			for(int i = 0; i < len; i+= 2){
				toConvert[2] = buf[i];
				toConvert[3] = buf[i+1];
				game[j++] = strtol(toConvert, NULL, 0);
			}
			
		}
		line_number++;
	}
	free(buf);
	free(cleaned_buf);
	fclose(fp);
	return game;
}

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
