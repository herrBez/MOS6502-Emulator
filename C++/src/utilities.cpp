#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int parseFriendlyFile(char * filename){
	FILE * fp = fopen(filename, "r");
	if(fp == NULL)
		perror("Could not open file");
	char * buf = (char*)malloc(sizeof(char) * 256);
	char * cleaned_buf = (char *) malloc(sizeof(char) * 256);
	int len;
	int counter = 0;
	int line_number = 1;
	printf("Starting importing file '%s'.. \n", filename);
	while(fgets(buf, 256, fp)) {
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
	char * buf = (char*)malloc(sizeof(char) * 256);
	char * cleaned_buf = (char *) malloc(sizeof(char) * 256);
	int len;
	int line_number = 1;
	printf("Starting importing file '%s'.. \n", filename);
	int j = 0;
	unsigned char * game = (unsigned char*) malloc(sizeof(unsigned char) * game_length);
	while(fgets(buf, 256, fp)) {
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

