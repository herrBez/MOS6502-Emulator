#ifndef __UTILITIES__H__
#define __UTILITIES__H__
#include "../header/MOS6502.hpp"
int parseFriendlyFile(char * filename);
unsigned char * loadGameFromFriendlyFile(int game_length, char * filename);
#endif
