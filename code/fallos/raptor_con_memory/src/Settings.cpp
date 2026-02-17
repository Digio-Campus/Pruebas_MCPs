#include "../include/Settings.h"

Settings::Settings(): players(2), boards(1) {}
int Settings::numPlayers() const { return players; }
int Settings::numBoards() const { return boards; }
void Settings::setNumPlayers(int p) { if (p>=0 && p<=2) players=p; }
void Settings::setNumBoards(int n) { if (n>=1 && n<=9) boards=n; }
bool Settings::isValid() const { return players>=0 && players<=2 && boards>=1 && boards<=9; }
