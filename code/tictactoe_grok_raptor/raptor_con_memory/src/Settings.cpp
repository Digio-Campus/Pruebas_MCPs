#include "../include/Settings.h"

Settings::Settings() : numPlayers(2), numBoards(1) {}

bool Settings::setNumPlayers(int v) {
    if (v < 0 || v > 2) return false;
    numPlayers = v;
    return true;
}

bool Settings::setNumBoards(int v) {
    if (v < 1 || v > 9) return false;
    numBoards = v;
    return true;
}
