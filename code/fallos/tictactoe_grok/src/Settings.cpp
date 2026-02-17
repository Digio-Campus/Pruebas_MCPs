#include "Settings.h"

Settings::Settings() : numPlayers(2), numBoards(1) {}

bool Settings::setNumPlayers(int players) {
    if (players >= 0 && players <= 2) {
        numPlayers = players;
        return true;
    }
    return false;
}

bool Settings::setNumBoards(int boards) {
    if (boards >= 1 && boards <= 9) {
        numBoards = boards;
        return true;
    }
    return false;
}

bool Settings::isValid() const {
    return numPlayers >= 0 && numPlayers <= 2 && numBoards >= 1 && numBoards <= 9;
}