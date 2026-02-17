#include "Settings.h"

Settings::Settings() : numPlayers(2), numBoards(1) {}

void Settings::setNumPlayers(int players) {
    if(players >= 0 && players <= 2) {
        numPlayers = players;
    }
}

void Settings::setNumBoards(int boards) {
    if(boards >= 1 && boards <= 9) {
        numBoards = boards;
    }
}

int Settings::getNumPlayers() const {
    return numPlayers;
}

int Settings::getNumBoards() const {
    return numBoards;
}

bool Settings::isValid() const {
    return numPlayers >= 0 && numPlayers <= 2 && numBoards >= 1 && numBoards <= 9;
}