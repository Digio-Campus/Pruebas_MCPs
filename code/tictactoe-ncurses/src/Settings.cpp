#include "Settings.h"

namespace ttt {

Settings::Settings() : numPlayers_(2), numBoards_(1) {}

int Settings::getNumPlayers() const {
    return numPlayers_;
}

int Settings::getNumBoards() const {
    return numBoards_;
}

void Settings::setNumPlayers(int num) {
    if (num >= 0 && num <= 2) {
        numPlayers_ = num;
    }
}

void Settings::setNumBoards(int num) {
    if (num >= 1 && num <= 9) {
        numBoards_ = num;
    }
}

bool Settings::isValid() const {
    return numPlayers_ >= 0 && numPlayers_ <= 2 && numBoards_ >= 1 && numBoards_ <= 9;
}

}  // namespace ttt
