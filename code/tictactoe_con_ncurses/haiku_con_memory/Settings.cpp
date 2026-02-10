#include "Settings.h"
#include <algorithm>

Settings::Settings() : gameMode(GameMode::TWO_PLAYERS), numBoards(1), boardsPerRow(1) {}

GameMode Settings::getGameMode() const {
    return gameMode;
}

int Settings::getNumBoards() const {
    return numBoards;
}

int Settings::getBoardsPerRow() const {
    return boardsPerRow;
}

void Settings::setGameMode(GameMode mode) {
    gameMode = mode;
}

void Settings::setNumBoards(int num) {
    numBoards = std::max(1, std::min(num, 9));
}

void Settings::calculateLayout(int termWidth, int termHeight) {
    // Each board needs approximately 11 chars width (3x3 + borders + padding)
    // and 7 rows height (3x3 + borders + header)
    int boardWidth = 11;
    int boardHeight = 7;
    
    boardsPerRow = std::max(1, termWidth / boardWidth);
    int maxRows = std::max(1, termHeight / boardHeight);
    
    // Calculate max boards that fit
    int maxBoards = boardsPerRow * maxRows;
    numBoards = std::min(numBoards, std::max(1, maxBoards - 1)); // Leave space for UI
}
