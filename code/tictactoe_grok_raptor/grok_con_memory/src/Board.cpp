#include "Board.h"
#include <cstdlib>
#include <ctime>
#include <vector>

Board::Board() {
    srand(time(NULL));
    reset();
    xWins = 0;
    oWins = 0;
    draws = 0;
}

void Board::reset() {
    for(int i = 0; i < 3; i++) {
        for(int j = 0; j < 3; j++) {
            grid[i][j] = ' ';
        }
    }
    currentPlayer = 'X';
}

bool Board::makeMove(int row, int col) {
    if(row < 0 || row > 2 || col < 0 || col > 2 || grid[row][col] != ' ') {
        return false;
    }
    grid[row][col] = currentPlayer;
    currentPlayer = (currentPlayer == 'X') ? 'O' : 'X';
    return true;
}

void Board::makeAutoMove() {
    std::vector<std::pair<int, int>> available;
    for(int i = 0; i < 3; i++) {
        for(int j = 0; j < 3; j++) {
            if(grid[i][j] == ' ') {
                available.push_back({i, j});
            }
        }
    }
    if(available.empty()) return;
    auto move = available[rand() % available.size()];
    makeMove(move.first, move.second);
}

char Board::checkWin() {
    // Check rows
    for(int i = 0; i < 3; i++) {
        if(grid[i][0] == grid[i][1] && grid[i][1] == grid[i][2] && grid[i][0] != ' ') {
            return grid[i][0];
        }
    }
    // Check columns
    for(int j = 0; j < 3; j++) {
        if(grid[0][j] == grid[1][j] && grid[1][j] == grid[2][j] && grid[0][j] != ' ') {
            return grid[0][j];
        }
    }
    // Check diagonals
    if(grid[0][0] == grid[1][1] && grid[1][1] == grid[2][2] && grid[0][0] != ' ') {
        return grid[0][0];
    }
    if(grid[0][2] == grid[1][1] && grid[1][1] == grid[2][0] && grid[0][2] != ' ') {
        return grid[0][2];
    }
    return ' ';
}

bool Board::checkDraw() {
    for(int i = 0; i < 3; i++) {
        for(int j = 0; j < 3; j++) {
            if(grid[i][j] == ' ') {
                return false;
            }
        }
    }
    return checkWin() == ' ';
}

char Board::getCell(int row, int col) const {
    return grid[row][col];
}

char Board::getCurrentPlayer() const {
    return currentPlayer;
}

void Board::incrementWin(char player) {
    if(player == 'X') xWins++;
    else if(player == 'O') oWins++;
    else draws++;
}

int Board::getXWins() const { return xWins; }
int Board::getOWins() const { return oWins; }
int Board::getDraws() const { return draws; }