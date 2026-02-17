#include "Board.h"
#include <cstdlib>
#include <ctime>
#include <vector>

Board::Board() : currentPlayer('X'), xWins(0), oWins(0), draws(0) {
    initializeGrid();
    std::srand(std::time(nullptr));
}

void Board::initializeGrid() {
    for (int i = 0; i < 3; ++i) {
        for (int j = 0; j < 3; ++j) {
            grid[i][j] = ' ';
        }
    }
}

bool Board::isValidMove(int row, int col) const {
    return row >= 0 && row < 3 && col >= 0 && col < 3 && grid[row][col] == ' ';
}

bool Board::makeMove(int row, int col) {
    if (!isValidMove(row, col)) {
        return false;
    }

    grid[row][col] = currentPlayer;

    if (checkWin()) {
        if (currentPlayer == 'X') {
            incrementXWins();
        } else {
            incrementOWins();
        }
    } else if (checkDraw()) {
        incrementDraws();
    }

    // Switch player
    currentPlayer = (currentPlayer == 'X') ? 'O' : 'X';

    return true;
}

void Board::makeAutoMove() {
    std::vector<std::pair<int, int>> emptyCells;
    for (int i = 0; i < 3; ++i) {
        for (int j = 0; j < 3; ++j) {
            if (grid[i][j] == ' ') {
                emptyCells.push_back({i, j});
            }
        }
    }

    if (!emptyCells.empty()) {
        int randomIndex = std::rand() % emptyCells.size();
        auto [row, col] = emptyCells[randomIndex];
        makeMove(row, col);
    }
}

void Board::reset() {
    initializeGrid();
    currentPlayer = 'X';
}

char Board::checkWinner() const {
    // Check rows
    for (int i = 0; i < 3; ++i) {
        if (grid[i][0] != ' ' && grid[i][0] == grid[i][1] && grid[i][1] == grid[i][2]) {
            return grid[i][0];
        }
    }

    // Check columns
    for (int j = 0; j < 3; ++j) {
        if (grid[0][j] != ' ' && grid[0][j] == grid[1][j] && grid[1][j] == grid[2][j]) {
            return grid[0][j];
        }
    }

    // Check diagonals
    if (grid[0][0] != ' ' && grid[0][0] == grid[1][1] && grid[1][1] == grid[2][2]) {
        return grid[0][0];
    }
    if (grid[0][2] != ' ' && grid[0][2] == grid[1][1] && grid[1][1] == grid[2][0]) {
        return grid[0][2];
    }

    return ' ';
}

bool Board::checkWin() const {
    return checkWinner() != ' ';
}

bool Board::checkDraw() const {
    if (checkWin()) {
        return false;
    }
    for (int i = 0; i < 3; ++i) {
        for (int j = 0; j < 3; ++j) {
            if (grid[i][j] == ' ') {
                return false;
            }
        }
    }
    return true;
}

char Board::getWinner() const {
    return checkWinner();
}

char Board::getCell(int row, int col) const {
    if (row >= 0 && row < 3 && col >= 0 && col < 3) {
        return grid[row][col];
    }
    return ' ';
}

std::string Board::getBoardString() const {
    std::string str;
    for (int i = 0; i < 3; ++i) {
        for (int j = 0; j < 3; ++j) {
            str += grid[i][j];
            if (j < 2) str += "|";
        }
        if (i < 2) str += "\n-+-+-\n";
    }
    return str;
}