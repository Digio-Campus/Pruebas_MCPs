#include "Board.h"
#include <cstdlib>
#include <ctime>

Board::Board() : currentPlayer('X'), xWins(0), oWins(0), draws(0) {
    reset();
}

void Board::reset() {
    grid = std::vector<std::vector<char>>(3, std::vector<char>(3, ' '));
    currentPlayer = 'X';
}

bool Board::makeMove(int row, int col) {
    if (row < 0 || row > 2 || col < 0 || col > 2 || grid[row][col] != ' ') {
        return false;
    }
    grid[row][col] = currentPlayer;
    currentPlayer = (currentPlayer == 'X') ? 'O' : 'X';
    return true;
}

bool Board::makeAutoMove() {
    std::vector<std::pair<int, int>> emptyCells;
    for (int i = 0; i < 3; ++i) {
        for (int j = 0; j < 3; ++j) {
            if (grid[i][j] == ' ') {
                emptyCells.push_back({i, j});
            }
        }
    }
    if (emptyCells.empty()) {
        return false;
    }
    int randomIndex = rand() % emptyCells.size();
    auto [row, col] = emptyCells[randomIndex];
    return makeMove(row, col);
}

char Board::checkWin() const {
    // Verificar filas
    for (int i = 0; i < 3; ++i) {
        if (grid[i][0] != ' ' && grid[i][0] == grid[i][1] && grid[i][1] == grid[i][2]) {
            return grid[i][0];
        }
    }
    // Verificar columnas
    for (int j = 0; j < 3; ++j) {
        if (grid[0][j] != ' ' && grid[0][j] == grid[1][j] && grid[1][j] == grid[2][j]) {
            return grid[0][j];
        }
    }
    // Verificar diagonales
    if (grid[0][0] != ' ' && grid[0][0] == grid[1][1] && grid[1][1] == grid[2][2]) {
        return grid[0][0];
    }
    if (grid[0][2] != ' ' && grid[0][2] == grid[1][1] && grid[1][1] == grid[2][0]) {
        return grid[0][2];
    }
    return ' ';
}

bool Board::checkDraw() const {
    for (const auto& row : grid) {
        for (char cell : row) {
            if (cell == ' ') {
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

void Board::getStats(int& xWins_, int& oWins_, int& draws_) const {
    xWins_ = xWins;
    oWins_ = oWins;
    draws_ = draws;
}

void Board::incrementWin(char winner) {
    if (winner == 'X') {
        ++xWins;
    } else if (winner == 'O') {
        ++oWins;
    } else {
        ++draws;
    }
}