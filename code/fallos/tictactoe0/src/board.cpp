#include "../include/board.h"

Board::Board() : winner(' '), moves_count(0) {
    grid.resize(3, std::vector<char>(3, ' '));
}

bool Board::makeMove(int row, int col, char symbol) {
    if (row < 0 || row > 2 || col < 0 || col > 2) return false;
    if (grid[row][col] != ' ') return false;
    if (isGameOver()) return false;
    
    grid[row][col] = symbol;
    moves_count++;
    
    checkWin();
    if (!isGameOver()) {
        checkDraw();
    }
    
    return true;
}

char Board::getCell(int row, int col) const {
    if (row < 0 || row > 2 || col < 0 || col > 2) return ' ';
    return grid[row][col];
}

bool Board::checkWin() {
    // Verificar filas
    for (int i = 0; i < 3; i++) {
        if (grid[i][0] != ' ' && grid[i][0] == grid[i][1] && grid[i][1] == grid[i][2]) {
            winner = grid[i][0];
            return true;
        }
    }
    
    // Verificar columnas
    for (int i = 0; i < 3; i++) {
        if (grid[0][i] != ' ' && grid[0][i] == grid[1][i] && grid[1][i] == grid[2][i]) {
            winner = grid[0][i];
            return true;
        }
    }
    
    // Verificar diagonales
    if (grid[0][0] != ' ' && grid[0][0] == grid[1][1] && grid[1][1] == grid[2][2]) {
        winner = grid[0][0];
        return true;
    }
    if (grid[0][2] != ' ' && grid[0][2] == grid[1][1] && grid[1][1] == grid[2][0]) {
        winner = grid[0][2];
        return true;
    }
    
    return false;
}

bool Board::checkDraw() {
    if (moves_count == 9 && winner == ' ') {
        winner = 'D';
        return true;
    }
    return false;
}

char Board::getWinner() const {
    return winner;
}

bool Board::isGameOver() const {
    return winner != ' ';
}

void Board::reset() {
    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 3; j++) {
            grid[i][j] = ' ';
        }
    }
    winner = ' ';
    moves_count = 0;
}

int Board::getMovesCount() const {
    return moves_count;
}
