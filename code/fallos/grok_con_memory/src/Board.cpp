#include "Board.h"
#include <cstdlib>
#include <ctime>

// Constructor
Board::Board() : currentPlayer('X'), winsX(0), winsO(0), draws(0) {
    reset();
}

// Reinicia el tablero
void Board::reset() {
    grid = std::vector<std::vector<char>>(3, std::vector<char>(3, ' '));
    currentPlayer = 'X';
}

// Hace un movimiento si es válido
bool Board::makeMove(int row, int col) {
    if (row < 0 || row > 2 || col < 0 || col > 2 || grid[row][col] != ' ') {
        return false;
    }
    grid[row][col] = currentPlayer;
    switchPlayer();
    return true;
}

// Movimiento automático aleatorio
void Board::autoMove() {
    std::vector<std::pair<int, int>> emptyCells;
    for (int i = 0; i < 3; ++i) {
        for (int j = 0; j < 3; ++j) {
            if (grid[i][j] == ' ') {
                emptyCells.push_back({i, j});
            }
        }
    }
    if (!emptyCells.empty()) {
        int idx = rand() % emptyCells.size();
        makeMove(emptyCells[idx].first, emptyCells[idx].second);
    }
}

// Verifica el ganador
char Board::checkWinner() {
    // Filas, columnas, diagonales
    for (int i = 0; i < 3; ++i) {
        if (grid[i][0] == grid[i][1] && grid[i][1] == grid[i][2] && grid[i][0] != ' ') return grid[i][0];
        if (grid[0][i] == grid[1][i] && grid[1][i] == grid[2][i] && grid[0][i] != ' ') return grid[0][i];
    }
    if (grid[0][0] == grid[1][1] && grid[1][1] == grid[2][2] && grid[0][0] != ' ') return grid[0][0];
    if (grid[0][2] == grid[1][1] && grid[1][1] == grid[2][0] && grid[0][2] != ' ') return grid[0][2];
    // Empate
    bool full = true;
    for (auto& row : grid) for (char c : row) if (c == ' ') full = false;
    if (full) return 'D';
    return ' ';
}

// Obtiene el valor de una celda
char Board::getCell(int row, int col) const {
    return grid[row][col];
}

// Cambia el turno
void Board::switchPlayer() {
    currentPlayer = (currentPlayer == 'X') ? 'O' : 'X';
}