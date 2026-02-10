#include "board.h"
#include <cstdio>
#include <cstdlib>
#include <ctime>

// Constructor: inicializa el tablero vacío
Board::Board() {
    reset();
    xWins = 0;
    oWins = 0;
    draws = 0;
}

// Destructor
Board::~Board() {
    // Nada que limpiar en esta implementación
}

// Resetea el tablero para una nueva partida
void Board::reset() {
    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 3; j++) {
            grid[i][j] = EMPTY;
        }
    }
    currentTurn = PLAYER_X;  // X siempre comienza
    winner = EMPTY;
    isFull = false;
    moveCount = 0;
}

// Realiza un movimiento en el tablero
bool Board::makeMove(int row, int col) {
    if (row < 0 || row > 2 || col < 0 || col > 2) {
        return false;  // Posición inválida
    }
    
    if (grid[row][col] != EMPTY) {
        return false;  // Casilla ya ocupada
    }
    
    grid[row][col] = currentTurn;
    moveCount++;
    
    // Verificar si hay ganador
    if (checkWinner() != EMPTY) {
        winner = currentTurn;
        if (currentTurn == PLAYER_X) {
            incrementXWins();
        } else {
            incrementOWins();
        }
        return true;
    }
    
    // Verificar si el tablero está lleno (empate)
    if (moveCount == 9) {
        isFull = true;
        winner = EMPTY;
        incrementDraws();
        return true;
    }
    
    // Cambiar turno
    currentTurn = (currentTurn == PLAYER_X) ? PLAYER_O : PLAYER_X;
    return true;
}

// Verifica si hay un ganador
CellState Board::checkWinner() {
    // Verificar filas
    for (int i = 0; i < 3; i++) {
        if (grid[i][0] == grid[i][1] && grid[i][1] == grid[i][2] && grid[i][0] != EMPTY) {
            return grid[i][0];
        }
    }
    
    // Verificar columnas
    for (int j = 0; j < 3; j++) {
        if (grid[0][j] == grid[1][j] && grid[1][j] == grid[2][j] && grid[0][j] != EMPTY) {
            return grid[0][j];
        }
    }
    
    // Verificar diagonales
    if (grid[0][0] == grid[1][1] && grid[1][1] == grid[2][2] && grid[0][0] != EMPTY) {
        return grid[0][0];
    }
    
    if (grid[0][2] == grid[1][1] && grid[1][1] == grid[2][0] && grid[0][2] != EMPTY) {
        return grid[0][2];
    }
    
    return EMPTY;  // No hay ganador
}

// Verifica si el juego ha terminado
bool Board::isGameOver() {
    return (winner != EMPTY) || isFull;
}

// Verifica si una casilla está vacía
bool Board::isCellEmpty(int row, int col) const {
    if (row < 0 || row > 2 || col < 0 || col > 2) {
        return false;
    }
    return grid[row][col] == EMPTY;
}

// Getters
CellState Board::getCell(int row, int col) const {
    if (row < 0 || row > 2 || col < 0 || col > 2) {
        return EMPTY;
    }
    return grid[row][col];
}

CellState Board::getCurrentTurn() const {
    return currentTurn;
}

CellState Board::getWinner() const {
    return winner;
}

bool Board::getIsFull() const {
    return isFull;
}

int Board::getMoveCount() const {
    return moveCount;
}

int Board::getXWins() const {
    return xWins;
}

int Board::getOWins() const {
    return oWins;
}

int Board::getDraws() const {
    return draws;
}

// Setters
void Board::setCurrentTurn(CellState turn) {
    if (turn == PLAYER_X || turn == PLAYER_O) {
        currentTurn = turn;
    }
}

void Board::incrementXWins() {
    xWins++;
}

void Board::incrementOWins() {
    oWins++;
}

void Board::incrementDraws() {
    draws++;
}

// Imprime el tablero en consola (para debugging)
void Board::print() const {
    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 3; j++) {
            if (grid[i][j] == EMPTY) {
                printf(".");
            } else if (grid[i][j] == PLAYER_X) {
                printf("X");
            } else {
                printf("O");
            }
            if (j < 2) printf(" ");
        }
        printf("\n");
    }
}

// Obtiene los movimientos disponibles
std::vector<std::pair<int, int>> Board::getAvailableMoves() const {
    std::vector<std::pair<int, int>> moves;
    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 3; j++) {
            if (grid[i][j] == EMPTY) {
                moves.push_back({i, j});
            }
        }
    }
    return moves;
}
