#include "../include/Board.h"
#include <cstdlib>
#include <ctime>

Board::Board(int id) : boardId(id) {
    // Inicializar el tablero 3x3 vacío
    grid.resize(3, std::vector<CellState>(3, EMPTY));
    
    // Siempre comenzar con X
    currentTurn = X;
    result = ONGOING;
    movesCount = 0;
    
    // Estadísticas
    xWins = 0;
    oWins = 0;
    draws = 0;
    
    // Semilla aleatoria (solo primera vez)
    static bool seeded = false;
    if (!seeded) {
        srand(time(nullptr));
        seeded = true;
    }
}

bool Board::makeMove(int row, int col) {
    // Validar movimiento
    if (!isValidMove(row, col) || isGameOver()) {
        return false;
    }
    
    // Realizar movimiento
    grid[row][col] = currentTurn;
    movesCount++;
    
    // Verificar victoria
    if (checkWin(currentTurn)) {
        result = (currentTurn == X) ? X_WINS : O_WINS;
        if (currentTurn == X) {
            xWins++;
        } else {
            oWins++;
        }
    }
    // Verificar empate
    else if (checkDraw()) {
        result = DRAW;
        draws++;
    }
    
    // Alternar turno: X → O → X → O
    currentTurn = (currentTurn == X) ? O : X;
    
    return true;
}

bool Board::makeAutoMove() {
    if (isGameOver()) {
        return false;
    }
    
    // Recopilar casillas vacías
    std::vector<std::pair<int, int>> emptyCells;
    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 3; j++) {
            if (grid[i][j] == EMPTY) {
                emptyCells.push_back({i, j});
            }
        }
    }
    
    // Si no hay casillas vacías
    if (emptyCells.empty()) {
        return false;
    }
    
    // Elegir una casilla aleatoria
    int idx = rand() % emptyCells.size();
    return makeMove(emptyCells[idx].first, emptyCells[idx].second);
}

void Board::reset() {
    // Limpiar tablero
    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 3; j++) {
            grid[i][j] = EMPTY;
        }
    }
    
    // Reiniciar estado (siempre comenzar con X)
    currentTurn = X;
    result = ONGOING;
    movesCount = 0;
}

bool Board::checkWin(CellState player) {
    // Verificar filas
    for (int i = 0; i < 3; i++) {
        if (grid[i][0] == player && grid[i][1] == player && grid[i][2] == player) {
            return true;
        }
    }
    
    // Verificar columnas
    for (int j = 0; j < 3; j++) {
        if (grid[0][j] == player && grid[1][j] == player && grid[2][j] == player) {
            return true;
        }
    }
    
    // Verificar diagonales
    if (grid[0][0] == player && grid[1][1] == player && grid[2][2] == player) {
        return true;
    }
    if (grid[0][2] == player && grid[1][1] == player && grid[2][0] == player) {
        return true;
    }
    
    return false;
}

bool Board::checkDraw() {
    // Si hay 9 movimientos y nadie ganó, es empate
    return movesCount >= 9;
}

CellState Board::getCell(int row, int col) const {
    if (row >= 0 && row < 3 && col >= 0 && col < 3) {
        return grid[row][col];
    }
    return EMPTY;
}

CellState Board::getCurrentTurn() const {
    return currentTurn;
}

GameResult Board::getResult() const {
    return result;
}

int Board::getMovesCount() const {
    return movesCount;
}

int Board::getBoardId() const {
    return boardId;
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

bool Board::isValidMove(int row, int col) const {
    return (row >= 0 && row < 3 && col >= 0 && col < 3 && grid[row][col] == EMPTY);
}

bool Board::isGameOver() const {
    return result != ONGOING;
}

std::string Board::getCellSymbol(int row, int col) const {
    CellState cell = getCell(row, col);
    switch (cell) {
        case X: return "X";
        case O: return "O";
        default: return " ";
    }
}

std::string Board::getCurrentTurnSymbol() const {
    return (currentTurn == X) ? "X" : "O";
}
