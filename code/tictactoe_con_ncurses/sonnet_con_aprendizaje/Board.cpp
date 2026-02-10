#include "Board.h"
#include <cstdlib>
#include <ctime>

Board::Board() : cells(3, std::vector<CellState>(3, CellState::EMPTY)), 
                 state(GameState::PLAYING), 
                 currentTurn(CellState::X), 
                 movesCount(0) {
}

void Board::reset() {
    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 3; j++) {
            cells[i][j] = CellState::EMPTY;
        }
    }
    state = GameState::PLAYING;
    currentTurn = CellState::X;
    movesCount = 0;
}

bool Board::makeMove(int row, int col) {
    // Verificar que la posición es válida y está vacía
    if (row < 0 || row >= 3 || col < 0 || col >= 3) {
        return false;
    }
    
    if (cells[row][col] != CellState::EMPTY) {
        return false;
    }
    
    if (state != GameState::PLAYING) {
        return false;
    }
    
    // Realizar el movimiento
    cells[row][col] = currentTurn;
    movesCount++;
    
    // Verificar ganador
    checkWinner();
    
    // Cambiar turno solo si el juego continúa
    if (state == GameState::PLAYING) {
        switchTurn();
    }
    
    return true;
}

bool Board::makeRandomMove() {
    if (state != GameState::PLAYING || isFull()) {
        return false;
    }
    
    // Obtener todas las casillas vacías
    std::vector<std::pair<int, int>> emptyCells;
    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 3; j++) {
            if (cells[i][j] == CellState::EMPTY) {
                emptyCells.push_back({i, j});
            }
        }
    }
    
    if (emptyCells.empty()) {
        return false;
    }
    
    // Seleccionar una casilla aleatoria
    int index = rand() % emptyCells.size();
    return makeMove(emptyCells[index].first, emptyCells[index].second);
}

CellState Board::getCell(int row, int col) const {
    if (row < 0 || row >= 3 || col < 0 || col >= 3) {
        return CellState::EMPTY;
    }
    return cells[row][col];
}

GameState Board::getState() const {
    return state;
}

CellState Board::getCurrentTurn() const {
    return currentTurn;
}

bool Board::isFull() const {
    return movesCount >= 9;
}

void Board::checkWinner() {
    // Verificar filas
    for (int i = 0; i < 3; i++) {
        if (cells[i][0] != CellState::EMPTY &&
            cells[i][0] == cells[i][1] && 
            cells[i][1] == cells[i][2]) {
            state = (cells[i][0] == CellState::X) ? GameState::X_WINS : GameState::O_WINS;
            return;
        }
    }
    
    // Verificar columnas
    for (int j = 0; j < 3; j++) {
        if (cells[0][j] != CellState::EMPTY &&
            cells[0][j] == cells[1][j] && 
            cells[1][j] == cells[2][j]) {
            state = (cells[0][j] == CellState::X) ? GameState::X_WINS : GameState::O_WINS;
            return;
        }
    }
    
    // Verificar diagonales
    if (cells[0][0] != CellState::EMPTY &&
        cells[0][0] == cells[1][1] && 
        cells[1][1] == cells[2][2]) {
        state = (cells[0][0] == CellState::X) ? GameState::X_WINS : GameState::O_WINS;
        return;
    }
    
    if (cells[0][2] != CellState::EMPTY &&
        cells[0][2] == cells[1][1] && 
        cells[1][1] == cells[2][0]) {
        state = (cells[0][2] == CellState::X) ? GameState::X_WINS : GameState::O_WINS;
        return;
    }
    
    // Verificar empate
    if (isFull()) {
        state = GameState::DRAW;
    }
}

void Board::switchTurn() {
    currentTurn = (currentTurn == CellState::X) ? CellState::O : CellState::X;
}
