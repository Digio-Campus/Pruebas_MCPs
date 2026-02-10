#include "Board.h"
#include <algorithm>

Board::Board() : currentTurn(CellState::X), gameState(GameState::IN_PROGRESS), turnsPlayed(0) {
    for (auto& row : grid) {
        row.fill(CellState::EMPTY);
    }
}

bool Board::checkWin(CellState player) const {
    // Check rows
    for (int i = 0; i < 3; i++) {
        if (grid[i][0] == player && grid[i][1] == player && grid[i][2] == player) {
            return true;
        }
    }
    
    // Check columns
    for (int j = 0; j < 3; j++) {
        if (grid[0][j] == player && grid[1][j] == player && grid[2][j] == player) {
            return true;
        }
    }
    
    // Check diagonals
    if (grid[0][0] == player && grid[1][1] == player && grid[2][2] == player) {
        return true;
    }
    if (grid[0][2] == player && grid[1][1] == player && grid[2][0] == player) {
        return true;
    }
    
    return false;
}

bool Board::isBoardFull() const {
    for (const auto& row : grid) {
        for (const auto& cell : row) {
            if (cell == CellState::EMPTY) {
                return false;
            }
        }
    }
    return true;
}

void Board::updateGameState() {
    if (checkWin(CellState::X)) {
        gameState = GameState::X_WINS;
    } else if (checkWin(CellState::O)) {
        gameState = GameState::O_WINS;
    } else if (isBoardFull()) {
        gameState = GameState::DRAW;
    }
}

bool Board::makeMove(int row, int col, CellState player) {
    if (row < 0 || row >= 3 || col < 0 || col >= 3) {
        return false;
    }
    
    if (grid[row][col] != CellState::EMPTY) {
        return false;
    }
    
    if (gameState != GameState::IN_PROGRESS) {
        return false;
    }
    
    grid[row][col] = player;
    turnsPlayed++;
    
    // Alternate turn
    currentTurn = (currentTurn == CellState::X) ? CellState::O : CellState::X;
    
    updateGameState();
    return true;
}

bool Board::canMakeMove(int row, int col) const {
    if (row < 0 || row >= 3 || col < 0 || col >= 3) {
        return false;
    }
    return grid[row][col] == CellState::EMPTY && gameState == GameState::IN_PROGRESS;
}

void Board::reset() {
    for (auto& row : grid) {
        row.fill(CellState::EMPTY);
    }
    currentTurn = CellState::X;
    gameState = GameState::IN_PROGRESS;
    turnsPlayed = 0;
}

CellState Board::getCell(int row, int col) const {
    if (row < 0 || row >= 3 || col < 0 || col >= 3) {
        return CellState::EMPTY;
    }
    return grid[row][col];
}

CellState Board::getCurrentTurn() const {
    return currentTurn;
}

GameState Board::getGameState() const {
    return gameState;
}

bool Board::isGameOver() const {
    return gameState != GameState::IN_PROGRESS;
}

int Board::getTurnsPlayed() const {
    return turnsPlayed;
}

std::vector<std::pair<int, int>> Board::getEmptyCells() const {
    std::vector<std::pair<int, int>> empty;
    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 3; j++) {
            if (grid[i][j] == CellState::EMPTY) {
                empty.push_back({i, j});
            }
        }
    }
    return empty;
}
