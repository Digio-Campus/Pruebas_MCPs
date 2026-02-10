#include "GameManager.h"

GameManager::GameManager() : mode(GameMode::TWO_PLAYERS), 
                             numBoards(1), 
                             selectedBoard(0),
                             selectedRow(1),
                             selectedCol(1) {
    boards.resize(1);
    stats.resize(1);
}

void GameManager::setNumBoards(int num) {
    if (num < 1) num = 1;
    if (num > 12) num = 12;
    
    numBoards = num;
    boards.resize(numBoards);
    stats.resize(numBoards);
    
    // Reiniciar todos los tableros
    for (auto& board : boards) {
        board.reset();
    }
    
    selectedBoard = 0;
    selectedRow = 1;
    selectedCol = 1;
}

void GameManager::setGameMode(GameMode m) {
    mode = m;
}

int GameManager::getNumBoards() const {
    return numBoards;
}

GameMode GameManager::getGameMode() const {
    return mode;
}

const Board& GameManager::getBoard(int index) const {
    if (index < 0 || index >= numBoards) {
        return boards[0];
    }
    return boards[index];
}

const BoardStats& GameManager::getStats(int index) const {
    if (index < 0 || index >= numBoards) {
        return stats[0];
    }
    return stats[index];
}

bool GameManager::makeMove() {
    if (selectedBoard < 0 || selectedBoard >= numBoards) {
        return false;
    }
    
    Board& board = boards[selectedBoard];
    
    // Verificar que el juego aún está activo
    if (board.getState() != GameState::PLAYING) {
        return false;
    }
    
    // Realizar el movimiento
    bool moved = board.makeMove(selectedRow, selectedCol);
    
    if (moved) {
        // Si el movimiento fue exitoso y el juego terminó, actualizar estadísticas
        if (board.getState() != GameState::PLAYING) {
            updateStats(selectedBoard);
        }
        
        // En modo 2 jugadores, después de que el jugador mueva (O), hacer movimiento automático (X)
        if (mode == GameMode::TWO_PLAYERS && board.getState() == GameState::PLAYING) {
            // El jugador controló O, ahora hacer movimiento automático para X
            if (board.getCurrentTurn() == CellState::X) {
                board.makeRandomMove();
                
                // Verificar si el juego terminó después del movimiento automático
                if (board.getState() != GameState::PLAYING) {
                    updateStats(selectedBoard);
                }
            }
        }
    }
    
    return moved;
}

void GameManager::updateAutoBoards() {
    if (mode == GameMode::ZERO_PLAYERS) {
        // En modo 0 jugadores, actualizar todos los tableros automáticamente
        for (int i = 0; i < numBoards; i++) {
            if (boards[i].getState() == GameState::PLAYING) {
                boards[i].makeRandomMove();
                
                if (boards[i].getState() != GameState::PLAYING) {
                    updateStats(i);
                }
            }
        }
    }
}

void GameManager::selectBoard(int index) {
    if (index >= 0 && index < numBoards) {
        selectedBoard = index;
    }
}

void GameManager::selectCell(int row, int col) {
    if (row >= 0 && row < 3) {
        selectedRow = row;
    }
    if (col >= 0 && col < 3) {
        selectedCol = col;
    }
}

int GameManager::getSelectedBoard() const {
    return selectedBoard;
}

int GameManager::getSelectedRow() const {
    return selectedRow;
}

int GameManager::getSelectedCol() const {
    return selectedCol;
}

void GameManager::moveSelection(int dr, int dc) {
    selectedRow += dr;
    selectedCol += dc;
    
    // Mantener dentro de los límites
    if (selectedRow < 0) selectedRow = 0;
    if (selectedRow > 2) selectedRow = 2;
    if (selectedCol < 0) selectedCol = 0;
    if (selectedCol > 2) selectedCol = 2;
}

void GameManager::resetBoard(int index) {
    if (index >= 0 && index < numBoards) {
        boards[index].reset();
    }
}

void GameManager::resetAll() {
    for (auto& board : boards) {
        board.reset();
    }
    for (auto& stat : stats) {
        stat = BoardStats();
    }
}

void GameManager::updateStats(int boardIndex) {
    if (boardIndex < 0 || boardIndex >= numBoards) {
        return;
    }
    
    BoardStats& stat = stats[boardIndex];
    GameState state = boards[boardIndex].getState();
    
    stat.gamesPlayed++;
    
    if (state == GameState::X_WINS) {
        stat.xWins++;
    } else if (state == GameState::O_WINS) {
        stat.oWins++;
    } else if (state == GameState::DRAW) {
        stat.draws++;
    }
}
