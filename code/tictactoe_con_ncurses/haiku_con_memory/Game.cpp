#include "Game.h"
#include <cstdlib>
#include <ctime>
#include <algorithm>

Game::Game() : currentBoardIdx(0), currentCellRow(0), currentCellCol(0), 
               running(true), gameActive(false) {
    srand(time(nullptr));
    ui.init();
    input.enableMouse();
}

void Game::run() {
    while (running) {
        mainMenu();
    }
    ui.cleanup();
}

void Game::mainMenu() {
    bool inMenu = true;
    
    while (inMenu && running) {
        ui.drawMainMenu();
        
        int ch = input.getKeypress();
        
        if (ch == '1') {
            initializeGame();
            playGame();
            inMenu = false;
        } else if (ch == '2') {
            settingsMenu();
        } else if (ch == '3') {
            helpMenu();
        } else if (ch == '4' || ch == 'q') {
            running = false;
            inMenu = false;
        } else if (ch == KEY_MOUSE) {
            MouseEvent event;
            if (input.getMouseEvent(event)) {
                // Handle mouse clicks in menu
                if (event.y >= ui.getMaxY() / 2 - 1 && event.y <= ui.getMaxY() / 2 + 2) {
                    int offset = event.y - (ui.getMaxY() / 2 - 1);
                    if (offset == 0) ch = '1';
                    else if (offset == 1) ch = '2';
                    else if (offset == 2) ch = '3';
                    else if (offset == 3) ch = '4';
                }
            }
        }
    }
}

void Game::settingsMenu() {
    bool inSettings = true;
    int mode = static_cast<int>(settings.getGameMode());
    int numBoards = settings.getNumBoards();
    
    while (inSettings) {
        ui.drawSettingsMenu(mode, numBoards);
        
        int ch = input.getKeypress();
        
        if (ch == '1' || ch == '2' || ch == '3') {
            mode = ch - '1';
            settings.setGameMode(static_cast<GameMode>(mode));
        } else if (ch >= '1' && ch <= '9') {
            numBoards = ch - '0';
            settings.setNumBoards(numBoards);
        } else if (ch == 'b') {
            inSettings = false;
        } else if (ch == '\n') {
            inSettings = false;
        }
    }
}

void Game::helpMenu() {
    ui.drawHelpMenu();
    input.getKeypress();
}

void Game::initializeGame() {
    int numBoards = settings.getNumBoards();
    boards.clear();
    
    for (int i = 0; i < numBoards; i++) {
        boards.push_back(Board());
    }
    
    settings.calculateLayout(ui.getMaxX(), ui.getMaxY());
    ui.calculateBoardPositions(boards.size(), settings.getBoardsPerRow());
    
    currentBoardIdx = 0;
    currentCellRow = 0;
    currentCellCol = 0;
    gameActive = true;
    
    // 0 players mode: fill boards automatically
    if (settings.getGameMode() == GameMode::ZERO_PLAYERS) {
        for (int i = 0; i < static_cast<int>(boards.size()); i++) {
            while (!boards[i].isGameOver()) {
                makeRandomMove(i);
            }
        }
    }
}

void Game::playGame() {
    while (gameActive) {
        ui.drawAllBoards(boards, currentBoardIdx);
        
        int ch = input.getKeypress();
        
        if (ch == 'q') {
            gameActive = false;
        } else if (ch == KEY_UP) {
            currentCellRow = std::max(0, currentCellRow - 1);
        } else if (ch == KEY_DOWN) {
            currentCellRow = std::min(2, currentCellRow + 1);
        } else if (ch == KEY_LEFT) {
            currentCellCol = std::max(0, currentCellCol - 1);
        } else if (ch == KEY_RIGHT) {
            currentCellCol = std::min(2, currentCellCol + 1);
        } else if (ch == KEY_PPAGE) {
            currentBoardIdx = std::max(0, currentBoardIdx - 1);
        } else if (ch == KEY_NPAGE) {
            currentBoardIdx = std::min(static_cast<int>(boards.size()) - 1, currentBoardIdx + 1);
        } else if (ch == ' ' || ch == '\n') {
            Board& board = boards[currentBoardIdx];
            
            if (board.canMakeMove(currentCellRow, currentCellCol)) {
                // Determine whose turn it is
                CellState player = board.getCurrentTurn();
                board.makeMove(currentCellRow, currentCellCol, player);
                
                // Handle AI move in 2-player mode
                if (settings.getGameMode() == GameMode::TWO_PLAYERS && !board.isGameOver()) {
                    makeAIMove(currentBoardIdx);
                }
                // Handle auto-move in 1-player mode
                else if (settings.getGameMode() == GameMode::ONE_PLAYER) {
                    // No auto-move; player controls both X and O
                }
            }
        } else if (ch == 'r') {
            // Reset current board
            boards[currentBoardIdx].reset();
        } else if (ch == KEY_MOUSE) {
            MouseEvent event;
            if (input.getMouseEvent(event)) {
                int row, col;
                if (ui.cellClicked(event.x, event.y, currentBoardIdx, row, col)) {
                    Board& board = boards[currentBoardIdx];
                    if (board.canMakeMove(row, col)) {
                        CellState player = board.getCurrentTurn();
                        board.makeMove(row, col, player);
                        
                        if (settings.getGameMode() == GameMode::TWO_PLAYERS && !board.isGameOver()) {
                            makeAIMove(currentBoardIdx);
                        }
                    }
                }
            }
        }
        
        updateGame();
    }
}

void Game::makeAIMove(int boardIdx) {
    if (boardIdx < 0 || boardIdx >= static_cast<int>(boards.size())) {
        return;
    }
    
    Board& board = boards[boardIdx];
    
    if (board.isGameOver()) {
        return;
    }
    
    // Simple AI: pick random empty cell
    auto emptyCells = board.getEmptyCells();
    if (!emptyCells.empty()) {
        int randIdx = rand() % emptyCells.size();
        auto [row, col] = emptyCells[randIdx];
        board.makeMove(row, col, board.getCurrentTurn());
    }
}

void Game::makeRandomMove(int boardIdx) {
    if (boardIdx < 0 || boardIdx >= static_cast<int>(boards.size())) {
        return;
    }
    
    Board& board = boards[boardIdx];
    
    if (board.isGameOver()) {
        return;
    }
    
    auto emptyCells = board.getEmptyCells();
    if (!emptyCells.empty()) {
        int randIdx = rand() % emptyCells.size();
        auto [row, col] = emptyCells[randIdx];
        board.makeMove(row, col, board.getCurrentTurn());
    }
}

void Game::updateGame() {
    // Check if all boards are game over
    bool allOver = true;
    for (const auto& board : boards) {
        if (!board.isGameOver()) {
            allOver = false;
            break;
        }
    }
    
    if (allOver) {
        // Show statistics and ask to replay
        ui.drawAllBoards(boards, currentBoardIdx);
        mvprintw(ui.getMaxY() - 2, 2, "All boards finished! Press 'p' to play again or 'q' to quit");
        refresh();
        
        while (true) {
            int ch = input.getKeypress();
            if (ch == 'p') {
                gameActive = false;
                return;
            } else if (ch == 'q') {
                gameActive = false;
                return;
            }
        }
    }
}

bool Game::isBoardFull(int boardIdx) const {
    if (boardIdx < 0 || boardIdx >= static_cast<int>(boards.size())) {
        return false;
    }
    return boards[boardIdx].isGameOver();
}

void Game::resetAllBoards() {
    for (auto& board : boards) {
        board.reset();
    }
}
