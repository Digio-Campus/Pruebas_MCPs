#include "Game.h"
#include <ncurses.h>
#include <chrono>
#include <thread>

namespace ttt {

Game::Game(UI& ui, Settings& settings) 
    : ui_(ui), settings_(settings), selectedBoardIdx_(0), gameActive_(true) {
    initializeBoards();
}

void Game::play() {
    int mode = settings_.getNumPlayers();
    
    if (mode == 0) {
        handleMode0();
    } else if (mode == 1) {
        handleMode1();
    } else if (mode == 2) {
        handleMode2();
    }
}

void Game::initializeBoards() {
    boards_.clear();
    boards_.resize(settings_.getNumBoards());
    selectedCells_.clear();
    selectedCells_.resize(settings_.getNumBoards(), 0);
    selectedBoardIdx_ = 0;
}

void Game::handleMode0() {
    // 0 players: fully automatic
    gameActive_ = true;
    
    while (gameActive_) {
        // Make moves on all boards
        bool anyMovesMade = false;
        for (auto& board : boards_) {
            if (board.getResult() == Result::Ongoing) {
                board.makeAutoMove();
                anyMovesMade = true;
            }
        }
        
        if (!anyMovesMade) {
            gameActive_ = false;
        }
        
        drawFrame();
        std::this_thread::sleep_for(std::chrono::milliseconds(500));
        
        // Check for quit
        nodelay(stdscr, TRUE);
        int ch = ui_.getInput();
        nodelay(stdscr, FALSE);
        
        if (ch == 'q' || ch == 'Q') {
            gameActive_ = false;
        } else if (ch == 'R') {
            initializeBoards();
        }
    }
}

void Game::handleMode1() {
    // 1 player: manual control (player controls both X and O)
    gameActive_ = true;
    
    while (gameActive_) {
        drawFrame();
        
        if (!handleInput()) {
            gameActive_ = false;
        }
    }
}

void Game::handleMode2() {
    // 2 players: player controls O, AI controls X
    gameActive_ = true;
    
    while (gameActive_) {
        drawFrame();
        
        if (!handleInput()) {
            gameActive_ = false;
            break;
        }
        
        // AI makes a move for each board where it's X's turn
        for (int i = 0; i < static_cast<int>(boards_.size()); ++i) {
            if (boards_[i].getResult() == Result::Ongoing && boards_[i].getCurrentTurn() == Cell::X) {
                boards_[i].makeAutoMove();
            }
        }
    }
}

bool Game::handleInput() {
    int ch = ui_.getInput();
    
    if (ch == 'q' || ch == 'Q') {
        return false;
    } else if (ch == 'h' || ch == 'H') {
        ui_.drawHelp();
        return true;
    } else if (ch == 'r') {
        // Reset current board
        if (selectedBoardIdx_ < static_cast<int>(boards_.size())) {
            boards_[selectedBoardIdx_].reset();
            selectedCells_[selectedBoardIdx_] = 0;
        }
    } else if (ch == 'R') {
        // Reset all boards
        initializeBoards();
    } else if (ch == KEY_UP) {
        selectedCells_[selectedBoardIdx_] = (selectedCells_[selectedBoardIdx_] - 3 + 9) % 9;
    } else if (ch == KEY_DOWN) {
        selectedCells_[selectedBoardIdx_] = (selectedCells_[selectedBoardIdx_] + 3) % 9;
    } else if (ch == KEY_LEFT) {
        if (selectedCells_[selectedBoardIdx_] % 3 != 0) {
            selectedCells_[selectedBoardIdx_]--;
        }
    } else if (ch == KEY_RIGHT) {
        if (selectedCells_[selectedBoardIdx_] % 3 != 2) {
            selectedCells_[selectedBoardIdx_]++;
        }
    } else if (ch == '\t') {
        selectedBoardIdx_ = (selectedBoardIdx_ + 1) % boards_.size();
    } else if (ch >= '1' && ch <= '9') {
        int boardIdx = ch - '1';
        if (boardIdx < static_cast<int>(boards_.size())) {
            selectedBoardIdx_ = boardIdx;
        }
    } else if (ch == '\n' || ch == KEY_ENTER) {
        playerMove(selectedBoardIdx_, selectedCells_[selectedBoardIdx_], boards_[selectedBoardIdx_].getCurrentTurn());
    } else if (ch == KEY_MOUSE) {
        int mouseX = 0, mouseY = 0;
        if (ui_.getMouseClick(mouseX, mouseY)) {
            // Calculate board layouts for hit testing
            std::vector<BoardLayout> layouts(boards_.size());
            updateBoardLayouts(layouts);
            
            int boardIdx = 0, row = 0, col = 0;
            if (ui_.mapClickToCell(mouseX, mouseY, layouts, boardIdx, row, col)) {
                selectedBoardIdx_ = boardIdx;
                selectedCells_[boardIdx] = row * 3 + col;
                playerMove(boardIdx, row * 3 + col, boards_[boardIdx].getCurrentTurn());
            }
        }
    }
    
    return true;
}

bool Game::playerMove(int boardIdx, int cellIdx, Cell player) {
    if (boardIdx < 0 || boardIdx >= static_cast<int>(boards_.size())) {
        return false;
    }
    
    auto [row, col] = cellIndexToCoords(cellIdx);
    return boards_[boardIdx].makeMove(row, col, player);
}

void Game::updateBoardLayouts(std::vector<BoardLayout>& layouts) {
    int numBoards = boards_.size();
    int boardsPerRow = (numBoards > 4) ? 3 : 2;
    
    int boardHeight = 9;
    int boardWidth = 13;
    int startY = 1;
    int startX = 2;
    
    for (int i = 0; i < numBoards; ++i) {
        int row = i / boardsPerRow;
        int col = i % boardsPerRow;
        int boardY = startY + row * (boardHeight + 2);
        int boardX = startX + col * (boardWidth + 3);
        
        layouts[i].startY = boardY;
        layouts[i].startX = boardX;
        layouts[i].height = boardHeight;
        layouts[i].width = boardWidth;
        
        layouts[i].cellPositions.resize(3);
        for (int r = 0; r < 3; ++r) {
            layouts[i].cellPositions[r].resize(3);
            for (int c = 0; c < 3; ++c) {
                int cellY = boardY + 2 + r * 2;
                int cellX = boardX + 1 + c * 4;
                
                layouts[i].cellPositions[r][c].startY = cellY;
                layouts[i].cellPositions[r][c].startX = cellX;
                layouts[i].cellPositions[r][c].endY = cellY;
                layouts[i].cellPositions[r][c].endX = cellX;
            }
        }
    }
}

void Game::drawFrame() {
    std::vector<BoardLayout> layouts(boards_.size());
    updateBoardLayouts(layouts);
    ui_.drawBoards(boards_, selectedCells_, selectedBoardIdx_);
}

std::pair<int, int> Game::cellIndexToCoords(int cellIdx) const {
    return {cellIdx / 3, cellIdx % 3};
}

int Game::coordsToCell(int row, int col) const {
    return row * 3 + col;
}

}  // namespace ttt
