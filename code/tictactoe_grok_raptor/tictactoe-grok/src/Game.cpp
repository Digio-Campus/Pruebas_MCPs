#include "Game.h"
#include <unistd.h> // for usleep or napms

namespace ttt {

Game::Game(UI& ui, Settings& settings)
    : ui_(ui), settings_(settings), selectedBoard_(0), cursorRow_(0), cursorCol_(0) {}

void Game::run() {
    initBoards();
    int mode = settings_.getNumPlayers();
    if (mode == 0) {
        handleMode0();
    } else if (mode == 1) {
        handleMode1();
    } else if (mode == 2) {
        handleMode2();
    }
}

void Game::initBoards() {
    int n = settings_.getNumBoards();
    boards_.resize(n);
    for (auto& board : boards_) {
        board.reset();
    }
}

void Game::handleMode0() {
    nodelay(stdscr, TRUE);
    while (!allBoardsFinished()) {
        for (size_t i = 0; i < boards_.size(); ++i) {
            if (boards_[i].getResult() == Result::Ongoing) {
                boards_[i].makeAutoMove();
            }
        }
        ui_.drawGame(boards_, selectedBoard_, cursorRow_, cursorCol_);
        napms(500);
        int ch = getch();
        if (ch == 'q' || ch == 'Q') break;
    }
    nodelay(stdscr, FALSE);
}

void Game::handleMode1() {
    while (!allBoardsFinished()) {
        ui_.drawGame(boards_, selectedBoard_, cursorRow_, cursorCol_);
        int ch = getch();
        switch (ch) {
            case KEY_UP:
                cursorRow_ = (cursorRow_ - 1 + 3) % 3;
                break;
            case KEY_DOWN:
                cursorRow_ = (cursorRow_ + 1) % 3;
                break;
            case KEY_LEFT:
                cursorCol_ = (cursorCol_ - 1 + 3) % 3;
                break;
            case KEY_RIGHT:
                cursorCol_ = (cursorCol_ + 1) % 3;
                break;
            case '\n':
            case KEY_ENTER:
                playerMove(selectedBoard_, cursorRow_, cursorCol_);
                break;
            case '\t':
                switchBoard(1);
                break;
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
                selectBoard(ch - '0');
                break;
            case 'r':
                resetBoard(selectedBoard_);
                break;
            case 'R':
                resetAllBoards();
                break;
            case 'q':
            case 'Q':
                return;
        }
    }
}

void Game::handleMode2() {
    while (!allBoardsFinished()) {
        ui_.drawGame(boards_, selectedBoard_, cursorRow_, cursorCol_);
        if (boards_[selectedBoard_].getCurrentTurn() == Cell::X &&
            boards_[selectedBoard_].getResult() == Result::Ongoing) {
            boards_[selectedBoard_].makeAutoMove();
            continue;
        }
        int ch = getch();
        switch (ch) {
            case KEY_UP:
                cursorRow_ = (cursorRow_ - 1 + 3) % 3;
                break;
            case KEY_DOWN:
                cursorRow_ = (cursorRow_ + 1) % 3;
                break;
            case KEY_LEFT:
                cursorCol_ = (cursorCol_ - 1 + 3) % 3;
                break;
            case KEY_RIGHT:
                cursorCol_ = (cursorCol_ + 1) % 3;
                break;
            case '\n':
            case KEY_ENTER:
                if (boards_[selectedBoard_].getCurrentTurn() == Cell::O) {
                    playerMove(selectedBoard_, cursorRow_, cursorCol_);
                }
                break;
            case '\t':
                switchBoard(1);
                break;
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
                selectBoard(ch - '0');
                break;
            case 'r':
                resetBoard(selectedBoard_);
                break;
            case 'R':
                resetAllBoards();
                break;
            case 'q':
            case 'Q':
                return;
        }
    }
}

bool Game::playerMove(int boardIdx, int row, int col) {
    return boards_[boardIdx].makeMove(row, col);
}

void Game::resetBoard(int idx) {
    boards_[idx].reset();
}

void Game::resetAllBoards() {
    for (auto& board : boards_) {
        board.reset();
    }
}

void Game::switchBoard(int direction) {
    selectedBoard_ = (selectedBoard_ + direction + static_cast<int>(boards_.size())) % boards_.size();
}

void Game::selectBoard(int num) {
    if (num >= 1 && num <= static_cast<int>(boards_.size())) {
        selectedBoard_ = num - 1;
    }
}

bool Game::allBoardsFinished() const {
    for (const auto& board : boards_) {
        if (board.getResult() == Result::Ongoing) {
            return false;
        }
    }
    return true;
}

} // namespace ttt