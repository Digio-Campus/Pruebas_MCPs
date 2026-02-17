#include "Game.h"
#include <ncurses.h>
#include <unistd.h>  // for usleep

Game::Game(UI& uiRef, Settings& settingsRef) : ui(uiRef), settings(settingsRef), selectedBoard(0), cursorRow(0), cursorCol(0) {
    initializeBoards();
}

void Game::initializeBoards() {
    boards.clear();
    for (int i = 0; i < settings.getNumBoards(); ++i) {
        boards.emplace_back();
    }
}

void Game::run() {
    int mode = settings.getNumPlayers();
    if (mode == 0) {
        handleMode0();
    } else if (mode == 1) {
        handleMode1();
    } else {
        handleMode2();
    }
}

void Game::handleMode0() {
    while (!allBoardsFinished()) {
        for (size_t i = 0; i < boards.size(); ++i) {
            if (!boards[i].checkWin() && !boards[i].checkDraw()) {
                boards[i].makeAutoMove();
                if (boards[i].checkWin() || boards[i].checkDraw()) {
                    // Reset after win/draw
                    usleep(1000000);  // 1 second
                    resetBoard(i);
                }
            }
        }
        ui.drawBoards(boards, selectedBoard, cursorRow, cursorCol);
        ui.drawStats(boards);
        usleep(500000);  // 0.5 second
    }
}

void Game::handleMode1() {
    while (!allBoardsFinished()) {
        ui.drawBoards(boards, selectedBoard, cursorRow, cursorCol);
        ui.drawStats(boards);
        int ch = ui.getInput();
        handleInput(ch);
    }
}

void Game::handleMode2() {
    while (!allBoardsFinished()) {
        ui.drawBoards(boards, selectedBoard, cursorRow, cursorCol);
        ui.drawStats(boards);
        int ch = ui.getInput();
        handleInput(ch);
        // After player move, if it's X's turn, auto move
        if (boards[selectedBoard].getCurrentPlayer() == 'X' && !boards[selectedBoard].checkWin() && !boards[selectedBoard].checkDraw()) {
            boards[selectedBoard].makeAutoMove();
            if (boards[selectedBoard].checkWin() || boards[selectedBoard].checkDraw()) {
                resetBoard(selectedBoard);
            }
        }
    }
}

void Game::handleInput(int ch) {
    switch (ch) {
        case KEY_UP:
            cursorRow = (cursorRow - 1 + 3) % 3;
            break;
        case KEY_DOWN:
            cursorRow = (cursorRow + 1) % 3;
            break;
        case KEY_LEFT:
            cursorCol = (cursorCol - 1 + 3) % 3;
            break;
        case KEY_RIGHT:
            cursorCol = (cursorCol + 1) % 3;
            break;
        case '\n':
        case KEY_ENTER:
            makeMove(selectedBoard, cursorRow, cursorCol);
            break;
        case '\t':
            switchBoard();
            break;
        case 'r':
        case 'R':
            resetBoard(selectedBoard);
            break;
        case 'q':
        case 'Q':
            // Quit to menu, but since in run, perhaps set flag
            // For now, reset all or something, but to exit, make all finished
            for (auto& board : boards) {
                board.reset();
            }
            break;
    }
}

void Game::makeMove(int boardIndex, int row, int col) {
    if (boards[boardIndex].makeMove(row, col)) {
        if (boards[boardIndex].checkWin() || boards[boardIndex].checkDraw()) {
            resetBoard(boardIndex);
        }
    }
}

void Game::resetBoard(int boardIndex) {
    boards[boardIndex].reset();
}

void Game::switchBoard() {
    selectedBoard = (selectedBoard + 1) % boards.size();
}

bool Game::allBoardsFinished() const {
    // For auto mode, perhaps never, but for manual, when all have many games or something
    // For simplicity, never finish, user quits with q
    return false;
}