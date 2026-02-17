#include "Game.h"
#include <unistd.h>

Game::Game(UI& u, Settings& s) : ui(u), settings(s), selectedBoard(0), cursorRow(0), cursorCol(0) {
    boards.resize(settings.getNumBoards());
}

void Game::run() {
    int mode = settings.getNumPlayers();
    if(mode == 0) handleMode0();
    else if(mode == 1) handleMode1();
    else handleMode2();
}

void Game::handleMode0() {
    while(true) {
        ui.drawBoards(boards, selectedBoard, cursorRow, cursorCol);
        bool moved = false;
        for(auto& board : boards) {
            if(!board.checkDraw() && board.checkWin() == ' ') {
                board.makeAutoMove();
                char winner = board.checkWin();
                if(winner != ' ') {
                    board.incrementWin(winner);
                } else if(board.checkDraw()) {
                    board.incrementWin(' ');
                }
                moved = true;
            }
        }
        if(!moved) {
            ui.drawStats(boards);
            getch();
            return;
        }
        usleep(500000); // 0.5s delay
    }
}

void Game::handleMode1() {
    while(true) {
        ui.drawBoards(boards, selectedBoard, cursorRow, cursorCol);
        int ch = getch();
        if(ch == 'q' || ch == 'Q') return;
        processInput(ch);
    }
}

void Game::handleMode2() {
    while(true) {
        ui.drawBoards(boards, selectedBoard, cursorRow, cursorCol);
        Board& board = boards[selectedBoard];
        if(board.getCurrentPlayer() == 'X') {
            board.makeAutoMove();
            char winner = board.checkWin();
            if(winner != ' ') board.incrementWin(winner);
            else if(board.checkDraw()) board.incrementWin(' ');
        } else {
            int ch = getch();
            if(ch == 'q' || ch == 'Q') return;
            processInput(ch);
        }
    }
}

void Game::processInput(int ch) {
    if(ch == KEY_UP && cursorRow > 0) cursorRow--;
    else if(ch == KEY_DOWN && cursorRow < 2) cursorRow++;
    else if(ch == KEY_LEFT && cursorCol > 0) cursorCol--;
    else if(ch == KEY_RIGHT && cursorCol < 2) cursorCol++;
    else if(ch == '\t') { // Tab
        selectedBoard = (selectedBoard + 1) % boards.size();
    } else if(ch == 'r' || ch == 'R') {
        boards[selectedBoard].reset();
    } else if(ch == '\n' || ch == KEY_ENTER) {
        Board& board = boards[selectedBoard];
        if(board.makeMove(cursorRow, cursorCol)) {
            char winner = board.checkWin();
            if(winner != ' ') board.incrementWin(winner);
            else if(board.checkDraw()) board.incrementWin(' ');
        }
    } else if(ch == KEY_MOUSE && ui.isMouseEnabled()) {
        MEVENT event;
        if(getmouse(&event) == OK) {
            for(size_t b = 0; b < boards.size(); b++) {
                auto cell = ui.mapClickToCell(event.x, event.y, static_cast<int>(b));
                if(cell.first != -1) {
                    selectedBoard = static_cast<int>(b);
                    cursorRow = cell.first;
                    cursorCol = cell.second;
                    Board& board = boards[selectedBoard];
                    if(board.makeMove(cursorRow, cursorCol)) {
                        char winner = board.checkWin();
                        if(winner != ' ') board.incrementWin(winner);
                        else if(board.checkDraw()) board.incrementWin(' ');
                    }
                    break;
                }
            }
        }
    }
}

void Game::checkGameEnd() {
    // Not used in current implementation
}