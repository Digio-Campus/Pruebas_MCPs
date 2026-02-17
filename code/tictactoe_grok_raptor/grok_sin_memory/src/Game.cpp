#include "Game.h"
#include <unistd.h> // for usleep
#include <ncurses.h>

Game::Game(Settings& settings, UI& ui) : settings(settings), ui(ui), selectedBoard(0), cursorRow(0), cursorCol(0) {}

void Game::initializeBoards() {
    boards.clear();
    for (int i = 0; i < settings.getNumBoards(); ++i) {
        boards.emplace_back();
    }
}

void Game::play() {
    initializeBoards();
    switch (settings.getNumPlayers()) {
        case 0: playMode0(); break;
        case 1: playMode1(); break;
        case 2: playMode2(); break;
    }
}

void Game::playMode0() {
    bool running = true;
    while (running) {
        ui.drawBoards(boards, selectedBoard, cursorRow, cursorCol);
        usleep(500000); // 0.5 segundos

        bool allFull = true;
        for (auto& board : boards) {
            if (!board.makeAutoMove()) {
                char winner = board.checkWin();
                if (winner != ' ') {
                    board.incrementWin(winner);
                } else if (board.checkDraw()) {
                    board.incrementWin(' '); // empate
                } else {
                    allFull = false;
                }
            } else {
                allFull = false;
            }
        }

        if (allFull) {
            // Reiniciar todos los tableros
            for (auto& board : boards) {
                board.reset();
            }
        }
    }
}

void Game::playMode1() {
    while (processInput()) {
        ui.drawBoards(boards, selectedBoard, cursorRow, cursorCol);
    }
}

void Game::playMode2() {
    while (processInput()) {
        ui.drawBoards(boards, selectedBoard, cursorRow, cursorCol);
        // Después de que el jugador coloque O, hacer movimiento automático para X
        if (boards[selectedBoard].getCurrentPlayer() == 'O') {
            boards[selectedBoard].makeAutoMove();
            checkGameEnd(selectedBoard);
        }
    }
}

bool Game::processInput() {
    int ch = ui.getInput();
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
        case '\t': // Tab
            selectedBoard = (selectedBoard + 1) % boards.size();
            break;
        case '\n':
        case KEY_ENTER:
            if (boards[selectedBoard].makeMove(cursorRow, cursorCol)) {
                checkGameEnd(selectedBoard);
            }
            break;
        case 'r':
        case 'R':
            boards[selectedBoard].reset();
            break;
        case 27: // ESC
            return false;
        case KEY_MOUSE:
            if (ui.hasMouse()) {
                MEVENT event;
                if (getmouse(&event) == OK) {
                    handleMouseClick(event.y, event.x);
                }
            }
            break;
    }
    return true;
}

void Game::handleMouseClick(int mouseY, int mouseX) {
    int boardIndex, cellRow, cellCol;
    if (ui.mapClickToCell(mouseY, mouseX, boardIndex, cellRow, cellCol, boards)) {
        selectedBoard = boardIndex;
        cursorRow = cellRow;
        cursorCol = cellCol;
        if (boards[selectedBoard].makeMove(cursorRow, cursorCol)) {
            checkGameEnd(selectedBoard);
        }
    }
}

void Game::checkGameEnd(int boardIndex) {
    char winner = boards[boardIndex].checkWin();
    if (winner != ' ') {
        boards[boardIndex].incrementWin(winner);
        // Opcional: mostrar mensaje de victoria
    } else if (boards[boardIndex].checkDraw()) {
        boards[boardIndex].incrementWin(' '); // empate
    }
}