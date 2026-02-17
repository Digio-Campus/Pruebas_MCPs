#include "../include/Game.h"
#include "../include/UI.h"
#include "../include/Menu.h"
#include <ncurses.h>
#include <chrono>
#include <thread>

Game::Game() : selected(0), ui(nullptr) {
    settings = Settings();
    ensureBoards();
}

void Game::ensureBoards() {
    int want = settings.numBoards;
    boards.clear();
    boards.resize(want);
}

bool Game::playerPlace(int boardIdx, int cellIdx) {
    if (boardIdx < 0 || boardIdx >= (int)boards.size()) return false;
    Board &b = const_cast<Board&>(boards[boardIdx]);
    // Depending on mode, allow human moves:
    if (settings.numPlayers == 0) return false; // no human moves
    if (settings.numPlayers == 2) {
        // player controls O only
        if (b.getTurn() != 'O') return false;
        return b.placeAt(cellIdx);
    }
    // mode 1: human controls both X and O
    return b.placeAt(cellIdx);
}

void Game::autoStep() {
    for (auto &b : boards) {
        if (b.checkWin() != Board::EMPTY || b.checkDraw()) continue;
        if (settings.numPlayers == 0) {
            // both auto
            b.makeAutoMove();
        } else if (settings.numPlayers == 2) {
            // X is automatic, O is human
            if (b.getTurn() == 'X') b.makeAutoMove();
        }
    }
}

void Game::resetBoard(int boardIdx) {
    if (boardIdx < 0 || boardIdx >= (int)boards.size()) return;
    boards[boardIdx].reset();
}

void Game::run() {
    UI localUI;
    ui = &localUI;
    ui->init();
    Menu menu;
    while (true) {
        int sel = menu.run();
        if (sel == 0) {
            // Play
            ensureBoards();
            int cx = 1, cy = 1; // cursor in cell coordinates
            selected = 0;
            nodelay(stdscr, FALSE);
            bool inGame = true;
            while (inGame) {
                ui->draw(boards, selected, settings);
                // automatic steps when appropriate
                if (settings.numPlayers == 0) {
                    // make auto moves and animate slightly
                    autoStep();
                    std::this_thread::sleep_for(std::chrono::milliseconds(80));
                }
                int ch = getch();
                if (ch == 'q' || ch == 'Q') { inGame = false; break; }
                else if (ch == '	') { selected = (selected + 1) % (int)boards.size(); }
                else if (ch == 'r' || ch == 'R') { resetBoard(selected); }
                else if (ch == KEY_RESIZE) { ui->clearScreen(); }
                else if (ch == KEY_UP || ch == KEY_DOWN || ch == KEY_LEFT || ch == KEY_RIGHT) {
                    ui->mapKeyToCellMove(ch, cx, cy);
                }
                else if (ch == 10 || ch == KEY_ENTER) {
                    int idx = cx*3 + cy;
                    if (playerPlace(selected, idx)) {
                        // after human move, immediately allow auto X in mode 2
                        if (settings.numPlayers == 2) {
                            autoStep();
                        }
                    }
                }
            }
        }
        else if (sel == 1) {
            // Settings
            int np = settings.numPlayers; int nb = settings.numBoards;
            int res = menu.runSettings(np, nb);
            if (res == 0) { settings.numPlayers = np; settings.numBoards = nb; ensureBoards(); }
        }
        else if (sel == 2) { menu.showHelp(); }
        else break; // quit
    }
    ui->shutdown();
}
