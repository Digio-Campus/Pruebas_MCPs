#include "../include/Game.h"
#include <ncurses.h>
#include <cstdlib>
#include <ctime>
#include <unistd.h>

Game::Game() : selectedBoard_(0), cursorR_(0), cursorC_(0) {
    std::srand((unsigned)std::time(nullptr));
}

void Game::run() {
    ui_.init();
    mainMenu();
    ui_.shutdown();
}

void Game::mainMenu() {
    bool running = true;
    while (running) {
        ui_.drawMainMenu(menu_.options(), menu_.current());
        int ch = getch();
        switch (ch) {
            case KEY_UP: menu_.up(); break;
            case KEY_DOWN: menu_.down(); break;
            case '\n': {
                int sel = menu_.current();
                if (sel == 0) { // Jugar
                    resetBoards();
                    playLoop();
                } else if (sel == 1) { // Ajustes
                    settingsMenu();
                } else if (sel == 2) { // Ayuda
                    helpScreen();
                } else if (sel == 3) { // Salir
                    running = false;
                }
            } break;
            case 'q': case 'Q': running = false; break;
            default: break;
        }
    }
}

void Game::settingsMenu() {
    int highlight = 0;
    bool done = false;
    while (!done) {
        ui_.drawSettings(settings_, highlight);
        int ch = getch();
        switch (ch) {
            case KEY_LEFT:
                if (highlight == 0) settings_.setNumPlayers((settings_.numPlayers+2)%3);
                else if (highlight == 1) settings_.setNumBoards(std::max(1, settings_.numBoards-1));
                break;
            case KEY_RIGHT:
                if (highlight == 0) settings_.setNumPlayers((settings_.numPlayers+1)%3);
                else if (highlight == 1) settings_.setNumBoards(std::min(9, settings_.numBoards+1));
                break;
            case KEY_UP: highlight = std::max(0, highlight-1); break;
            case KEY_DOWN: highlight = std::min(1, highlight+1); break;
            case '\n': done = true; break;
            case 'q': case 'Q': done = true; break;
            default: break;
        }
    }
}

void Game::helpScreen() {
    ui_.drawHelp();
    getch();
}

void Game::resetBoards() {
    boards_.clear();
    boards_.resize(settings_.numBoards);
    for (auto &b : boards_) b.reset();
    selectedBoard_ = 0;
    cursorR_ = cursorC_ = 0;
    // modo 0 -> rellenar automáticamente
    if (settings_.numPlayers == 0) {
        for (auto &b : boards_) {
            while (!b.isFinished()) { b.makeRandomMove(); }
            if (b.winner() == 'X') b.xWins++;
            else if (b.winner() == 'O') b.oWins++;
            else if (b.isDraw()) b.draws++;
        }
    } else if (settings_.numPlayers == 2) {
        // Modo 2: el jugador controla O — asegurar que X juegue automáticamente al inicio
        for (size_t i = 0; i < boards_.size(); ++i) {
            Board &b = boards_[i];
            if (!b.isFinished() && b.turn == 'X') {
                b.makeRandomMove();
                checkAndUpdateStats((int)i);
            }
        }
    }
}

void Game::checkAndUpdateStats(int idx) {
    if (idx < 0 || idx >= (int)boards_.size()) return;
    Board &b = boards_[idx];
    if (b.isFinished()) {
        if (b.winner() == 'X') b.xWins++;
        else if (b.winner() == 'O') b.oWins++;
        else if (b.isDraw()) b.draws++;
    }
}

void Game::playLoop() {
    bool running = true;
    while (running) {
        ui_.drawBoards(boards_, selectedBoard_, cursorR_, cursorC_, settings_);
        int ch = getch();
        if (ch == KEY_RESIZE) { ui_.refreshSize(); continue; }
        if (ch == '	') { selectedBoard_ = (selectedBoard_ + 1) % (int)boards_.size(); cursorR_ = cursorC_ = 0; continue; }
        if (ch == 'r' || ch == 'R') { boards_[selectedBoard_].reset(); continue; }
        if (ch == 'q' || ch == 'Q' || ch == 27) break; // volver al menú

        if (ch == KEY_MOUSE) {
            MEVENT ev;
            if (getmouse(&ev) == OK) {
                int bidx, rr, cc;
                if (ui_.mapClickToBoardCell(ev.x, ev.y, (int)boards_.size(), bidx, rr, cc)) {
                    selectedBoard_ = bidx; cursorR_ = rr; cursorC_ = cc;
                    // procesar click -> colocar ficha según modo
                    Board &B = boards_[selectedBoard_];
                    if (!B.isFinished()) {
                        if (settings_.numPlayers == 1) {
                            if (B.makeMove(rr, cc)) checkAndUpdateStats(selectedBoard_);
                        } else if (settings_.numPlayers == 2) {
                            // jugador controla O
                            if (B.turn == 'O') {
                                if (B.makeMove(rr, cc)) {
                                    checkAndUpdateStats(selectedBoard_);
                                    // si no terminó, X automático
                                    if (!B.isFinished()) { B.makeRandomMove(); checkAndUpdateStats(selectedBoard_); }
                                }
                            }
                        }
                    }
                }
            }
            continue;
        }

        switch (ch) {
            case KEY_UP: cursorR_ = std::max(0, cursorR_-1); break;
            case KEY_DOWN: cursorR_ = std::min(2, cursorR_+1); break;
            case KEY_LEFT: cursorC_ = std::max(0, cursorC_-1); break;
            case KEY_RIGHT: cursorC_ = std::min(2, cursorC_+1); break;
            case '\n': {
                Board &B = boards_[selectedBoard_];
                if (B.isFinished()) break;
                if (settings_.numPlayers == 1) {
                    if (B.makeMove(cursorR_, cursorC_)) checkAndUpdateStats(selectedBoard_);
                } else if (settings_.numPlayers == 2) {
                    // jugador controla O
                    if (B.turn == 'O') {
                        if (B.makeMove(cursorR_, cursorC_)) {
                            checkAndUpdateStats(selectedBoard_);
                            if (!B.isFinished()) { B.makeRandomMove(); checkAndUpdateStats(selectedBoard_); }
                        }
                    }
                } else if (settings_.numPlayers == 0) {
                    // no-op: modo automático ya rellenado
                }
            } break;
            default: break;
        }
    }
}
