#include "../include/Game.h"
#include "../include/Menu.h"
#include <ncurses.h>
#include <thread>
#include <chrono>
#include <iostream>

using namespace ttt;

Game::Game()
    : boards_{}
    , settings_()
    , ui_()
    , selectedBoard_(0)
    , mode_(settings_.numPlayers())
    , running_(true)
{
}

void Game::initBoards()
{
    boards_.assign(settings_.numBoards(), Board());
    selectedBoard_ = 0;
}

int Game::run()
{
    Menu menu;
    std::vector<std::string> items = {"Jugar", "Ajustes", "Ayuda", "Salir"};
    while (true) {
        int sel = menu.run(items);
        if (sel == -1 || sel == 3) break;
        if (sel == 0) { // Jugar
            initBoards();
            mode_ = settings_.numPlayers();
            mainLoop();
        } else if (sel == 1) {
            // Adjust settings inline (simple)
            int p = settings_.numPlayers();
            int b = settings_.numBoards();
            bool inSettings = true;
            int selField = 0; // 0 players, 1 boards
            while (inSettings) {
                ui_.drawSettings(settings_, selField);
                int ch = getch();
                if (ch == KEY_LEFT) {
                    if (selField == 0) settings_.setNumPlayers(p = std::max(0, p-1));
                    else settings_.setNumBoards(b = std::max(1, b-1));
                } else if (ch == KEY_RIGHT) {
                    if (selField == 0) settings_.setNumPlayers(p = std::min(2, p+1));
                    else settings_.setNumBoards(b = std::min(9, b+1));
                } else if (ch == '\t') selField = (selField + 1) % 2;
                else if (ch == '\n' || ch == KEY_ENTER) inSettings = false;
                else if (ch == 'q') inSettings = false;
            }
        } else if (sel == 2) {
            ui_.drawHelp();
            getch();
        }
    }
    return 0;
}

void Game::mainLoop()
{
    nodelay(stdscr, FALSE);
    running_ = true;
    while (running_) {
        ui_.refreshAll();
        ui_.drawBoards(boards_, selectedBoard_);
        int ch = getch();
        if (ch == KEY_RESIZE) { ui_.refreshAll(); continue; }
        if (ch == KEY_MOUSE) {
            MEVENT m;
            if (getmouse(&m) == OK) handleMouse(m);
            continue;
        }
        handleKey(ch);
        // Mode 0: automatic animation step
        if (settings_.numPlayers() == 0) {
            autoStepAllBoards();
            std::this_thread::sleep_for(std::chrono::milliseconds(120));
        }
    }
}

void Game::handleKey(int ch)
{
    if (ch == '\t') {
        selectedBoard_ = (selectedBoard_ + 1) % (int)boards_.size();
    } else if (ch >= '1' && ch <= '9') {
        int idx = ch - '1';
        if (idx < (int)boards_.size()) selectedBoard_ = idx;
    } else if (ch == 'q' || ch == 'Q') {
        running_ = false;
    } else if (ch == 'r') {
        boards_[selectedBoard_].reset();
    } else if (ch == 'R') {
        for (auto &b : boards_) b.reset();
    } else if (ch == 'h' || ch == 'H') {
        ui_.drawHelp(); getch();
    } else if (ch == KEY_UP || ch == 'w') {
        // not used - placeholder for keyboard cell navigation
    } else if (ch == '\n' || ch == KEY_ENTER) {
        // keyboard 'place' -> place in center if empty for demo
        Board &bd = boards_[selectedBoard_];
        if (bd.result() == Result::Ongoing) {
            // naive: place in first available cell
            auto moves = bd.availableMoves();
            if (!moves.empty()) {
                int idx = moves.front();
                int r = idx / 3, c = idx % 3;
                bd.makeMove(r, c);
                // mode 2: if player controls O and X auto
                if (settings_.numPlayers() == 2) {
                    if (bd.result() == Result::Ongoing && bd.currentTurn() == Cell::X) {
                        bd.makeAutoMove();
                    }
                }
            }
        }
    }
}

void Game::handleMouse(MEVENT& m)
{
    auto res = ui_.mapClickToCell(m.x, m.y, (int)boards_.size());
    if (!res) return;
    int b, r, c; std::tie(b,r,c) = *res;
    if (b < 0 || b >= (int)boards_.size()) return;
    Board &bd = boards_[b];
    if (bd.result() != Result::Ongoing) return;

    // Mode semantics:
    // 0 -> fully automatic (ignore clicks)
    // 1 -> single player controls both X and O
    // 2 -> player controls O, X auto after player's O
    int mode = settings_.numPlayers();
    if (mode == 0) return; // ignore

    if (mode == 1) {
        bd.makeMove(r,c);
        return;
    }
    if (mode == 2) {
        // player controls O only; accept move only if currentTurn == O
        if (bd.currentTurn() == Cell::O) {
            if (bd.makeMove(r,c)) {
                // after player's O, auto X
                if (bd.result() == Result::Ongoing && bd.currentTurn() == Cell::X) bd.makeAutoMove();
            }
        }
        return;
    }
}

void Game::autoStepAllBoards()
{
    for (auto &b : boards_) {
        if (b.result() == Result::Ongoing) b.makeAutoMove();
    }
}
