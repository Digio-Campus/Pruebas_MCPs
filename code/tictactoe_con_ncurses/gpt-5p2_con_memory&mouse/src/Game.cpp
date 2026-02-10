#include "Game.h"

#include "UI.h"

#include <chrono>
#include <thread>

#include <ncurses.h>

Game::Game(const Settings& settings) {
    std::random_device rd;
    m_rng.seed(rd());
    ensureBoards_(settings.numBoards());
}

void Game::ensureBoards_(int n) {
    if (n < 1) n = 1;
    if (n > 9) n = 9;
    m_boards.resize(n);
    m_finishTicks.assign(n, 0);
    if (m_selectedBoard >= n) m_selectedBoard = 0;
}

bool Game::coordToCell_(const Rect& boardRect, int x, int y, int& outRow, int& outCol) {
    Rect inner = boardRect.inset(1, 1);
    if (!inner.contains(x, y)) return false;
    int cw = std::max(1, inner.w / 3);
    int ch = std::max(1, inner.h / 3);

    int relX = x - inner.x;
    int relY = y - inner.y;
    int col = relX / cw;
    int row = relY / ch;
    if (row < 0 || row >= 3 || col < 0 || col >= 3) return false;

    outRow = row;
    outCol = col;
    return true;
}

void Game::tryHumanMove_(int boardIdx, int row, int col, const Settings& settings) {
    if (boardIdx < 0 || boardIdx >= (int)m_boards.size()) return;

    int mode = settings.numPlayers();
    if (mode == 0) return; // auto

    Board& b = m_boards[boardIdx];
    if (b.isFinished()) return;

    if (mode == 1) {
        b.place(row, col);
        return;
    }

    if (mode == 2) {
        // Jugador controla O.
        if (b.nextTurn() == 'O') {
            b.place(row, col);
        }
        return;
    }
}

void Game::tickAuto_(const Settings& settings) {
    int mode = settings.numPlayers();

    if (mode == 0) {
        // Un tick = una jugada por tablero. Si termina, se deja un pequeño tiempo y se resetea.
        for (int i = 0; i < (int)m_boards.size(); ++i) {
            if (m_boards[i].isFinished()) {
                m_finishTicks[i]++;
                if (m_finishTicks[i] >= 10) {
                    m_boards[i].resetRound();
                    m_finishTicks[i] = 0;
                }
            } else {
                m_boards[i].autoMove(m_rng);
            }
        }
        return;
    }

    if (mode == 2) {
        // X automático en cualquier tablero que esté esperando a X.
        for (auto& b : m_boards) {
            if (!b.isFinished() && b.nextTurn() == 'X') {
                b.autoMove(m_rng);
            }
        }
        return;
    }

    // mode == 1: sin automovimientos.
}

void Game::handleMouse_(int mx, int my, const std::vector<Rect>& rects, const Settings& settings) {
    // Selección de tablero por clic.
    for (int i = 0; i < (int)rects.size(); ++i) {
        if (rects[i].contains(mx, my)) {
            m_selectedBoard = i;
            int r, c;
            if (coordToCell_(rects[i], mx, my, r, c)) {
                m_cursorRow = r;
                m_cursorCol = c;
                tryHumanMove_(i, r, c, settings);
            }
            return;
        }
    }
}

void Game::run(UI& ui, const Settings& settings) {
    ensureBoards_(settings.numBoards());

    m_selectedBoard = 0;
    m_cursorRow = 0;
    m_cursorCol = 0;

    // Timeout para permitir animación/auto-jugadas y resize.
    ui.setTimeoutMs(80);

    while (true) {
        ui.updateSize();
        auto rects = ui.computeBoardRects((int)m_boards.size());

        if ((int)rects.size() != (int)m_boards.size()) {
            ui.clear();
            ui.drawTooSmall("No se pudo calcular el layout de tableros.");
            ui.refresh();
            std::this_thread::sleep_for(std::chrono::milliseconds(100));
            continue;
        }

        ui.clear();
        ui.drawGame(m_boards, rects, m_selectedBoard, m_cursorRow, m_cursorCol, settings,
                    "Tab: cambiar tablero | Flechas: mover | Enter/Espacio: colocar | R: reiniciar | Q: salir");
        ui.refresh();

        int ch = ui.getInput();
        if (ch == ERR) {
            tickAuto_(settings);
            continue;
        }

        if (ch == KEY_RESIZE) {
            continue;
        }

        if (ch == 'q' || ch == 'Q') {
            break;
        }

        if (ch == '\t') {
            m_selectedBoard = (m_selectedBoard + 1) % (int)m_boards.size();
        } else if (ch == KEY_UP) {
            m_cursorRow = (m_cursorRow + 2) % 3;
        } else if (ch == KEY_DOWN) {
            m_cursorRow = (m_cursorRow + 1) % 3;
        } else if (ch == KEY_LEFT) {
            m_cursorCol = (m_cursorCol + 2) % 3;
        } else if (ch == KEY_RIGHT) {
            m_cursorCol = (m_cursorCol + 1) % 3;
        } else if (ch == 'r' || ch == 'R') {
            m_boards[m_selectedBoard].resetRound();
            m_finishTicks[m_selectedBoard] = 0;
        } else if (ch == ' ' || ch == '\n' || ch == KEY_ENTER) {
            tryHumanMove_(m_selectedBoard, m_cursorRow, m_cursorCol, settings);
        } else if (ch == KEY_MOUSE) {
            MEVENT ev{};
            if (getmouse(&ev) == OK) {
                if (ev.bstate & (BUTTON1_CLICKED | BUTTON1_PRESSED | BUTTON1_RELEASED | BUTTON1_DOUBLE_CLICKED)) {
                    handleMouse_(ev.x, ev.y, rects, settings);
                }
            }
        }

        tickAuto_(settings);
    }

    ui.setTimeoutMs(-1);
}
