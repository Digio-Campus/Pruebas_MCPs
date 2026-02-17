#include "../include/UI.h"
#include <ncurses.h>
#include <cmath>
#include <chrono>
#include <thread>
#include <sstream>

using namespace std::chrono_literals;

namespace ttt {

UI::UI(Game& game) : game_(game), selectedBoard_(0), selRow_(0), selCol_(0), shouldExit_(false) {}

void UI::initCurses() {
    initscr();
    cbreak();
    noecho();
    keypad(stdscr, TRUE);
    curs_set(0);
    mousemask(ALL_MOUSE_EVENTS | REPORT_MOUSE_POSITION, NULL);
    nodelay(stdscr, FALSE);
    start_color();
    use_default_colors();
    init_pair(1, COLOR_WHITE, COLOR_BLUE);   // selección
    init_pair(2, COLOR_RED, -1);             // X
    init_pair(3, COLOR_CYAN, -1);            // O
    init_pair(4, COLOR_BLACK, COLOR_GREEN);  // estado
}

void UI::deinitCurses() {
    endwin();
}

int UI::run() {
    initCurses();
    while (!shouldExit_) {
        showMainMenu();
    }
    deinitCurses();
    return 0;
}

// ---- Menú principal ----
void UI::showMainMenu() {
    const std::vector<std::string> items = {"Jugar", "Ajustes", "Ayuda", "Salir"};
    int sel = 0;
    while (true) {
        clear();
        int midx = COLS / 2;
        mvprintw(2, midx - 9, "TRES EN RAYA - ncurses");
        for (size_t i = 0; i < items.size(); ++i) {
            int y = 6 + (int)i * 2;
            if ((int)i == sel) attron(A_REVERSE);
            mvprintw(y, midx - (int)items[i].size() / 2, items[i].c_str());
            if ((int)i == sel) attroff(A_REVERSE);
        }
        mvprintw(LINES - 2, 2, "Usa flechas / ratón / Enter");
        refresh();

        int ch = getch();
        if (ch == KEY_MOUSE) {
            MEVENT ev;
            if (getmouse(&ev) == OK) {
                if (ev.bstate & BUTTON1_CLICKED) {
                    for (size_t i = 0; i < items.size(); ++i) {
                        int y = 6 + (int)i * 2;
                        if (ev.y == y && ev.x >= COLS / 2 - 10 && ev.x <= COLS / 2 + 10) {
                            sel = (int)i;
                            break;
                        }
                    }
                }
            }
        } else if (ch == KEY_UP) {
            sel = (sel - 1 + (int)items.size()) % (int)items.size();
        } else if (ch == KEY_DOWN) {
            sel = (sel + 1) % (int)items.size();
        } else if (ch == '\n' || ch == KEY_ENTER) {
            if (sel == 0) { runGameLoop(); return; }
            if (sel == 1) { showSettings(); break; }
            if (sel == 2) { showHelp(); break; }
            if (sel == 3) { shouldExit_ = true; return; }
        } else if (ch == 'q' || ch == 'Q') {
            shouldExit_ = true; return;
        } else if (ch == KEY_RESIZE) {
            ; // redraw
        }
    }
}

// ---- Ajustes ----
void UI::showSettings() {
    int players = (int)game_.mode(); // 0,1,2
    int boards = game_.numBoards();
    int sel = 0; // 0 players, 1 boards, 2 back
    while (true) {
        clear();
        mvprintw(2, 4, "AJUSTES");
        mvprintw(5, 6, "Número de jugadores: ");
        std::string ptxt = (players == 0 ? "0 (Auto aleatorio)" : (players == 1 ? "1 (un jugador controla X y O)" : "2 (jug. controla O; X automático)"));
        mvprintw(5, 28, ptxt.c_str());
        mvprintw(7, 6, "Número de tableros simultáneos: %d", boards);

        const char* opts[] = {"Guardar y volver", "Cancelar"};
        for (int i = 0; i < 2; ++i) {
            if (sel == i) attron(A_REVERSE);
            mvprintw(11 + i * 2, 6, opts[i]);
            if (sel == i) attroff(A_REVERSE);
        }

        mvprintw(LINES - 3, 2, "Flechas izquierda/derecha para cambiar valores, Enter para seleccionar");
        refresh();

        int ch = getch();
        if (ch == KEY_LEFT) {
            if (sel == 0) players = std::max(0, players - 1);
            else if (sel == 1) boards = std::max(1, boards - 1);
        } else if (ch == KEY_RIGHT) {
            if (sel == 0) players = std::min(2, players + 1);
            else if (sel == 1) boards = std::min(12, boards + 1);
        } else if (ch == KEY_UP) {
            sel = (sel - 1 + 2) % 2;
        } else if (ch == KEY_DOWN) {
            sel = (sel + 1) % 2;
        } else if (ch == '\n' || ch == KEY_ENTER) {
            if (sel == 0) {
                // guardar
                game_.setMode((Mode)players);
                game_.setNumBoards(boards);
                return;
            } else {
                return; // cancelar
            }
        } else if (ch == 'q') {
            return;
        } else if (ch == KEY_RESIZE) {
            ;
        }
    }
}

// ---- Ayuda ----
void UI::showHelp() {
    clear();
    int y = 2;
    mvprintw(y++, 2, "AYUDA - CONTROLES Y REGLAS");
    y++;
    mvprintw(y++, 4, "Menú principal: usar flechas, ratón y Enter para navegar.");
    mvprintw(y++, 4, "Dentro del juego:");
    mvprintw(y++, 6, "- Flechas: mover el cursor dentro del tablero seleccionado");
    mvprintw(y++, 6, "- Enter / Espacio: colocar marca (según turno)");
    mvprintw(y++, 6, "- Tab / ← → : cambiar tablero seleccionado");
    mvprintw(y++, 6, "- r: reiniciar tablero actual, R: reiniciar todos");
    mvprintw(y++, 6, "- h: mostrar esta ayuda, q: volver al menú");
    y++;
    mvprintw(y++, 4, "Modos de juego (Ajustes):");
    mvprintw(y++, 6, "0 jugadores - todos los tableros se rellenan automáticamente (aleatorio)");
    mvprintw(y++, 6, "1 jugador  - el usuario controla X y O, alternando turnos manualmente");
    mvprintw(y++, 6, "2 jugadores - el usuario controla O; la X se genera automáticamente tras cada turno");
    y++;
    mvprintw(y++, 4, "Cada tablero es independiente: estado, turno, victorias y empates no se comparten.");
    y++;
    mvprintw(LINES - 2, 2, "Pulsa cualquier tecla para volver...");
    refresh();
    getch();
}

// ---- Bucle de juego ----
void UI::runGameLoop() {
    // asegurar cursor y estado inicial
    selectedBoard_ = 0;
    selRow_ = 0; selCol_ = 0;

    // velocidad de autopartidas en modo 0 (ms)
    const int autoDelayMs = 80;

    // modo no-bloqueante para refrescos periódicos
    nodelay(stdscr, TRUE);
    int idleCounter = 0;

    while (true) {
        erase();
        drawBoards();
        mvprintw(LINES - 2, 2, "Tab: cambiar tablero | r: reiniciar tablero | R: reiniciar todos | h: ayuda | q: salir al menú");
        refresh();

        int ch = getch();
        if (ch == ERR) {
            // sin entrada: en modo Auto o PlayerO_AutoX dejamos que el motor avance
            if (game_.mode() == Mode::Auto) {
                bool moved = game_.updateAutoPlayStep(autoDelayMs);
                if (!moved) {
                    // nothing left to do
                    std::this_thread::sleep_for(50ms);
                }
            } else if (game_.mode() == Mode::PlayerO_AutoX) {
                // generar X automáticas si hay tableros pendientes
                bool moved = game_.updateAutoPlayStep(0);
                if (!moved) std::this_thread::sleep_for(30ms);
            } else {
                std::this_thread::sleep_for(10ms);
            }
            continue;
        }

        if (ch == 'q' || ch == 'Q') break; // volver al menú
        if (ch == 'h' || ch == 'H') { showHelp(); continue; }
        if (ch == 'r') { game_.resetBoard(selectedBoard_); continue; }
        if (ch == 'R') { game_.resetAll(); continue; }
        if (ch == '\t') { selectedBoard_ = (selectedBoard_ + 1) % game_.numBoards(); selRow_ = selCol_ = 0; continue; }

        if (ch == KEY_MOUSE) {
            MEVENT ev;
            if (getmouse(&ev) == OK) handleMouse(ev);
            continue;
        }

        if (ch == KEY_RESIZE) { resizeHandler(); continue; }

        // movimiento de cursor dentro del tablero
        if (ch == KEY_UP) { selRow_ = (selRow_ + 2) % 3; }
        else if (ch == KEY_DOWN) { selRow_ = (selRow_ + 1) % 3; }
        else if (ch == KEY_LEFT) { selCol_ = (selCol_ + 2) % 3; }
        else if (ch == KEY_RIGHT) { selCol_ = (selCol_ + 1) % 3; }
        else if (ch == '\n' || ch == ' ' || ch == KEY_ENTER) {
            // intentar jugar en el tablero seleccionado
            const Board &b = game_.getBoard(selectedBoard_);
            if (b.result() == Result::Ongoing) {
                // En modo ManualBoth, jugador coloca X u O según turno.
                // En modo PlayerO_AutoX, el jugador controla O (el sistema generará X automáticamente).
                game_.playerMove(selectedBoard_, selRow_, selCol_);
            }
        }

        // selección rápida por número (1..9)
        if (ch >= '1' && ch <= '9') {
            int idx = ch - '1';
            if (idx < game_.numBoards()) selectedBoard_ = idx;
        }
    }

    // volver a modo bloqueo
    nodelay(stdscr, FALSE);
}

// Dibuja todos los tableros adaptándose al tamaño de terminal
void UI::drawBoards() {
    int n = game_.numBoards();
    if (n <= 0) return;

    // elegir disposición lo más cuadrada posible
    int cols = (int)std::ceil(std::sqrt((double)n));
    int rows = (n + cols - 1) / cols;

    int minW = 18, minH = 9;
    int cellW = COLS / cols;
    int cellH = (LINES - 3) / rows; // dejar líneas inferiores para mensajes

    if (cellW < minW || cellH < minH) {
        mvprintw(2, 2, "La ventana es demasiado pequeña. Aumenta el tamaño del terminal.");
        return;
    }

    int idx = 0;
    for (int r = 0; r < rows; ++r) {
        for (int c = 0; c < cols; ++c) {
            if (idx >= n) break;
            int top = r * cellH + 1;
            int left = c * cellW + 1;
            int h = cellH - 1;
            int w = cellW - 2;
            bool highlighted = (idx == selectedBoard_);
            drawSingleBoard(idx, top, left, h, w, highlighted);
            ++idx;
        }
    }
}

void UI::drawSingleBoard(int idx, int top, int left, int height, int width, bool highlighted) {
    const Board &b = game_.getBoard(idx);
    // bordes simples
    for (int x = left; x < left + width; ++x) {
        mvaddch(top, x, ACS_HLINE);
        mvaddch(top + height - 1, x, ACS_HLINE);
    }
    for (int y = top; y < top + height; ++y) {
        mvaddch(y, left, ACS_VLINE);
        mvaddch(y, left + width - 1, ACS_VLINE);
    }
    mvaddch(top, left, ACS_ULCORNER);
    mvaddch(top, left + width - 1, ACS_URCORNER);
    mvaddch(top + height - 1, left, ACS_LLCORNER);
    mvaddch(top + height - 1, left + width - 1, ACS_LRCORNER);

    // cabecera con id y estado
    std::stringstream hdr;
    hdr << "Tablero " << (idx + 1) << "  ";
    Result res = b.result();
    if (res == Result::Ongoing) {
        hdr << "Turno: " << (b.currentTurn() == Cell::X ? 'X' : 'O');
    } else if (res == Result::X_Win) hdr << "X gana";
    else if (res == Result::O_Win) hdr << "O gana";
    else hdr << "Empate";
    mvprintw(top, left + 2, hdr.str().c_str());

    // estadísticas pequeñas
    const auto &st = game_.stats()[idx];
    mvprintw(top + height - 1, left + 2, "X:%d O:%d D:%d", st.xWins, st.oWins, st.draws);

    // calcular celda gráfica
    int innerW = width - 4; // márgenes
    int innerH = height - 4;
    int cellW = innerW / 3;
    int cellH = innerH / 3;
    int boardTop = top + 2;
    int boardLeft = left + 2;

    // dibujar líneas internas
    for (int i = 1; i <= 2; ++i) {
        int vx = boardLeft + i * cellW;
        for (int y = boardTop; y < boardTop + cellH * 3; ++y) mvaddch(y, vx, ACS_VLINE);
        int hy = boardTop + i * cellH;
        for (int x = boardLeft; x < boardLeft + cellW * 3; ++x) mvaddch(hy, x, ACS_HLINE);
    }

    // dibujar celdas
    for (int r = 0; r < 3; ++r) {
        for (int c = 0; c < 3; ++c) {
            int cy = boardTop + r * cellH + cellH / 2;
            int cx = boardLeft + c * cellW + cellW / 2;
            char ch = ' ';
            auto cell = b.cellAt(r, c);
            if (cell == Cell::X) {
                attron(COLOR_PAIR(2) | A_BOLD);
                mvaddch(cy, cx, 'X');
                attroff(COLOR_PAIR(2) | A_BOLD);
            } else if (cell == Cell::O) {
                attron(COLOR_PAIR(3) | A_BOLD);
                mvaddch(cy, cx, 'O');
                attroff(COLOR_PAIR(3) | A_BOLD);
            } else {
                mvaddch(cy, cx, '.');
            }

            // resaltar la celda seleccionada si este es el tablero activo
            if (highlighted && r == selRow_ && c == selCol_) {
                mvchgat(cy, cx, 1, A_REVERSE | A_BOLD, 1, NULL);
            }
        }
    }
}

// Convierte coordenadas de pantalla a tablero/celda (si corresponde)
bool UI::screenToBoardCell(int y, int x, int& boardIdx, int& row, int& col) {
    int n = game_.numBoards();
    int cols = (int)std::ceil(std::sqrt((double)n));
    int rows = (n + cols - 1) / cols;
    int cellW = COLS / cols;
    int cellH = (LINES - 3) / rows;
    int idx = 0;
    for (int r = 0; r < rows; ++r) {
        for (int c = 0; c < cols; ++c) {
            if (idx >= n) { ++idx; continue; }
            int top = r * cellH + 1;
            int left = c * cellW + 1;
            int h = cellH - 1;
            int w = cellW - 2;
            if (y >= top && y < top + h && x >= left && x < left + w) {
                // dentro del tablero
                int innerW = w - 4;
                int innerH = h - 4;
                int bTop = top + 2;
                int bLeft = left + 2;
                if (y >= bTop && y < bTop + innerH && x >= bLeft && x < bLeft + innerW) {
                    int localY = y - bTop;
                    int localX = x - bLeft;
                    int cellHsize = innerH / 3;
                    int cellWsize = innerW / 3;
                    row = std::min(2, localY / std::max(1, cellHsize));
                    col = std::min(2, localX / std::max(1, cellWsize));
                    boardIdx = idx;
                    return true;
                }
            }
            ++idx;
        }
    }
    return false;
}

void UI::handleMouse(MEVENT& ev) {
    if (ev.bstate & BUTTON1_CLICKED) {
        int b, r, c;
        if (screenToBoardCell(ev.y, ev.x, b, r, c)) {
            selectedBoard_ = b;
            selRow_ = r; selCol_ = c;
            // intentar jugar si corresponde
            const Board &bd = game_.getBoard(selectedBoard_);
            if (bd.result() == Result::Ongoing) {
                game_.playerMove(selectedBoard_, selRow_, selCol_);
            }
        }
    }
}

void UI::performAutoPlayIfNeeded() {
    // helper no usado actualmente; la lógica está integrada en runGameLoop
}

void UI::resizeHandler() {
    // al redimensionar simplemente limpiamos y dejaremos que el siguiente refresh redibuje
    clear();
}

void UI::centerText(int y, const std::string& s) {
    int x = std::max(0, (COLS - (int)s.size()) / 2);
    mvprintw(y, x, "%s", s.c_str());
}

} // namespace ttt
