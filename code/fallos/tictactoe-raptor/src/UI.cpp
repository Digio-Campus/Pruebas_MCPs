#include "../include/UI.h"
#include <ncurses.h>
#include <string>
#include <cmath>

UI::UI() {}
UI::~UI() { shutdown(); }

bool UI::init() {
    initscr();
    cbreak();
    noecho();
    keypad(stdscr, TRUE);
    curs_set(0);
    start_color();
    use_default_colors();
    init_pair(1, COLOR_WHITE, -1);
    init_pair(2, COLOR_CYAN, -1);
    init_pair(3, COLOR_GREEN, -1);
    init_pair(4, COLOR_MAGENTA, -1);
    init_pair(5, COLOR_YELLOW, -1);
    init_pair(6, COLOR_RED, -1);
    // enable mouse events (optional)
    mousemask(ALL_MOUSE_EVENTS | REPORT_MOUSE_POSITION, NULL);
    return true;
}

void UI::shutdown() {
    endwin();
}

static void draw_cell(int top, int left, int h, int w, char ch) {
    int cy = top + h/2;
    int cx = left + w/2;
    if (ch != ' ') mvaddch(cy, cx, ch);
}

void UI::computeBoardLayout(int numBoards, int &rows, int &cols) const {
    // map common options
    if (numBoards == 1) { rows = 1; cols = 1; }
    else if (numBoards == 2) { rows = 1; cols = 2; }
    else if (numBoards == 4) { rows = 2; cols = 2; }
    else if (numBoards == 6) { rows = 2; cols = 3; }
    else { rows = 3; cols = 3; }
}

void UI::draw(const std::vector<Board> &boards, int selectedBoard, const Settings &settings) {
    clear();
    int n = (int)boards.size();
    int brow, bcol; computeBoardLayout(n, brow, bcol);
    int boardW = COLS / bcol;
    int boardH = (LINES - 4) / brow; // keep room for stats
    int bi = 0;
    for (int r = 0; r < brow; ++r) {
        for (int c = 0; c < bcol; ++c) {
            if (bi >= n) break;
            int left = c * boardW;
            int top = r * boardH + 1;
            // draw border
            for (int x = left + 1; x < left + boardW - 1; ++x) mvaddch(top, x, '-');
            for (int x = left + 1; x < left + boardW - 1; ++x) mvaddch(top + boardH - 2, x, '-');
            for (int y = top + 1; y < top + boardH - 2; ++y) {
                mvaddch(y, left, '|');
                mvaddch(y, left + boardW - 1, '|');
            }
            if (bi == selectedBoard) attron(A_BOLD);
            // draw 3x3 cells
            int cellW = std::max(3, (boardW - 6) / 3);
            int cellH = std::max(1, (boardH - 6) / 3);
            for (int i = 0; i < 3; ++i) for (int j = 0; j < 3; ++j) {
                int cellTop = top + 1 + i * (cellH + 0);
                int cellLeft = left + 2 + j * (cellW + 1);
                char ch = boards[bi].cellAt(i*3 + j);
                draw_cell(cellTop, cellLeft, cellH, cellW, ch);
            }
            if (bi == selectedBoard) attroff(A_BOLD);

            // draw stats
            mvprintw(top + boardH - 1, left + 2, "#%d X:%d O:%d D:%d", bi+1, boards[bi].xWins(), boards[bi].oWins(), boards[bi].draws());
            ++bi;
        }
    }
    // global status
    mvprintw(LINES-2, 2, "Modo: %d  Tableros: %d  (Tab cambia tablero, Flechas=cursor, Enter=colocar, R=reiniciar, Q=menu)", settings.numPlayers, settings.numBoards);
    refresh();
}

void UI::drawMenu() { clear(); mvprintw(2,2,"TicTacToe - press any key"); refresh(); }
void UI::drawHelp() { clear(); mvprintw(2,2,"Help (TODO)"); refresh(); }

int UI::mapKeyToCellMove(int key, int &cx, int &cy) {
    // return 1 if moved
    switch (key) {
        case KEY_UP: if (cx > 0) { cx--; return 1; } break;
        case KEY_DOWN: if (cx < 2) { cx++; return 1; } break;
        case KEY_LEFT: if (cy > 0) { cy--; return 1; } break;
        case KEY_RIGHT: if (cy < 2) { cy++; return 1; } break;
    }
    return 0;
}

void UI::clearScreen() { clear(); refresh(); }
