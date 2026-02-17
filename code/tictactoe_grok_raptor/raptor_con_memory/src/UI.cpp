#include "../include/UI.h"
#include <ncurses.h>
#include <cmath>
#include <sstream>

UI::UI() : rows_(0), cols_(0) {}
UI::~UI() { shutdown(); }

void UI::init() {
    initscr();
    cbreak();
    noecho();
    curs_set(0);
    keypad(stdscr, TRUE);
    start_color();
    init_pair(1, COLOR_WHITE, COLOR_BLUE);   // selected board
    init_pair(2, COLOR_BLACK, COLOR_CYAN);   // menu highlight
    init_pair(3, COLOR_YELLOW, COLOR_BLACK); // marks
    mousemask(ALL_MOUSE_EVENTS | REPORT_MOUSE_POSITION, NULL);
    refreshSize();
}

void UI::shutdown() {
    endwin();
}

void UI::refreshSize() {
    getmaxyx(stdscr, rows_, cols_);
}

int UI::termRows() const { return rows_; }
int UI::termCols() const { return cols_; }

void UI::drawMainMenu(const std::vector<std::string>& options, int highlight) {
    clear();
    std::string title = "TicTacToe - Tres en Raya";
    mvprintw(1, (cols_ - (int)title.size())/2, "%s", title.c_str());
    for (size_t i = 0; i < options.size(); ++i) {
        int y = 4 + (int)i*2;
        int x = (cols_ - 20)/2;
        if ((int)i == highlight) {
            attron(COLOR_PAIR(2));
            mvprintw(y, x, "  %s  ", options[i].c_str());
            attroff(COLOR_PAIR(2));
        } else {
            mvprintw(y, x, "  %s  ", options[i].c_str());
        }
    }
    mvprintw(rows_-2, 2, "Flechas: navegar   ENTER: seleccionar   Q: salir");
    refresh();
}

void UI::drawSettings(const Settings& s, int highlight) {
    clear();
    mvprintw(2, 4, "AJUSTES");
    mvprintw(5, 6, "NumPlayers:  %d   (0=auto,1=1 jugador,2=jugador controla O)", s.numPlayers);
    mvprintw(7, 6, "NumBoards:   %d   (1..9)", s.numBoards);
    mvprintw(rows_-2, 2, "Izq/Dcha: cambiar valor   ENTER: aceptar   Q: volver");
    refresh();
}

void UI::drawHelp() {
    clear();
    mvprintw(2, 4, "AYUDA - Controles y reglas");
    mvprintw(4, 6, "- Flechas / WASD: mover cursor dentro del tablero seleccionado");
    mvprintw(5, 6, "- ENTER: colocar ficha (según modo)");
    mvprintw(6, 6, "- TAB: cambiar tablero");
    mvprintw(7, 6, "- R: reiniciar tablero seleccionado");
    mvprintw(8, 6, "- Q / ESC: volver al menú / salir");
    mvprintw(10, 6, "Modos:");
    mvprintw(11, 8, "0: ejecución automática (todos los tableros se rellenan aleatoriamente)");
    mvprintw(12, 8, "1: un jugador controla X y O manualmente (turno alterno por tablero)");
    mvprintw(13, 8, "2: el jugador controla O; X se genera automáticamente tras cada turno");
    mvprintw(rows_-2, 2, "Pulsa cualquier tecla para volver");
    refresh();
}

static void draw_cell(int top, int left, int h, int w, char ch, bool highlight)
{
    int center_y = top + h/2;
    int center_x = left + w/2;
    if (highlight) attron(A_REVERSE);
    mvaddch(center_y, center_x, ch);
    if (highlight) attroff(A_REVERSE);
}

void UI::drawBoards(const std::vector<Board>& boards, int selectedBoard, int curR, int curC, const Settings& s) {
    clear();
    refreshSize();
    int n = (int)boards.size();
    if (n == 0) { mvprintw(2,2,"No hay tableros"); refresh(); return; }

    int cols = (int)std::ceil(std::sqrt(n));
    int rows = (int)std::ceil((double)n / cols);

    int availW = cols_ - 4;
    int availH = rows_ - 6; // espacio para header/footer
    int bw = std::max(9, availW / cols); // board width
    int bh = std::max(5, availH / rows); // board height

    if (bw < 9 || bh < 5) {
        mvprintw(2,2, "Ventana demasiado pequeña para mostrar %d tableros. Redimensiona la terminal.", n);
        refresh();
        return;
    }

    // título y ayuda rápida
    mvprintw(1, 2, "Modo: %d  |  Tableros: %d  |  TAB: cambiar tablero  R: reinicio  Q: salir", s.numPlayers, s.numBoards);

    for (int i = 0; i < n; ++i) {
        int br = i / cols;
        int bc = i % cols;
        int left = 2 + bc * bw;
        int top  = 3 + br * bh;
        // border
        for (int x = left; x < left + bw-1; ++x) {
            mvaddch(top, x, '-');
            mvaddch(top + bh - 2, x, '-');
        }
        for (int y = top; y < top + bh-1; ++y) {
            mvaddch(y, left, '|');
            mvaddch(y, left + bw - 2, '|');
        }
        // highlight selected board
        if (i == selectedBoard) attron(COLOR_PAIR(1));
        mvprintw(top, left+1, " Tab %d ", i+1);
        if (i == selectedBoard) attroff(COLOR_PAIR(1));

        // draw 3x3 cells inside (use cell width/height)
        int innerW = bw - 3;
        int innerH = bh - 4;
        int cellW = std::max(1, innerW / 3);
        int cellH = std::max(1, innerH / 3);
        for (int r = 0; r < 3; ++r) {
            for (int c = 0; c < 3; ++c) {
                int cellTop = top + 1 + r * cellH;
                int cellLeft = left + 1 + c * cellW;
                char ch = boards[i].get(r,c);
                bool isCursor = (i == selectedBoard && r == curR && c == curC);
                if (ch != ' ') attron(COLOR_PAIR(3));
                draw_cell(cellTop, cellLeft, cellH, cellW, ch == ' ' ? '.' : ch, isCursor);
                if (ch != ' ') attroff(COLOR_PAIR(3));
            }
        }

        // stats
        std::ostringstream ss;
        ss << "X:" << boards[i].xWins << " O:" << boards[i].oWins << " D:" << boards[i].draws;
        mvprintw(top + bh - 1, left + 1, ss.str().c_str());
    }
    refresh();
}

bool UI::mapClickToBoardCell(int mx, int my, int numBoards, int& outBoard, int& outR, int& outC) {
    if (numBoards <= 0) return false;
    int cols = (int)std::ceil(std::sqrt(numBoards));
    int rows = (int)std::ceil((double)numBoards / cols);
    int availW = cols_ - 4;
    int availH = rows_ - 6;
    int bw = std::max(9, availW / cols);
    int bh = std::max(5, availH / rows);
    // comprobar cada tablero
    for (int i = 0; i < numBoards; ++i) {
        int br = i / cols;
        int bc = i % cols;
        int left = 2 + bc * bw;
        int top  = 3 + br * bh;
        int right = left + bw - 2;
        int bottom = top + bh - 2;
        if (mx >= left && mx <= right && my >= top && my <= bottom) {
            // dentro del tablero i
            int innerW = bw - 3;
            int innerH = bh - 4;
            int cellW = std::max(1, innerW / 3);
            int cellH = std::max(1, innerH / 3);
            int relX = mx - (left + 1);
            int relY = my - (top + 1);
            int col = relX / cellW;
            int row = relY / cellH;
            if (row < 0) row = 0; if (row > 2) row = 2;
            if (col < 0) col = 0; if (col > 2) col = 2;
            outBoard = i; outR = row; outC = col; return true;
        }
    }
    return false;
}
