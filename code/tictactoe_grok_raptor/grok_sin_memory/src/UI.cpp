#include "UI.h"
#include <ncurses.h>
#include <string>
#include <cmath>

UI::UI() : maxY(0), maxX(0) {}

UI::~UI() {
    cleanup();
}

void UI::init() {
    initscr();
    cbreak();
    noecho();
    keypad(stdscr, TRUE);
    curs_set(0);
    if (has_colors()) {
        start_color();
        init_pair(1, COLOR_WHITE, COLOR_BLACK);   // Normal
        init_pair(2, COLOR_YELLOW, COLOR_BLACK);  // Seleccionado
        init_pair(3, COLOR_GREEN, COLOR_BLACK);   // X
        init_pair(4, COLOR_RED, COLOR_BLACK);     // O
        init_pair(5, COLOR_CYAN, COLOR_BLACK);    // Bordes
    }
    updateDimensions();
}

void UI::cleanup() {
    endwin();
}

void UI::updateDimensions() {
    getmaxyx(stdscr, maxY, maxX);
}

void UI::drawMenu(int selectedItem) {
    clear();
    attron(COLOR_PAIR(1));
    mvprintw(maxY/2 - 4, maxX/2 - 10, "TRES EN RAYA");
    mvprintw(maxY/2 - 2, maxX/2 - 5, "Jugar");
    mvprintw(maxY/2 - 1, maxX/2 - 5, "Ajustes");
    mvprintw(maxY/2, maxX/2 - 5, "Ayuda");
    mvprintw(maxY/2 + 1, maxX/2 - 5, "Salir");

    attron(COLOR_PAIR(2));
    switch (selectedItem) {
        case 0: mvprintw(maxY/2 - 2, maxX/2 - 5, "Jugar"); break;
        case 1: mvprintw(maxY/2 - 1, maxX/2 - 5, "Ajustes"); break;
        case 2: mvprintw(maxY/2, maxX/2 - 5, "Ayuda"); break;
        case 3: mvprintw(maxY/2 + 1, maxX/2 - 5, "Salir"); break;
    }
    attroff(COLOR_PAIR(2));
    mvprintw(maxY - 2, 0, "Usa flechas para navegar, Enter para seleccionar");
    refresh();
}

void UI::drawSettingsMenu(const Settings& settings, int selectedItem) {
    clear();
    mvprintw(1, 1, "AJUSTES");
    mvprintw(3, 1, "Jugadores: %d", settings.getNumPlayers());
    mvprintw(4, 1, "Tableros: %d", settings.getNumBoards());
    mvprintw(6, 1, "Presiona Enter para guardar y volver");
    mvprintw(maxY - 2, 0, "Flechas izquierda/derecha para cambiar valores");
    refresh();
}

void UI::drawHelp() {
    clear();
    mvprintw(1, 1, "AYUDA - TRES EN RAYA");
    mvprintw(3, 1, "MODOS DE JUEGO:");
    mvprintw(4, 1, "0 jugadores: Movimientos automaticos aleatorios");
    mvprintw(5, 1, "1 jugador: Controla X y O manualmente");
    mvprintw(6, 1, "2 jugadores: Controla O, X automatica");
    mvprintw(8, 1, "CONTROLES:");
    mvprintw(9, 1, "Flechas: Mover cursor");
    mvprintw(10, 1, "Enter: Colocar ficha");
    mvprintw(11, 1, "Tab: Cambiar tablero");
    mvprintw(12, 1, "R: Reiniciar tablero");
    mvprintw(13, 1, "ESC: Volver al menu");
    mvprintw(maxY - 2, 0, "Presiona cualquier tecla para volver");
    refresh();
}

void UI::drawBoards(const std::vector<Board>& boards, int selectedBoard, int cursorRow, int cursorCol) {
    clear();
    int cols = std::ceil(std::sqrt(boards.size()));
    int rows = std::ceil(static_cast<double>(boards.size()) / cols);
    int boardWidth = 15;
    int boardHeight = 7;

    for (size_t i = 0; i < boards.size(); ++i) {
        int boardRow = i / cols;
        int boardCol = i % cols;
        int startY = boardRow * boardHeight + 1;
        int startX = boardCol * boardWidth + 1;

        // Dibujar borde
        attron(COLOR_PAIR(i == static_cast<size_t>(selectedBoard) ? 2 : 5));
        for (int y = 0; y < 5; ++y) {
            for (int x = 0; x < 11; ++x) {
                mvaddch(startY + y, startX + x, ' ');
            }
        }

        // Dibujar tablero
        for (int r = 0; r < 3; ++r) {
            for (int c = 0; c < 3; ++c) {
                char cell = boards[i].getCell(r, c);
                int color = 1;
                if (cell == 'X') color = 3;
                else if (cell == 'O') color = 4;
                attron(COLOR_PAIR(color));
                mvaddch(startY + r * 2 + 1, startX + c * 4 + 2, cell == ' ' ? '.' : cell);
                attroff(COLOR_PAIR(color));
            }
        }

        // Dibujar cursor
        if (i == static_cast<size_t>(selectedBoard)) {
            attron(COLOR_PAIR(2));
            mvaddch(startY + cursorRow * 2 + 1, startX + cursorCol * 4 + 2, '[');
            mvaddch(startY + cursorRow * 2 + 1, startX + cursorCol * 4 + 3, ']');
            attroff(COLOR_PAIR(2));
        }

        // Dibujar estadísticas
        drawStats(boards[i], i, startY + 6, startX);
    }
    refresh();
}

void UI::drawStats(const Board& board, int boardIndex, int y, int x) {
    int xWins, oWins, draws;
    board.getStats(xWins, oWins, draws);
    mvprintw(y, x, "Tablero %d: X:%d O:%d E:%d", boardIndex + 1, xWins, oWins, draws);
}

int UI::getInput() {
    return getch();
}

bool UI::hasMouse() {
    return mousemask(ALL_MOUSE_EVENTS | REPORT_MOUSE_POSITION, NULL) != 0;
}

bool UI::mapClickToCell(int mouseY, int mouseX, int& boardIndex, int& cellRow, int& cellCol, const std::vector<Board>& boards) {
    int cols = std::ceil(std::sqrt(boards.size()));
    int boardWidth = 15;
    int boardHeight = 7;

    for (size_t i = 0; i < boards.size(); ++i) {
        int boardRow = i / cols;
        int boardCol = i % cols;
        int startY = boardRow * boardHeight + 1;
        int startX = boardCol * boardWidth + 1;

        if (mouseY >= startY && mouseY < startY + 5 && mouseX >= startX && mouseX < startX + 11) {
            boardIndex = i;
            cellRow = (mouseY - startY - 1) / 2;
            cellCol = (mouseX - startX - 2) / 4;
            if (cellRow >= 0 && cellRow < 3 && cellCol >= 0 && cellCol < 3) {
                return true;
            }
        }
    }
    return false;
}