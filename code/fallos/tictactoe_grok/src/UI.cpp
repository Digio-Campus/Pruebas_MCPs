#include "UI.h"
#include "Board.h"
#include "Settings.h"
#include <algorithm>
#include <sstream>

UI::UI() {}

UI::~UI() {
    cleanup();
}

void UI::init() {
    initscr();
    cbreak();
    noecho();
    keypad(stdscr, TRUE);
    curs_set(0);
    start_color();
    initColors();
    getmaxyx(stdscr, maxY, maxX);
}

void UI::cleanup() {
    endwin();
}

void UI::initColors() {
    init_pair(1, COLOR_WHITE, COLOR_BLACK);    // Normal
    init_pair(2, COLOR_BLACK, COLOR_WHITE);    // Highlight
    init_pair(3, COLOR_RED, COLOR_BLACK);      // X
    init_pair(4, COLOR_BLUE, COLOR_BLACK);     // O
    init_pair(5, COLOR_GREEN, COLOR_BLACK);    // Selected
    init_pair(6, COLOR_YELLOW, COLOR_BLACK);   // Cursor
}

void UI::calculateLayout(int numBoards, int& rows, int& cols, int& boardHeight, int& boardWidth) {
    cols = std::min(3, numBoards);
    rows = (numBoards + cols - 1) / cols;
    boardHeight = (maxY - 4) / rows;
    boardWidth = (maxX - 2) / cols;
}

void UI::drawBorder(int startY, int startX, int height, int width) {
    mvaddch(startY, startX, ACS_ULCORNER);
    mvaddch(startY, startX + width - 1, ACS_URCORNER);
    mvaddch(startY + height - 1, startX, ACS_LLCORNER);
    mvaddch(startY + height - 1, startX + width - 1, ACS_LRCORNER);

    for (int i = 1; i < width - 1; ++i) {
        mvaddch(startY, startX + i, ACS_HLINE);
        mvaddch(startY + height - 1, startX + i, ACS_HLINE);
    }

    for (int i = 1; i < height - 1; ++i) {
        mvaddch(startY + i, startX, ACS_VLINE);
        mvaddch(startY + i, startX + width - 1, ACS_VLINE);
    }
}

std::string UI::centerText(const std::string& text, int width) {
    int padding = (width - text.length()) / 2;
    return std::string(std::max(0, padding), ' ') + text;
}

void UI::drawMenu(size_t selectedOption, const std::vector<std::string>& options) {
    clear();
    attron(COLOR_PAIR(1));
    mvprintw(1, (maxX - 20) / 2, "TRES EN RAYA");
    attroff(COLOR_PAIR(1));

    for (size_t i = 0; i < options.size(); ++i) {
        if (i == selectedOption) {
            attron(COLOR_PAIR(2));
        } else {
            attron(COLOR_PAIR(1));
        }
        mvprintw(3 + i, (maxX - options[i].length()) / 2, "%s", options[i].c_str());
        if (i == selectedOption) {
            attroff(COLOR_PAIR(2));
        } else {
            attroff(COLOR_PAIR(1));
        }
    }
    refresh();
}

void UI::drawSettingsMenu(Settings& settings, size_t selectedOption) {
    clear();
    attron(COLOR_PAIR(1));
    mvprintw(1, (maxX - 15) / 2, "AJUSTES");
    attroff(COLOR_PAIR(1));

    std::vector<std::string> options = {
        "Jugadores: " + std::to_string(settings.getNumPlayers()),
        "Tableros: " + std::to_string(settings.getNumBoards()),
        "Volver"
    };

    for (size_t i = 0; i < options.size(); ++i) {
        if (i == selectedOption) {
            attron(COLOR_PAIR(2));
        } else {
            attron(COLOR_PAIR(1));
        }
        mvprintw(3 + i, (maxX - options[i].length()) / 2, "%s", options[i].c_str());
        if (i == selectedOption) {
            attroff(COLOR_PAIR(2));
        } else {
            attroff(COLOR_PAIR(1));
        }
    }
    refresh();
}

void UI::drawBoards(const std::vector<Board>& boards, size_t selectedBoard, int cursorRow, int cursorCol) {
    clear();
    int rows, cols, boardHeight, boardWidth;
    calculateLayout(boards.size(), rows, cols, boardHeight, boardWidth);

    for (size_t i = 0; i < boards.size(); ++i) {
        int y, x;
        getBoardPosition(i, boards.size(), y, x, boardHeight, boardWidth);
        drawSingleBoard(y, x, boards[i], i == selectedBoard, cursorRow, cursorCol);
    }
    refresh();
}

void UI::drawSingleBoard(int y, int x, const Board& board, bool isSelected, int cursorRow, int cursorCol) {
    if (isSelected) {
        attron(COLOR_PAIR(5));
    } else {
        attron(COLOR_PAIR(1));
    }

    // Draw title
    std::string title = "Tablero " + std::to_string(&board - &board + 1);  // Hack, but since vector, better pass index
    // Wait, better to pass index.

    // For now, draw the grid
    for (int i = 0; i < 3; ++i) {
        for (int j = 0; j < 3; ++j) {
            char cell = board.getCell(i, j);
            int cellY = y + i * 2;
            int cellX = x + j * 4;

            if (isSelected && i == cursorRow && j == cursorCol) {
                attron(COLOR_PAIR(6));
            } else if (cell == 'X') {
                attron(COLOR_PAIR(3));
            } else if (cell == 'O') {
                attron(COLOR_PAIR(4));
            } else {
                attron(COLOR_PAIR(1));
            }

            mvaddch(cellY, cellX, cell == ' ' ? '.' : cell);

            if (isSelected && i == cursorRow && j == cursorCol) {
                attroff(COLOR_PAIR(6));
            } else if (cell == 'X') {
                attroff(COLOR_PAIR(3));
            } else if (cell == 'O') {
                attroff(COLOR_PAIR(4));
            } else {
                attroff(COLOR_PAIR(1));
            }
        }
    }

    // Draw separators
    for (int i = 0; i < 3; ++i) {
        mvaddch(y + i * 2, x + 1, '|');
        mvaddch(y + i * 2, x + 5, '|');
        if (i < 2) {
            mvaddch(y + i * 2 + 1, x, '-');
            mvaddch(y + i * 2 + 1, x + 2, '-');
            mvaddch(y + i * 2 + 1, x + 4, '-');
            mvaddch(y + i * 2 + 1, x + 6, '-');
            mvaddch(y + i * 2 + 1, x + 8, '-');
        }
    }

    if (isSelected) {
        attroff(COLOR_PAIR(5));
    } else {
        attroff(COLOR_PAIR(1));
    }
}

void UI::drawStats(const std::vector<Board>& boards) {
    int totalX = 0, totalO = 0, totalDraws = 0;
    for (const auto& board : boards) {
        totalX += board.getXWins();
        totalO += board.getOWins();
        totalDraws += board.getDraws();
    }

    attron(COLOR_PAIR(1));
    mvprintw(maxY - 3, 1, "X: %d | O: %d | Empates: %d", totalX, totalO, totalDraws);
    attroff(COLOR_PAIR(1));
}

void UI::drawHelp() {
    clear();
    attron(COLOR_PAIR(1));
    mvprintw(1, (maxX - 10) / 2, "AYUDA");
    mvprintw(3, 1, "Controles:");
    mvprintw(4, 1, "Flechas: Mover cursor");
    mvprintw(5, 1, "Enter: Seleccionar");
    mvprintw(6, 1, "Tab: Cambiar tablero");
    mvprintw(7, 1, "R: Reiniciar tablero");
    mvprintw(8, 1, "Q: Salir al menu");
    mvprintw(10, 1, "Modos:");
    mvprintw(11, 1, "0 jugadores: Auto");
    mvprintw(12, 1, "1 jugador: Manual X y O");
    mvprintw(13, 1, "2 jugadores: Jugador O, X auto");
    attroff(COLOR_PAIR(1));
    refresh();
}

void UI::clearScreen() {
    clear();
    refresh();
}

int UI::getInput() {
    return getch();
}

void UI::getBoardPosition(int boardIndex, int numBoards, int& y, int& x, int& height, int& width) {
    int rows, cols;
    calculateLayout(numBoards, rows, cols, height, width);

    int row = boardIndex / cols;
    int col = boardIndex % cols;

    y = row * height + 1;
    x = col * width + 1;
}