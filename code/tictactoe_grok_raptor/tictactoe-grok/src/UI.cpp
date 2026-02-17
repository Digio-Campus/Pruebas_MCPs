#include "UI.h"
#include <cmath>
#include <sstream>

namespace ttt {

UI::UI() {}

UI::~UI() {}

void UI::init() {
    initscr();
    cbreak();
    noecho();
    keypad(stdscr, TRUE);
    curs_set(0);
    initColors();
    use_default_colors();
}

void UI::cleanup() {
    endwin();
}

void UI::initColors() {
    start_color();
    init_pair(1, COLOR_RED, -1);     // X
    init_pair(2, COLOR_CYAN, -1);    // O
    init_pair(3, COLOR_YELLOW, -1);  // Selected
    init_pair(4, COLOR_GREEN, -1);   // Menu selected
    init_pair(5, COLOR_BLUE, -1);    // Borders
    init_pair(6, COLOR_MAGENTA, -1); // Help
}

void UI::drawMenu(const std::vector<std::string>& options, int selected) {
    clear();
    auto [h, w] = getTerminalSize();
    int startY = h / 2 - options.size() / 2;
    int startX = w / 2 - 10; // Approximate

    for (size_t i = 0; i < options.size(); ++i) {
        move(startY + i, startX);
        if (static_cast<int>(i) == selected) {
            attron(COLOR_PAIR(4) | A_BOLD);
        }
        printw("%s", options[i].c_str());
        if (static_cast<int>(i) == selected) {
            attroff(COLOR_PAIR(4) | A_BOLD);
        }
    }
    refresh();
}

void UI::drawSettingsMenu(const Settings& settings, int selectedOption) {
    clear();
    auto [h, w] = getTerminalSize();
    int startY = h / 2 - 3;
    int startX = w / 2 - 20;

    std::vector<std::string> labels = {
        "Jugadores: " + std::to_string(settings.getNumPlayers()) + " (usa <- ->)",
        "Tableros: " + std::to_string(settings.getNumBoards()) + " (usa <- ->)",
        "Volver"
    };

    for (size_t i = 0; i < labels.size(); ++i) {
        move(startY + i, startX);
        if (static_cast<int>(i) == selectedOption) {
            attron(COLOR_PAIR(4) | A_BOLD);
        }
        printw("%s", labels[i].c_str());
        if (static_cast<int>(i) == selectedOption) {
            attroff(COLOR_PAIR(4) | A_BOLD);
        }
    }
    refresh();
}

void UI::drawHelp() {
    clear();
    int startY = 2;
    int startX = 2;

    move(startY++, startX);
    printw("AYUDA - TicTacToe");
    move(startY++, startX);
    printw("Controles:");
    move(startY++, startX);
    printw("- Menu: Flechas arriba/abajo para navegar, Enter para seleccionar");
    move(startY++, startX);
    printw("- Ajustes: Flechas izquierda/derecha para cambiar valores");
    move(startY++, startX);
    printw("- Juego: Flechas para mover cursor, Enter para colocar, Tab para cambiar tablero");
    move(startY++, startX);
    printw("- R para reiniciar tablero actual, Shift+R para reiniciar todos");
    move(startY++, startX);
    printw("- Q para salir al menu");
    move(startY++, startX);
    printw("Modos:");
    move(startY++, startX);
    printw("- 0 jugadores: Relleno automatico aleatorio");
    move(startY++, startX);
    printw("- 1 jugador: Control manual de X y O alternando");
    move(startY++, startX);
    printw("- 2 jugadores: Control de O, X automatico");
    move(startY++, startX);
    printw("Presiona cualquier tecla para volver");
    refresh();
}

void UI::drawGame(const std::vector<Board>& boards, int selectedBoard, int cursorRow, int cursorCol) {
    clear();
    if (boards.empty()) return;

    auto [h, w] = getTerminalSize();
    int n = boards.size();
    int cols = static_cast<int>(std::ceil(std::sqrt(n)));
    int rows = (n + cols - 1) / cols; // ceil(n / cols)

    const int boardH = 7; // 3 cells + borders + stats
    const int boardW = 13; // 3 cells + borders

    int totalH = rows * boardH;
    int totalW = cols * boardW;

    int startY = std::max(0, (h - totalH) / 2);
    int startX = std::max(0, (w - totalW) / 2);

    for (int i = 0; i < n; ++i) {
        int r = i / cols;
        int c = i % cols;
        int boardY = startY + r * boardH;
        int boardX = startX + c * boardW;
        drawBoard(boards[i], boardY, boardX, i == selectedBoard, cursorRow, cursorCol);
    }

    // Bottom bar
    move(h - 1, 0);
    printw("Tab: cambiar tablero | R: reiniciar | Shift+R: reiniciar todos | Q: salir");
    refresh();
}

void UI::drawBoard(const Board& board, int startY, int startX, bool isSelected, int cursorRow, int cursorCol) {
    // Draw border
    attron(COLOR_PAIR(5));
    mvaddch(startY, startX, ACS_ULCORNER);
    mvaddch(startY, startX + 12, ACS_URCORNER);
    mvaddch(startY + 4, startX, ACS_LLCORNER);
    mvaddch(startY + 4, startX + 12, ACS_LRCORNER);
    mvhline(startY, startX + 1, ACS_HLINE, 11);
    mvhline(startY + 4, startX + 1, ACS_HLINE, 11);
    mvvline(startX + 1, startY + 1, ACS_VLINE, 3);
    mvvline(startX + 5, startY + 1, ACS_VLINE, 3);
    mvvline(startX + 9, startY + 1, ACS_VLINE, 3);
    attroff(COLOR_PAIR(5));

    // Draw cells
    for (int i = 0; i < 3; ++i) {
        for (int j = 0; j < 3; ++j) {
            int y = startY + 1 + i;
            int x = startX + 2 + j * 4;
            Cell cell = board.getCell(i, j);
            if (isSelected && i == cursorRow && j == cursorCol) {
                attron(A_REVERSE | A_BOLD);
            }
            if (cell == Cell::X) {
                attron(COLOR_PAIR(1));
                mvaddch(y, x, 'X');
                attroff(COLOR_PAIR(1));
            } else if (cell == Cell::O) {
                attron(COLOR_PAIR(2));
                mvaddch(y, x, 'O');
                attroff(COLOR_PAIR(2));
            } else {
                mvaddch(y, x, ' ');
            }
            if (isSelected && i == cursorRow && j == cursorCol) {
                attroff(A_REVERSE | A_BOLD);
            }
        }
    }

    // Draw turn/result
    int infoY = startY + 5;
    move(infoY, startX);
    Result res = board.getResult();
    Cell turn = board.getCurrentTurn();
    if (res == Result::Ongoing) {
        printw("Turno: %c", turn == Cell::X ? 'X' : 'O');
    } else if (res == Result::X_Win) {
        printw("Ganador: X");
    } else if (res == Result::O_Win) {
        printw("Ganador: O");
    } else {
        printw("Empate");
    }

    drawBoardStats(board, infoY + 1, startX);
}

void UI::drawBoardStats(const Board& board, int startY, int startX) {
    const auto& stats = board.getStats();
    move(startY, startX);
    printw("X:%d O:%d D:%d", stats.xWins, stats.oWins, stats.draws);
}

std::pair<int, int> UI::getTerminalSize() const {
    int y, x;
    getmaxyx(stdscr, y, x);
    return {y, x};
}

} // namespace ttt