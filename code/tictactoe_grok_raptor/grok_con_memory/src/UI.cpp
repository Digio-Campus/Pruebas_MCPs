#include "UI.h"
#include <cmath>
#include <string>

UI::UI() : termHeight(0), termWidth(0), mouseEnabled(false) {}

UI::~UI() {
    cleanup();
}

void UI::init() {
    initscr();
    cbreak();
    noecho();
    keypad(stdscr, TRUE);
    curs_set(0);
    if(has_colors()) {
        start_color();
        initColors();
    }
    mouseEnabled = (mousemask(ALL_MOUSE_EVENTS | REPORT_MOUSE_POSITION, NULL) != 0);
    getTerminalSize();
}

void UI::cleanup() {
    if(mouseEnabled) mousemask(0, NULL);
    endwin();
}

void UI::initColors() {
    init_pair(1, COLOR_WHITE, COLOR_BLACK);   // normal
    init_pair(2, COLOR_YELLOW, COLOR_BLACK);  // selected
    init_pair(3, COLOR_GREEN, COLOR_BLACK);   // X
    init_pair(4, COLOR_RED, COLOR_BLACK);     // O
    init_pair(5, COLOR_CYAN, COLOR_BLACK);    // cursor
    init_pair(6, COLOR_MAGENTA, COLOR_BLACK); // win
    for(int i = 0; i < 6; i++) colorPairs[i] = i + 1;
}

void UI::getTerminalSize() {
    getmaxyx(stdscr, termHeight, termWidth);
}

void UI::calculateLayout(int numBoards) {
    boardLayouts.clear();
    int cols = (numBoards <= 3) ? numBoards : 3;
    int rows = (numBoards + cols - 1) / cols;
    int boardHeight = 6; // 3 cells + borders + number
    int boardWidth = 7;  // 3 cells + borders
    int totalHeight = rows * boardHeight;
    int totalWidth = cols * boardWidth;
    int startY = std::max(0, (termHeight - totalHeight) / 2);
    int startX = std::max(0, (termWidth - totalWidth) / 2);
    for(int i = 0; i < numBoards; i++) {
        int r = i / cols;
        int c = i % cols;
        BoardLayout layout;
        layout.startY = startY + r * boardHeight;
        layout.startX = startX + c * boardWidth;
        layout.height = boardHeight;
        layout.width = boardWidth;
        boardLayouts.push_back(layout);
    }
}

void UI::drawMenu(int selected) {
    clear();
    mvprintw(1, (termWidth - 20) / 2, "TIC TAC TOE");
    std::vector<std::string> options = {"Jugar", "Ajustes", "Ayuda", "Salir"};
    for(size_t i = 0; i < options.size(); i++) {
        int color = (static_cast<int>(i) == selected) ? colorPairs[1] : colorPairs[0];
        attron(COLOR_PAIR(color));
        mvprintw(3 + static_cast<int>(i) * 2, (termWidth - static_cast<int>(options[i].size())) / 2, "%s", options[i].c_str());
        attroff(COLOR_PAIR(color));
    }
    refresh();
}

void UI::drawSettingsMenu(Settings& settings, int selectedOption, int selectedValue) {
    clear();
    mvprintw(1, (termWidth - 20) / 2, "AJUSTES");
    std::vector<std::string> options = {
        "Jugadores: " + std::to_string(settings.getNumPlayers()),
        "Tableros: " + std::to_string(settings.getNumBoards())
    };
    for(size_t i = 0; i < options.size(); i++) {
        int color = (static_cast<int>(i) == selectedOption) ? colorPairs[1] : colorPairs[0];
        if(static_cast<int>(i) == selectedOption && selectedValue != -1) {
            options[i] += " <" + std::to_string(selectedValue) + ">";
        }
        attron(COLOR_PAIR(color));
        mvprintw(3 + static_cast<int>(i) * 2, (termWidth - static_cast<int>(options[i].size())) / 2, "%s", options[i].c_str());
        attroff(COLOR_PAIR(color));
    }
    refresh();
}

void UI::drawBoards(const std::vector<Board>& boards, int selectedBoard, int cursorRow, int cursorCol) {
    clear();
    calculateLayout(boards.size());
    for(size_t b = 0; b < boards.size(); b++) {
        const Board& board = boards[b];
        BoardLayout layout = boardLayouts[b];
        int y = layout.startY;
        int x = layout.startX;
        // Draw border
        mvvline(y, x, ACS_VLINE, layout.height);
        mvvline(y, x + layout.width - 1, ACS_VLINE, layout.height);
        mvhline(y, x, ACS_HLINE, layout.width);
        mvhline(y + layout.height - 1, x, ACS_HLINE, layout.width);
        // Corners
        mvaddch(y, x, ACS_ULCORNER);
        mvaddch(y, x + layout.width - 1, ACS_URCORNER);
        mvaddch(y + layout.height - 1, x, ACS_LLCORNER);
        mvaddch(y + layout.height - 1, x + layout.width - 1, ACS_LRCORNER);
        // Crosses
        mvaddch(y + 2, x + 2, ACS_PLUS);
        mvaddch(y + 2, x + 4, ACS_PLUS);
        mvaddch(y + 1, x + 3, ACS_TTEE);
        mvaddch(y + 3, x + 3, ACS_BTEE);
        // Cells
        for(int r = 0; r < 3; r++) {
            for(int c = 0; c < 3; c++) {
                char cell = board.getCell(r, c);
                int cy = y + 1 + r;
                int cx = x + 1 + c * 2;
                int color = colorPairs[0];
                if(cell == 'X') color = colorPairs[2];
                else if(cell == 'O') color = colorPairs[3];
                if(static_cast<int>(b) == selectedBoard && r == cursorRow && c == cursorCol) color = colorPairs[4];
                attron(COLOR_PAIR(color));
                mvaddch(cy, cx, cell == ' ' ? '.' : cell);
                attroff(COLOR_PAIR(color));
            }
        }
        // Board number
        mvprintw(y + layout.height, x, "Tablero %zu", b + 1);
    }
    refresh();
}

void UI::drawHelp() {
    clear();
    mvprintw(1, (termWidth - 20) / 2, "AYUDA");
    mvprintw(3, 2, "Controles:");
    mvprintw(5, 2, "Flechas: Navegar");
    mvprintw(6, 2, "Enter: Seleccionar");
    mvprintw(7, 2, "Tab: Cambiar tablero");
    mvprintw(8, 2, "R: Reiniciar tablero");
    mvprintw(9, 2, "Q: Salir");
    mvprintw(11, 2, "Ratón: Clic para seleccionar celda");
    mvprintw(13, 2, "Modos:");
    mvprintw(15, 2, "0 jugadores: Automático");
    mvprintw(16, 2, "1 jugador: Manual X y O");
    mvprintw(17, 2, "2 jugadores: Jugador O, X automático");
    refresh();
}

void UI::drawStats(const std::vector<Board>& boards) {
    clear();
    mvprintw(1, (termWidth - 20) / 2, "ESTADÍSTICAS");
    for(size_t i = 0; i < boards.size(); i++) {
        mvprintw(3 + static_cast<int>(i) * 3, 2, "Tablero %zu: X=%d O=%d Empates=%d", i + 1, boards[i].getXWins(), boards[i].getOWins(), boards[i].getDraws());
    }
    refresh();
}

std::pair<int, int> UI::mapClickToCell(int x, int y, int boardIndex) {
    if(boardIndex < 0 || boardIndex >= static_cast<int>(boardLayouts.size())) return {-1, -1};
    BoardLayout layout = boardLayouts[boardIndex];
    int relX = x - layout.startX;
    int relY = y - layout.startY;
    if(relX < 1 || relX > 5 || relY < 1 || relY > 3) return {-1, -1};
    int col = (relX - 1) / 2;
    int row = relY - 1;
    if(col < 0 || col > 2 || row < 0 || row > 2) return {-1, -1};
    return {row, col};
}

bool UI::isMouseEnabled() const {
    return mouseEnabled;
}