#include "UI.h"
#include <ncurses.h>
#include <algorithm>
#include <cstring>

namespace ttt {

// Color pair constants
constexpr int COLOR_PAIR_BORDER = 1;
constexpr int COLOR_PAIR_X = 2;
constexpr int COLOR_PAIR_O = 3;
constexpr int COLOR_PAIR_SELECTED = 4;
constexpr int COLOR_PAIR_MENU = 5;
constexpr int COLOR_PAIR_INFO = 6;

UI::UI() {
    initscr();
    getmaxyx(stdscr, maxY_, maxX_);
    noecho();
    cbreak();
    nodelay(stdscr, FALSE);
    keypad(stdscr, TRUE);
    curs_set(0);
    mousemask(ALL_MOUSE_EVENTS, nullptr);
    initColors();
}

UI::~UI() {
    cleanup();
}

void UI::init() {
    clear();
    refresh();
}

void UI::cleanup() {
    endwin();
}

int UI::getMaxY() const {
    getmaxyx(stdscr, const_cast<int&>(maxY_), const_cast<int&>(maxX_));
    return maxY_;
}

int UI::getMaxX() const {
    getmaxyx(stdscr, const_cast<int&>(maxY_), const_cast<int&>(maxX_));
    return maxX_;
}

bool UI::isTerminalTooSmall() const {
    return getMaxY() < 20 || getMaxX() < 60;
}

void UI::initColors() {
    start_color();
    use_default_colors();
    
    // Define color pairs
    init_pair(COLOR_PAIR_BORDER, COLOR_WHITE, -1);
    init_pair(COLOR_PAIR_X, COLOR_RED, -1);
    init_pair(COLOR_PAIR_O, COLOR_CYAN, -1);
    init_pair(COLOR_PAIR_SELECTED, COLOR_BLACK, COLOR_GREEN);
    init_pair(COLOR_PAIR_MENU, COLOR_YELLOW, -1);
    init_pair(COLOR_PAIR_INFO, COLOR_GREEN, -1);
}

void UI::drawMenu(const std::vector<std::string>& options, int selected) {
    clear();
    
    int centerY = getMaxY() / 2 - options.size() / 2;
    int centerX = getMaxX() / 2;
    
    // Draw title
    attron(COLOR_PAIR(COLOR_PAIR_MENU) | A_BOLD);
    mvprintw(2, centerX - 6, "TIC TAC TOE");
    attroff(COLOR_PAIR(COLOR_PAIR_MENU) | A_BOLD);
    
    // Draw options
    for (size_t i = 0; i < options.size(); ++i) {
        int y = centerY + i * 2;
        int x = centerX - options[i].length() / 2;
        
        if (static_cast<int>(i) == selected) {
            attron(COLOR_PAIR(COLOR_PAIR_SELECTED) | A_BOLD);
        } else {
            attron(COLOR_PAIR(COLOR_PAIR_INFO));
        }
        
        mvprintw(y, x, "%s", options[i].c_str());
        attroff(COLOR_PAIR(COLOR_PAIR_SELECTED) | A_BOLD);
        attroff(COLOR_PAIR(COLOR_PAIR_INFO));
    }
    
    drawControlsBar("[↑↓] Navigate  [Enter] Select");
    refresh();
}

void UI::drawSettingsMenu(int numPlayers, int numBoards, int selectedOption, bool editingValue) {
    clear();
    
    attron(COLOR_PAIR(COLOR_PAIR_MENU) | A_BOLD);
    mvprintw(2, getMaxX() / 2 - 5, "SETTINGS");
    attroff(COLOR_PAIR(COLOR_PAIR_MENU) | A_BOLD);
    
    int startY = 6;
    
    // Number of players option
    attron(selectedOption == 0 ? (COLOR_PAIR(COLOR_PAIR_SELECTED) | A_BOLD) : COLOR_PAIR(COLOR_PAIR_INFO));
    mvprintw(startY, 5, "Number of Players: ");
    if (selectedOption == 0 && editingValue) {
        attron(COLOR_PAIR(COLOR_PAIR_SELECTED) | A_BOLD);
    }
    mvprintw(startY, 25, "%d (0=Auto, 1=Manual, 2=vs AI)", numPlayers);
    attroff(COLOR_PAIR(COLOR_PAIR_SELECTED) | A_BOLD);
    attroff(COLOR_PAIR(COLOR_PAIR_INFO));
    
    // Number of boards option
    attron(selectedOption == 1 ? (COLOR_PAIR(COLOR_PAIR_SELECTED) | A_BOLD) : COLOR_PAIR(COLOR_PAIR_INFO));
    mvprintw(startY + 2, 5, "Number of Boards:   ");
    if (selectedOption == 1 && editingValue) {
        attron(COLOR_PAIR(COLOR_PAIR_SELECTED) | A_BOLD);
    }
    mvprintw(startY + 2, 25, "%d (1-9)", numBoards);
    attroff(COLOR_PAIR(COLOR_PAIR_SELECTED) | A_BOLD);
    attroff(COLOR_PAIR(COLOR_PAIR_INFO));
    
    drawControlsBar("[↑↓] Select  [←→] Adjust  [Enter] Confirm");
    refresh();
}

void UI::drawBoards(const std::vector<Board>& boards, const std::vector<int>& selectedCells, int selectedBoard) {
    clear();
    
    int numBoards = boards.size();
    int boardsPerRow = std::min(3, numBoards);
    
    int boardHeight = 9;
    int boardWidth = 13;
    int startY = 1;
    int startX = 2;
    
    // Draw all boards
    for (int i = 0; i < numBoards; ++i) {
        int row = i / boardsPerRow;
        int col = i % boardsPerRow;
        int boardY = startY + row * (boardHeight + 2);
        int boardX = startX + col * (boardWidth + 3);
        
        bool isSelected = (i == selectedBoard);
        drawBoardFrame(boardY, boardX, i, boards[i]);
        drawBoardCells(boardY + 1, boardX + 1, 2, 4, boards[i], selectedCells[i], isSelected);
        drawBoardStats(boardY + 7, boardX, boards[i]);
    }
    
    std::string controlsInfo = "[Arrow/Tab] Move  [1-9] Select Board  [R] Reset  [Q] Quit  [H] Help";
    drawControlsBar(controlsInfo);
    refresh();
}

void UI::drawBoardFrame(int startY, int startX, int boardIdx, const Board& /* board */) {
    attron(COLOR_PAIR(COLOR_PAIR_BORDER));
    
    // Title
    attron(A_BOLD);
    mvprintw(startY, startX, "Board %d", boardIdx + 1);
    attroff(A_BOLD);
    
    // Draw corners
    mvaddch(startY + 1, startX, ACS_ULCORNER);
    mvaddch(startY + 1, startX + 11, ACS_URCORNER);
    mvaddch(startY + 7, startX, ACS_LLCORNER);
    mvaddch(startY + 7, startX + 11, ACS_LRCORNER);
    
    // Draw horizontal lines
    for (int x = startX + 1; x < startX + 11; ++x) {
        mvaddch(startY + 1, x, ACS_HLINE);
        mvaddch(startY + 7, x, ACS_HLINE);
    }
    
    // Draw vertical lines
    for (int y = startY + 2; y < startY + 7; ++y) {
        mvaddch(y, startX, ACS_VLINE);
        mvaddch(y, startX + 11, ACS_VLINE);
    }
    
    // Draw internal grid
    mvaddch(startY + 3, startX, ACS_LTEE);
    mvaddch(startY + 3, startX + 11, ACS_RTEE);
    mvaddch(startY + 5, startX, ACS_LTEE);
    mvaddch(startY + 5, startX + 11, ACS_RTEE);
    
    mvaddch(startY + 3, startX + 4, ACS_PLUS);
    mvaddch(startY + 3, startX + 8, ACS_PLUS);
    mvaddch(startY + 5, startX + 4, ACS_PLUS);
    mvaddch(startY + 5, startX + 8, ACS_PLUS);
    
    // Draw internal grid lines
    for (int x = startX + 1; x < startX + 11; ++x) {
        if (x != startX + 4 && x != startX + 8) {
            mvaddch(startY + 3, x, ACS_HLINE);
            mvaddch(startY + 5, x, ACS_HLINE);
        }
    }
    for (int y = startY + 2; y < startY + 7; ++y) {
        if (y != startY + 3 && y != startY + 5) {
            mvaddch(y, startX + 4, ACS_VLINE);
            mvaddch(y, startX + 8, ACS_VLINE);
        }
    }
    
    attroff(COLOR_PAIR(COLOR_PAIR_BORDER));
}

void UI::drawBoardCells(int startY, int startX, int /* cellHeight */, int /* cellWidth */, 
                        const Board& board, int selectedCell, bool isSelected) {
    for (int row = 0; row < 3; ++row) {
        for (int col = 0; col < 3; ++col) {
            int cellIdx = row * 3 + col;
            int y = startY + row * 2;
            int x = startX + col * 4;
            
            Cell cell = board.getCell(row, col);
            char symbol = ' ';
            int colorPair = COLOR_PAIR_BORDER;
            
            if (cell == Cell::X) {
                symbol = 'X';
                colorPair = COLOR_PAIR_X;
            } else if (cell == Cell::O) {
                symbol = 'O';
                colorPair = COLOR_PAIR_O;
            }
            
            if (cellIdx == selectedCell && isSelected) {
                attron(COLOR_PAIR(COLOR_PAIR_SELECTED) | A_BOLD);
            } else {
                attron(COLOR_PAIR(colorPair));
            }
            
            mvaddch(y, x, symbol);
            
            attroff(COLOR_PAIR(COLOR_PAIR_SELECTED) | A_BOLD);
            attroff(COLOR_PAIR(colorPair));
        }
    }
}

void UI::drawBoardStats(int startY, int startX, const Board& board) {
    const auto& stats = board.getStats();
    attron(COLOR_PAIR(COLOR_PAIR_INFO));
    mvprintw(startY, startX, "X:%d O:%d D:%d", stats.xWins, stats.oWins, stats.draws);
    attroff(COLOR_PAIR(COLOR_PAIR_INFO));
}

void UI::drawControlsBar(const std::string& info) {
    int y = getMaxY() - 1;
    attron(COLOR_PAIR(COLOR_PAIR_MENU) | A_BOLD);
    mvhline(y - 1, 0, ACS_HLINE, getMaxX());
    mvprintw(y, 1, "%s", info.c_str());
    attroff(COLOR_PAIR(COLOR_PAIR_MENU) | A_BOLD);
}

void UI::drawHelp() {
    clear();
    
    attron(COLOR_PAIR(COLOR_PAIR_MENU) | A_BOLD);
    mvprintw(1, getMaxX() / 2 - 3, "HELP");
    attroff(COLOR_PAIR(COLOR_PAIR_MENU) | A_BOLD);
    
    int y = 3;
    attron(COLOR_PAIR(COLOR_PAIR_INFO));
    mvprintw(y++, 2, "GAME MODES:");
    mvprintw(y++, 4, "0 Players: Fully automatic (CPU vs CPU)");
    mvprintw(y++, 4, "1 Player:  Manual (Control both X and O)");
    mvprintw(y++, 4, "2 Players: You vs CPU (You are O)");
    
    y += 2;
    mvprintw(y++, 2, "CONTROLS:");
    mvprintw(y++, 4, "[Arrow Keys] Move cursor within board");
    mvprintw(y++, 4, "[Tab] Switch between boards");
    mvprintw(y++, 4, "[1-9] Select board directly");
    mvprintw(y++, 4, "[Enter] Place symbol");
    mvprintw(y++, 4, "[R] Reset current board");
    mvprintw(y++, 4, "[Shift+R] Reset all boards");
    mvprintw(y++, 4, "[Mouse] Click to place symbol");
    mvprintw(y++, 4, "[H] Show this help");
    mvprintw(y++, 4, "[Q] Quit to menu");
    
    attroff(COLOR_PAIR(COLOR_PAIR_INFO));
    
    drawControlsBar("[Press Any Key to Continue]");
    refresh();
    getch();
}

void UI::clear() {
    ::clear();
}

void UI::refresh() {
    ::refresh();
}

int UI::getInput() {
    return getch();
}

bool UI::getMouseClick(int& x, int& y) {
    MEVENT event;
    if (getmouse(&event) == OK) {
        x = event.x;
        y = event.y;
        return true;
    }
    return false;
}

bool UI::mapClickToCell(int mouseX, int mouseY, const std::vector<BoardLayout>& layouts, 
                        int& outBoardIdx, int& outRow, int& outCol) {
    for (size_t boardIdx = 0; boardIdx < layouts.size(); ++boardIdx) {
        const auto& layout = layouts[boardIdx];
        
        if (mouseY < layout.startY || mouseY >= layout.startY + layout.height ||
            mouseX < layout.startX || mouseX >= layout.startX + layout.width) {
            continue;
        }
        
        for (int row = 0; row < 3; ++row) {
            for (int col = 0; col < 3; ++col) {
                if (row >= static_cast<int>(layout.cellPositions.size()) ||
                    col >= static_cast<int>(layout.cellPositions[row].size())) {
                    continue;
                }
                
                const auto& cellPos = layout.cellPositions[row][col];
                if (mouseY >= cellPos.startY && mouseY <= cellPos.endY &&
                    mouseX >= cellPos.startX && mouseX <= cellPos.endX) {
                    outBoardIdx = boardIdx;
                    outRow = row;
                    outCol = col;
                    return true;
                }
            }
        }
    }
    
    return false;
}

}  // namespace ttt
