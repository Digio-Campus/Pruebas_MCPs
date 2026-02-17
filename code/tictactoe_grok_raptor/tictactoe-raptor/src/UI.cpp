#include "../include/UI.h"
#include <ncurses.h>
#include <cmath>
#include <sstream>
#include <algorithm>

using namespace ttt;



UI::UI()
{
    initscr();
    cbreak();
    noecho();
    keypad(stdscr, TRUE);
    curs_set(0);
    mousemask(ALL_MOUSE_EVENTS | REPORT_MOUSE_POSITION, NULL);
    start_color();
    use_default_colors();
    initColors();
    getmaxyx(stdscr, termRows_, termCols_);
}

UI::~UI()
{
    endwin();
}

void UI::initColors()
{
    init_pair(1, COLOR_RED, -1);   // X
    init_pair(2, COLOR_CYAN, -1);  // O
    init_pair(3, COLOR_WHITE, -1); // board text
    init_pair(4, COLOR_GREEN, -1); // header
}

GridLayout UI::computeBoardLayout(int boardIndex, int numBoards) const
{
    // simple distribution: up to 3 columns
    int cols = std::min(3, numBoards);
    int rows = (numBoards + cols - 1) / cols;
    int availableW = termCols_ - 2; // margin
    int availableH = termRows_ - 4; // header/footer
    int boardW = std::max(7, availableW / cols);
    int boardH = std::max(6, availableH / rows);
    int colIdx = boardIndex % cols;
    int rowIdx = boardIndex / cols;
    GridLayout gl;
    gl.cols = 3; gl.rows = 3;
    gl.cellW = std::max(2, boardW / 3);
    gl.cellH = std::max(1, boardH / 3);
    gl.startX = 1 + colIdx * boardW;
    gl.startY = 2 + rowIdx * boardH;
    return gl;
}

void UI::drawMenu(const std::vector<std::string>& items, int selected)
{
    clear();
    mvprintw(0, 2, "TicTacToe - ncurses (arrow keys, Enter, mouse)");
    for (size_t i = 0; i < items.size(); ++i) {
        if ((int)i == selected) attron(A_REVERSE);
        mvprintw(3 + i, 4, items[i].c_str());
        if ((int)i == selected) attroff(A_REVERSE);
    }
    mvprintw(LINES - 1, 2, "Use mouse or arrows+Enter. 'q' to quit.");
    refresh();
}

void UI::drawBoards(const std::vector<Board>& boards, int selectedBoard)
{
    clear();
    mvprintw(0, 2, "TicTacToe - Tab to change board, 1-9 direct select, r/R reset, h help");
    int n = (int)boards.size();
    for (int b = 0; b < n; ++b) {
        auto layout = computeBoardLayout(b, n);
        // header
        const Board& board = boards[b];
        std::string header = "Board " + std::to_string(b+1) + " - ";
        if (board.result() == Result::Ongoing) {
            header += (board.currentTurn() == Cell::X) ? "Turn: X" : "Turn: O";
        } else if (board.result() == Result::X_Win) header += "Winner: X";
        else if (board.result() == Result::O_Win) header += "Winner: O";
        else header += "Draw";
        mvprintw(layout.startY - 1, layout.startX, header.c_str());
        // draw grid
        for (int r = 0; r < 3; ++r) {
            for (int c = 0; c < 3; ++c) {
                int cx = layout.startX + c * layout.cellW + layout.cellW/2;
                int cy = layout.startY + r * layout.cellH + layout.cellH/2;
                Cell cell = board.cellAt(r,c);
                if (cell == Cell::X) {
                    attron(COLOR_PAIR(1) | A_BOLD);
                    mvprintw(cy, cx, "X");
                    attroff(COLOR_PAIR(1) | A_BOLD);
                } else if (cell == Cell::O) {
                    attron(COLOR_PAIR(2) | A_BOLD);
                    mvprintw(cy, cx, "O");
                    attroff(COLOR_PAIR(2) | A_BOLD);
                } else {
                    mvprintw(cy, cx, ".");
                }
            }
        }
        // highlight selected board
        if (b == selectedBoard) {
            int x1 = layout.startX - 1, y1 = layout.startY - 1;
            int x2 = layout.startX + layout.cols * layout.cellW;
            int y2 = layout.startY + layout.rows * layout.cellH;
            // draw simple box
            for (int x = x1; x <= x2; ++x) mvaddch(y1, x, ACS_HLINE);
            for (int x = x1; x <= x2; ++x) mvaddch(y2, x, ACS_HLINE);
            for (int y = y1; y <= y2; ++y) mvaddch(y, x1, ACS_VLINE);
            for (int y = y1; y <= y2; ++y) mvaddch(y, x2, ACS_VLINE);
            mvaddch(y1, x1, ACS_ULCORNER);
            mvaddch(y1, x2, ACS_URCORNER);
            mvaddch(y2, x1, ACS_LLCORNER);
            mvaddch(y2, x2, ACS_LRCORNER);
        }
        // stats below
        auto st = board.stats();
        mvprintw(layout.startY + layout.rows * layout.cellH + 1, layout.startX,
                 "X:%d O:%d D:%d", st.xWins, st.oWins, st.draws);
    }
    mvprintw(LINES - 2, 2, "Controls: arrows/WASD, Enter, mouse. 'h' help, 'q' quit");
    refresh();
}

void UI::drawHelp()
{
    clear();
    mvprintw(1,2, "Help — Controls:");
    mvprintw(3,4, "Arrow keys / WASD : move (when applicable)");
    mvprintw(4,4, "Enter or mouse click : place mark");
    mvprintw(5,4, "Tab or keys 1-9 : switch selected board");
    mvprintw(6,4, "r : reset selected board, R : reset all");
    mvprintw(8,4, "Modes:\n 0 — automatic (all boards auto-fill)\n 1 — single player (player controls X and O)\n 2 — player controls O, X is auto");
    mvprintw(LINES-2,2, "Press any key to return");
    refresh();
}

void UI::drawSettings(const Settings& s, int selField)
{
    clear();
    mvprintw(2,4, "Settings");
    mvprintw(4,6, "Number of players (0=auto,1=single,2=player v auto): %d", s.numPlayers());
    mvprintw(5,6, "Number of boards (1-9): %d", s.numBoards());
    mvprintw(LINES-2,2, "Use left/right to change values, Enter to accept");
    refresh();
}

std::optional<std::tuple<int,int,int>> UI::mapClickToCell(int mouseX, int mouseY, int numBoards)
{
    for (int b = 0; b < numBoards; ++b) {
        auto layout = computeBoardLayout(b, numBoards);
        auto cell = getCellFromCoord(mouseX, mouseY, layout);
        if (cell) {
            return std::make_tuple(b, cell->first, cell->second);
        }
    }
    return std::nullopt;
}

void UI::refreshAll() { getmaxyx(stdscr, termRows_, termCols_); refresh(); }
