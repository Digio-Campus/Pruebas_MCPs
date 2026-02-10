#include "UI.h"
#include <cstring>
#include <algorithm>
#include <string>

UI::UI() : selectedBoardIdx(0), selectedCellRow(0), selectedCellCol(0) {
    getmaxyx(stdscr, maxY, maxX);
}

UI::~UI() {
    cleanup();
}

void UI::init() {
    initscr();
    raw();
    noecho();
    keypad(stdscr, TRUE);
    cbreak();
    nodelay(stdscr, TRUE);
    
    // Colors
    start_color();
    init_pair(1, COLOR_BLACK, COLOR_WHITE);  // Normal
    init_pair(2, COLOR_WHITE, COLOR_BLACK);  // Selected
    init_pair(3, COLOR_RED, COLOR_BLACK);    // X
    init_pair(4, COLOR_BLUE, COLOR_BLACK);   // O
    init_pair(5, COLOR_GREEN, COLOR_BLACK);  // Success
    
    getmaxyx(stdscr, maxY, maxX);
}

void UI::cleanup() {
    endwin();
}

void UI::calculateBoardPositions(int numBoards, int boardsPerRow) {
    boardPositions.clear();
    
    int boardWidth = 11;
    int boardHeight = 7;
    int paddingX = 2;
    int paddingY = 1;
    
    for (int i = 0; i < numBoards; i++) {
        int row = i / boardsPerRow;
        int col = i % boardsPerRow;
        
        BoardPosition pos;
        pos.startX = col * (boardWidth + paddingX);
        pos.startY = row * (boardHeight + paddingY);
        pos.width = boardWidth;
        pos.height = boardHeight;
        
        boardPositions.push_back(pos);
    }
}

BoardPosition UI::getBoardPosition(int boardIdx) const {
    if (boardIdx < 0 || boardIdx >= static_cast<int>(boardPositions.size())) {
        return {0, 0, 0, 0};
    }
    return boardPositions[boardIdx];
}

void UI::drawBoard(const Board& board, int boardIdx, bool selected) {
    BoardPosition pos = getBoardPosition(boardIdx);
    
    int attr = selected ? COLOR_PAIR(2) : COLOR_PAIR(1);
    attron(attr);
    
    // Draw border and title
    mvaddch(pos.startY, pos.startX, ACS_ULCORNER);
    mvaddch(pos.startY, pos.startX + pos.width - 1, ACS_URCORNER);
    mvaddch(pos.startY + pos.height - 1, pos.startX, ACS_LLCORNER);
    mvaddch(pos.startY + pos.height - 1, pos.startX + pos.width - 1, ACS_LRCORNER);
    
    for (int i = 1; i < pos.width - 1; i++) {
        mvaddch(pos.startY, pos.startX + i, ACS_HLINE);
        mvaddch(pos.startY + pos.height - 1, pos.startX + i, ACS_HLINE);
    }
    
    for (int i = 1; i < pos.height - 1; i++) {
        mvaddch(pos.startY + i, pos.startX, ACS_VLINE);
        mvaddch(pos.startY + i, pos.startX + pos.width - 1, ACS_VLINE);
    }
    
    attroff(attr);
    
    // Draw grid cells (3x3)
    int cellWidth = 3;
    int cellHeight = 1;
    
    for (int r = 0; r < 3; r++) {
        for (int c = 0; c < 3; c++) {
            int cellX = pos.startX + 1 + c * cellWidth;
            int cellY = pos.startY + 1 + r * cellHeight;
            
            CellState cell = board.getCell(r, c);
            char symbol = ' ';
            int colorPair = 1;
            
            if (cell == CellState::X) {
                symbol = 'X';
                colorPair = 3;
            } else if (cell == CellState::O) {
                symbol = 'O';
                colorPair = 4;
            }
            
            attron(COLOR_PAIR(colorPair));
            mvprintw(cellY, cellX + 1, "%c", symbol);
            attroff(COLOR_PAIR(colorPair));
            
            if (c < 2) mvaddch(cellY, cellX + 3, ACS_VLINE);
        }
        if (r < 2) {
            mvaddch(pos.startY + 2 + r * cellHeight, pos.startX + 1, ACS_PLUS);
            for (int c = 0; c < 3; c++) {
                mvaddch(pos.startY + 2 + r * cellHeight, pos.startX + 4 + c * cellWidth, ACS_HLINE);
                if (c < 2) mvaddch(pos.startY + 2 + r * cellHeight, pos.startX + 4 + c * cellWidth + 1, ACS_PLUS);
            }
        }
    }
    
    // Draw status
    attron(COLOR_PAIR(1));
    std::string status = "";
    if (board.isGameOver()) {
        GameState state = board.getGameState();
        if (state == GameState::X_WINS) status = "X WINS";
        else if (state == GameState::O_WINS) status = "O WINS";
        else status = "DRAW";
        attron(COLOR_PAIR(5));
    } else {
        status = board.getCurrentTurn() == CellState::X ? "TURN: X" : "TURN: O";
    }
    mvprintw(pos.startY + pos.height - 1, pos.startX + 1, "[%s]", status.c_str());
    attroff(COLOR_PAIR(1));
}

void UI::drawAllBoards(const std::vector<Board>& boards, int selectedIdx) {
    clear();
    
    for (int i = 0; i < static_cast<int>(boards.size()); i++) {
        drawBoard(boards[i], i, i == selectedIdx);
    }
    
    // Draw navigation info
    mvprintw(maxY - 1, 0, "Use Arrow Keys to navigate, SPACE to select cell, 'h' help, 'q' quit");
    
    refresh();
}

void UI::drawMainMenu() {
    clear();
    
    mvprintw(maxY / 2 - 3, maxX / 2 - 10, "===== TIC TAC TOE =====");
    mvprintw(maxY / 2 - 1, maxX / 2 - 8, "1. Play Game");
    mvprintw(maxY / 2, maxX / 2 - 8, "2. Settings");
    mvprintw(maxY / 2 + 1, maxX / 2 - 8, "3. Help");
    mvprintw(maxY / 2 + 2, maxX / 2 - 8, "4. Quit");
    mvprintw(maxY / 2 + 4, maxX / 2 - 15, "Use mouse or keyboard to navigate");
    
    refresh();
}

void UI::drawSettingsMenu(int gameMode, int numBoards) {
    clear();
    
    mvprintw(2, 2, "SETTINGS");
    mvprintw(4, 4, "Game Mode:");
    mvprintw(5, 6, "1. Zero Players (All Auto)");
    mvprintw(6, 6, "2. One Player (Manual Both Sides)");
    mvprintw(7, 6, "3. Two Players (Player vs AI)");
    mvprintw(8, 4, "Current: Mode %d", gameMode + 1);
    
    mvprintw(10, 4, "Number of Boards:");
    mvprintw(11, 6, "Type a number (1-9): %d", numBoards);
    
    mvprintw(maxY - 2, 2, "Press 'b' to go back, ENTER to confirm");
    
    refresh();
}

void UI::drawHelpMenu() {
    clear();
    
    mvprintw(1, 2, "=== HELP ===");
    mvprintw(3, 2, "NAVIGATION:");
    mvprintw(4, 4, "Arrow Keys: Move between boards or cells");
    mvprintw(5, 4, "Mouse: Click on menu items or board cells");
    
    mvprintw(7, 2, "GAME MODES:");
    mvprintw(8, 4, "0 Players: Boards fill automatically with random moves");
    mvprintw(9, 4, "1 Player: Control X and O manually (alternating)");
    mvprintw(10, 4, "2 Players: You play O, AI plays X");
    
    mvprintw(12, 2, "RULES:");
    mvprintw(13, 4, "- Each board is independent");
    mvprintw(14, 4, "- X and O alternate turns strictly");
    mvprintw(15, 4, "- First to get 3 in a row wins");
    mvprintw(16, 4, "- Game ends in draw if board is full");
    
    mvprintw(maxY - 1, 2, "Press any key to continue...");
    
    refresh();
}

void UI::drawGameOver(GameState state) {
    std::string message;
    switch (state) {
        case GameState::X_WINS:
            message = "X WINS!";
            break;
        case GameState::O_WINS:
            message = "O WINS!";
            break;
        case GameState::DRAW:
            message = "DRAW!";
            break;
        default:
            message = "GAME IN PROGRESS";
    }
    
    attron(COLOR_PAIR(5));
    mvprintw(maxY / 2, maxX / 2 - message.length() / 2, "%s", message.c_str());
    attroff(COLOR_PAIR(5));
}

void UI::clear() {
    ::clear();
}

void UI::refresh() {
    ::refresh();
}

void UI::handleResize() {
    getmaxyx(stdscr, maxY, maxX);
}

int UI::getMaxX() const {
    return maxX;
}

int UI::getMaxY() const {
    return maxY;
}

bool UI::cellClicked(int mouseX, int mouseY, int boardIdx, int& row, int& col) {
    BoardPosition pos = getBoardPosition(boardIdx);
    
    if (mouseX < pos.startX || mouseX >= pos.startX + pos.width ||
        mouseY < pos.startY || mouseY >= pos.startY + pos.height) {
        return false;
    }
    
    int relX = mouseX - pos.startX - 1;
    int relY = mouseY - pos.startY - 1;
    
    if (relX < 0 || relX >= 9 || relY < 0 || relY >= 3) {
        return false;
    }
    
    col = relX / 3;
    row = relY;
    
    return true;
}
