#ifndef UI_H
#define UI_H

#include <ncurses.h>
#include <vector>
#include "Board.h"

struct BoardPosition {
    int startX;
    int startY;
    int width;
    int height;
};

class UI {
private:
    int maxX, maxY;
    std::vector<BoardPosition> boardPositions;
    int selectedBoardIdx;
    int selectedCellRow, selectedCellCol;

public:
    UI();
    ~UI();
    
    // Initialization
    void init();
    void cleanup();
    
    // Layout
    void calculateBoardPositions(int numBoards, int boardsPerRow);
    BoardPosition getBoardPosition(int boardIdx) const;
    
    // Drawing
    void drawBoard(const Board& board, int boardIdx, bool selected);
    void drawAllBoards(const std::vector<Board>& boards, int selectedIdx);
    void drawMainMenu();
    void drawSettingsMenu(int gameMode, int numBoards);
    void drawHelpMenu();
    void drawGameOver(GameState state);
    
    // Screen management
    void clear();
    void refresh();
    void handleResize();
    
    // Utilities
    int getMaxX() const;
    int getMaxY() const;
    bool cellClicked(int mouseX, int mouseY, int boardIdx, int& row, int& col);
};

#endif // UI_H
