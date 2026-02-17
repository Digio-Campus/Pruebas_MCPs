#ifndef UI_H
#define UI_H

#include <vector>
#include <string>
#include "Board.h"

namespace ttt {

struct CellPosition {
    int startY = 0;
    int startX = 0;
    int endY = 0;
    int endX = 0;
};

struct BoardLayout {
    int startY = 0;
    int startX = 0;
    int height = 0;
    int width = 0;
    std::vector<std::vector<CellPosition>> cellPositions;
};

class UI {
public:
    UI();
    ~UI();
    
    UI(const UI&) = delete;
    UI& operator=(const UI&) = delete;
    
    // Initialization
    void init();
    void cleanup();
    
    // Dimension queries
    int getMaxY() const;
    int getMaxX() const;
    bool isTerminalTooSmall() const;
    
    // Rendering
    void drawMenu(const std::vector<std::string>& options, int selected);
    void drawSettingsMenu(int numPlayers, int numBoards, int selectedOption, bool editingValue);
    void drawBoards(const std::vector<Board>& boards, const std::vector<int>& selectedCells, int selectedBoard);
    void drawHelp();
    void clear();
    void refresh();
    
    // Input
    int getInput();
    bool getMouseClick(int& x, int& y);
    
    // Hit testing
    bool mapClickToCell(int mouseX, int mouseY, const std::vector<BoardLayout>& layouts, 
                       int& outBoardIdx, int& outRow, int& outCol);

private:
    int maxY_ = 0;
    int maxX_ = 0;
    
    // Colors
    void initColors();
    
    // Drawing helpers
    void drawBoardFrame(int startY, int startX, int boardIdx, const Board& board);
    void drawBoardCells(int startY, int startX, int cellHeight, int cellWidth, 
                       const Board& board, int selectedCell, bool isSelected);
    void drawBoardStats(int startY, int startX, const Board& board);
    void drawControlsBar(const std::string& info);
    void calculateBoardLayouts(const std::vector<BoardLayout>& layouts);
};

}  // namespace ttt

#endif  // UI_H
