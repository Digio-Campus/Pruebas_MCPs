#pragma once

#include <optional>
#include <tuple>
#include <vector>
#include <string>
#include "Board.h"
#include "Settings.h"

namespace ttt {

// Pure layout description used for hit-testing and unit tests
struct GridLayout {
    int startX = 0;
    int startY = 0;
    int cellW = 4;   // width in columns per tic cell
    int cellH = 2;   // height in rows per tic cell
    int rows = 3;
    int cols = 3;
};

// Convert absolute coordinates (screen cols, rows) to (row,col) in a grid.
// Returns std::nullopt if (x,y) is outside the grid.
inline std::optional<std::pair<int,int>> getCellFromCoord(int x, int y, const GridLayout& layout)
{
    int gridW = layout.cols * layout.cellW;
    int gridH = layout.rows * layout.cellH;
    if (x < layout.startX || x >= layout.startX + gridW) return std::nullopt;
    if (y < layout.startY || y >= layout.startY + gridH) return std::nullopt;
    int relX = x - layout.startX;
    int relY = y - layout.startY;
    int col = relX / layout.cellW;
    int row = relY / layout.cellH;
    if (row < 0 || row >= layout.rows || col < 0 || col >= layout.cols) return std::nullopt;
    return std::make_pair(row, col);
}

class UI {
public:
    UI();
    ~UI();

    // drawing
    void drawMenu(const std::vector<std::string>& items, int selected);
    void drawBoards(const std::vector<Board>& boards, int selectedBoard);
    void drawHelp();
    void drawSettings(const Settings& s, int selField);

    // mapping
    // returns (boardIndex, row, col) if the click is inside a cell
    std::optional<std::tuple<int,int,int>> mapClickToCell(int mouseX, int mouseY, int numBoards);

    void refreshAll();

private:
    void initColors();
    GridLayout computeBoardLayout(int boardIndex, int numBoards) const;
    int termRows_, termCols_;
};

} // namespace ttt
