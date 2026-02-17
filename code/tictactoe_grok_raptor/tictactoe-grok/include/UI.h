#ifndef UI_H
#define UI_H

#include <ncurses.h>
#include <vector>
#include <string>
#include "Board.h"
#include "Settings.h"

namespace ttt {

class UI {
public:
    UI();
    ~UI();

    void init();
    void cleanup();

    void drawMenu(const std::vector<std::string>& options, int selected);
    void drawSettingsMenu(const Settings& settings, int selectedOption);
    void drawHelp();
    void drawGame(const std::vector<Board>& boards, int selectedBoard, int cursorRow, int cursorCol);

    std::pair<int, int> getTerminalSize() const;

private:
    void initColors();
    void drawBoard(const Board& board, int startY, int startX, bool isSelected, int cursorRow, int cursorCol);
    void drawBoardStats(const Board& board, int startY, int startX);
};

} // namespace ttt

#endif // UI_H