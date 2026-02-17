#ifndef UI_H
#define UI_H

#include <ncurses.h>
#include <vector>
#include <utility>
#include "Board.h"
#include "Settings.h"

class UI {
private:
    int colorPairs[6];
    struct BoardLayout {
        int startY, startX;
        int height, width;
    };
    std::vector<BoardLayout> boardLayouts;
    int termHeight, termWidth;
    bool mouseEnabled;

    void calculateLayout(int numBoards);
    void initColors();

public:
    UI();
    ~UI();
    void init();
    void cleanup();
    void getTerminalSize();
    void drawMenu(int selected);
    void drawSettingsMenu(Settings& settings, int selectedOption, int selectedValue);
    void drawBoards(const std::vector<Board>& boards, int selectedBoard, int cursorRow, int cursorCol);
    void drawHelp();
    void drawStats(const std::vector<Board>& boards);
    std::pair<int, int> mapClickToCell(int x, int y, int boardIndex);
    bool isMouseEnabled() const;
};

#endif // UI_H