#ifndef UI_H
#define UI_H

#include <ncurses.h>
#include <string>
#include <vector>

class Board;
class Settings;

class UI {
private:
    int maxY, maxX;
    int colorPairs[6];  // 6 color pairs

    void initColors();
    void calculateLayout(int numBoards, int& rows, int& cols, int& boardHeight, int& boardWidth);
    void drawBorder(int startY, int startX, int height, int width);
    std::string centerText(const std::string& text, int width);

public:
    UI();
    ~UI();

    void init();
    void cleanup();

    // Drawing methods
    void drawMenu(size_t selectedOption, const std::vector<std::string>& options);
    void drawSettingsMenu(Settings& settings, size_t selectedOption);
    void drawBoards(const std::vector<Board>& boards, size_t selectedBoard, int cursorRow, int cursorCol);
    void drawSingleBoard(int y, int x, const Board& board, bool isSelected, int cursorRow, int cursorCol);
    void drawStats(const std::vector<Board>& boards);
    void drawHelp();
    void clearScreen();

    // Input
    int getInput();

    // Layout
    void getBoardPosition(int boardIndex, int numBoards, int& y, int& x, int& height, int& width);
};

#endif // UI_H