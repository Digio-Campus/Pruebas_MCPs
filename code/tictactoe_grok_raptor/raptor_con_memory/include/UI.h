#ifndef TICTACTOE_UI_H
#define TICTACTOE_UI_H

#include <vector>
#include <string>
#include "Board.h"
#include "Settings.h"

class UI {
public:
    UI();
    ~UI();
    void init();
    void shutdown();
    void refreshSize();

    void drawMainMenu(const std::vector<std::string>& options, int highlight);
    void drawSettings(const Settings& s, int highlight);
    void drawHelp();
    void drawBoards(const std::vector<Board>& boards, int selectedBoard, int curR, int curC, const Settings& s);

    // mapea (mouseX,mouseY) -> board index, cell row, cell col. retorna true si dentro de un tablero.
    bool mapClickToBoardCell(int mx, int my, int numBoards, int& outBoard, int& outR, int& outC);

    int termRows() const;
    int termCols() const;

private:
    int rows_, cols_;
};

#endif // TICTACTOE_UI_H
