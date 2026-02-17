#ifndef BOARD_H
#define BOARD_H

#include <string>

class Board {
private:
    char grid[3][3];  // ' ' empty, 'X', 'O'
    char currentPlayer;  // 'X' or 'O'
    int xWins;
    int oWins;
    int draws;

public:
    Board();
    void reset();
    bool makeMove(int row, int col);
    void makeAutoMove();
    char checkWin();
    bool checkDraw();
    char getCell(int row, int col) const;
    char getCurrentPlayer() const;
    void incrementWin(char player);
    int getXWins() const;
    int getOWins() const;
    int getDraws() const;
};

#endif // BOARD_H