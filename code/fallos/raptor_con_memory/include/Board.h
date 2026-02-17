#ifndef BOARD_H
#define BOARD_H

#include <array>

class Board {
public:
    Board();
    void reset();
    bool makeMove(int r, int c); // human/legal move
    bool makeAutoMove(); // random move for AI/auto
    char getCell(int r,int c) const;
    char currentTurn() const;
    bool hasWinner() const;
    char winner() const; // 'X' 'O' or ' '
    bool isDraw() const;
    bool isFull() const;
    int xWins, oWins, draws;
private:
    std::array<std::array<char,3>,3> cells;
    char turn;
    char winChar; // 'X' or 'O' when someone won, ' ' otherwise
    bool checkWinFor(char p) const;
};

#endif
