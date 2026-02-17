#ifndef BOARD_H
#define BOARD_H

#include <vector>
#include <string>

class Board {
private:
    char grid[3][3];
    char currentPlayer;
    int xWins;
    int oWins;
    int draws;

    void initializeGrid();
    bool isValidMove(int row, int col) const;
    char checkWinner() const;

public:
    Board();
    ~Board() = default;

    // Game logic
    bool makeMove(int row, int col);
    void makeAutoMove();
    void reset();

    // Check game state
    bool checkWin() const;
    bool checkDraw() const;
    char getWinner() const;
    char getCurrentPlayer() const { return currentPlayer; }

    // Get board state
    char getCell(int row, int col) const;
    std::string getBoardString() const;

    // Statistics
    int getXWins() const { return xWins; }
    int getOWins() const { return oWins; }
    int getDraws() const { return draws; }
    void incrementXWins() { xWins++; }
    void incrementOWins() { oWins++; }
    void incrementDraws() { draws++; }
};

#endif // BOARD_H