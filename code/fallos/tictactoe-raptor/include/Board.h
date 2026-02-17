#pragma once

#include <array>

// Simple 3x3 TicTacToe board class with per-board stats.
// - Maintains strict alternation of turns inside the board (currentTurn)
// - Provides makeMove / makeAutoMove / checkWin / checkDraw / reset

class Board {
public:
    enum Cell : char { EMPTY = ' ', X = 'X', O = 'O' };

    Board();

    // Place currentTurn at position (0..8). Returns true if placed.
    bool placeAt(int pos);

    // Make an automatic (random) move for currentTurn; returns true if moved.
    bool makeAutoMove();

    // Check winner: returns 'X' or 'O' if winner, otherwise ' ' (EMPTY)
    char checkWin() const;

    // Return true when board is full and no winner
    bool checkDraw() const;

    // Reset board state and keep statistics
    void reset();

    // Getters
    char cellAt(int pos) const;
    char getTurn() const { return currentTurn; }
    void toggleTurn();

    // statistics
    int xWins() const { return statXWins; }
    int oWins() const { return statOWins; }
    int draws() const { return statDraws; }

private:
    std::array<Cell, 9> cells;
    char currentTurn; // either 'X' or 'O'

    // per-board statistics
    int statXWins;
    int statOWins;
    int statDraws;
};
