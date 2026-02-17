#pragma once

#include <array>
#include <random>
#include <vector>
#include <optional>

namespace ttt {

enum class Cell { Empty, X, O };
enum class Result { Ongoing, X_Win, O_Win, Draw };

struct BoardStats {
    int xWins = 0;
    int oWins = 0;
    int draws = 0;
};

class Board {
public:
    Board();

    // Attempt to place a mark for the current turn at (row,col).
    // Returns true if move succeeded and toggles turn (unless game ended).
    bool makeMove(int row, int col);

    // Let the board play an automatic move for the current turn (uses mt19937)
    // Returns true if a move was made.
    bool makeAutoMove();

    void reset();

    std::vector<int> availableMoves() const; // indices 0..8

    Cell cellAt(int row, int col) const;
    Result result() const { return result_; }
    Cell currentTurn() const { return turn_; }
    const BoardStats& stats() const { return stats_; }

private:
    void updateResult();
    Result checkWinInternal() const;

    std::array<Cell, 9> cells_;
    Cell turn_;
    Result result_;
    BoardStats stats_;
    std::mt19937 rng_;
};

} // namespace ttt
