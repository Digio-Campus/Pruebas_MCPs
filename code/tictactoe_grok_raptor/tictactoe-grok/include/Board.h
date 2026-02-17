#ifndef BOARD_H
#define BOARD_H

#include <vector>
#include <random>
#include <optional>

namespace ttt {

enum class Cell {
    Empty,
    X,
    O
};

enum class Result {
    Ongoing,
    X_Win,
    O_Win,
    Draw
};

struct BoardStats {
    int xWins = 0;
    int oWins = 0;
    int draws = 0;
};

class Board {
public:
    Board();
    ~Board() = default;

    // Getters
    Cell getCell(int row, int col) const;
    Cell getCurrentTurn() const;
    Result getResult() const;
    const BoardStats& getStats() const;
    std::vector<std::pair<int, int>> getAvailableMoves() const;

    // Actions
    bool makeMove(int row, int col);
    void makeAutoMove();
    void reset();

private:
    std::vector<std::vector<Cell>> grid_;
    Cell currentTurn_;
    Result result_;
    BoardStats stats_;
    std::optional<Result> lastRecordedResult_;
    mutable std::mt19937 rng_;

    void updateResult();
    bool checkWin(Cell player) const;
    bool checkDraw() const;
    void recordResult();
};

} // namespace ttt

#endif // BOARD_H