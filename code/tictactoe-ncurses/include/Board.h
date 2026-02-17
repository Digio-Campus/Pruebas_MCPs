#ifndef BOARD_H
#define BOARD_H

#include <vector>
#include <random>

namespace ttt {

enum class Cell { X, O, Empty };
enum class Result { X_Win, O_Win, Draw, Ongoing };

struct BoardStats {
    int xWins = 0;
    int oWins = 0;
    int draws = 0;
};

class Board {
public:
    Board();
    
    // Game operations
    bool makeMove(int row, int col, Cell player);
    bool makeAutoMove();
    void reset();
    
    // Queries
    Cell getCell(int row, int col) const;
    Cell getCurrentTurn() const;
    Result getResult() const;
    const BoardStats& getStats() const;
    
    std::vector<std::pair<int, int>> availableMoves() const;
    bool isFull() const;

private:
    std::vector<std::vector<Cell>> grid_;
    Cell currentTurn_;
    Result result_;
    BoardStats stats_;
    Result lastRecordedResult_;
    
    std::mt19937 rng_;
    
    void updateResult();
    bool checkWin(Cell player);
    bool checkDraw();
};

}  // namespace ttt

#endif  // BOARD_H
