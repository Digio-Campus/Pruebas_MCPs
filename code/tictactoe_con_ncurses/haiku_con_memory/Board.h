#ifndef BOARD_H
#define BOARD_H

#include <array>
#include <vector>

enum class CellState { EMPTY, X, O };
enum class GameState { IN_PROGRESS, X_WINS, O_WINS, DRAW };

class Board {
private:
    std::array<std::array<CellState, 3>, 3> grid;
    CellState currentTurn;  // X or O
    GameState gameState;
    int turnsPlayed;

    // Helper methods
    bool checkWin(CellState player) const;
    bool isBoardFull() const;
    void updateGameState();

public:
    Board();
    
    // Game logic
    bool makeMove(int row, int col, CellState player);
    bool canMakeMove(int row, int col) const;
    void reset();
    
    // State queries
    CellState getCell(int row, int col) const;
    CellState getCurrentTurn() const;
    GameState getGameState() const;
    bool isGameOver() const;
    
    // Statistics
    int getTurnsPlayed() const;
    std::vector<std::pair<int, int>> getEmptyCells() const;
};

#endif // BOARD_H
