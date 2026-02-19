#ifndef GAME_H
#define GAME_H

#include <vector>
#include "Board.h"
#include "UI.h"
#include "Settings.h"

namespace ttt {

class Game {
public:
    Game(UI& ui, Settings& settings);
    
    void play();

private:
    UI& ui_;
    Settings& settings_;
    
    std::vector<Board> boards_;
    std::vector<int> selectedCells_;  // 0-8 for each board
    int selectedBoardIdx_;
    
    bool gameActive_;
    
    // Mode handlers
    void handleMode0();  // 0 players (auto)
    void handleMode1();  // 1 player (manual)
    void handleMode2();  // 2 players (1 vs AI)
    
    // Input handling
    bool playerMove(int boardIdx, int cellIdx, Cell player);
    bool handleInput();
    
    // Board management
    void initializeBoards();
    void updateBoardLayouts(std::vector<BoardLayout>& layouts);
    void drawFrame();
    
    // Utilities
    std::pair<int, int> cellIndexToCoords(int cellIdx) const;
    int coordsToCell(int row, int col) const;
};

}  // namespace ttt

#endif  // GAME_H
