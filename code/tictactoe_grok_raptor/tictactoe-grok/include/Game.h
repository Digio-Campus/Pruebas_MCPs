#ifndef GAME_H
#define GAME_H

#include "UI.h"
#include "Settings.h"
#include "Board.h"
#include <vector>

namespace ttt {

class Game {
public:
    Game(UI& ui, Settings& settings);

    void run();

private:
    UI& ui_;
    Settings& settings_;
    std::vector<Board> boards_;
    int selectedBoard_;
    int cursorRow_;
    int cursorCol_;

    void initBoards();
    void handleMode0();
    void handleMode1();
    void handleMode2();
    bool playerMove(int boardIdx, int row, int col);
    void resetBoard(int idx);
    void resetAllBoards();
    void switchBoard(int direction);
    void selectBoard(int num);
    bool allBoardsFinished() const;
};

} // namespace ttt

#endif // GAME_H