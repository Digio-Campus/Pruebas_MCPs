#ifndef TICTACTOE_GAME_H
#define TICTACTOE_GAME_H

#include "Board.h"
#include <vector>
#include <random>

namespace ttt {

enum class Mode { Auto = 0, ManualBoth = 1, PlayerO_AutoX = 2 };

struct BoardStats { int xWins = 0; int oWins = 0; int draws = 0; };

class Game {
public:
    Game(int numBoards = 1, Mode mode = Mode::ManualBoth);

    void resetAll();
    void resetBoard(int idx);

    int boardCount() const;
    Board& getBoard(int idx);
    const Board& getBoard(int idx) const;

    void setMode(Mode m);
    Mode mode() const;

    void setNumBoards(int n);
    int numBoards() const;

    // Realiza pasos automáticos (modo 0 o generación de X en modo 2). Devuelve true si se ejecutó algún movimiento.
    bool updateAutoPlayStep(int delayMs = 0);

    // Movimientos realizados por el jugador (invocado desde la UI)
    void playerMove(int boardIdx, int row, int col);

    const std::vector<BoardStats>& stats() const;

private:
    std::vector<Board> boards_;
    Mode mode_;
    std::vector<BoardStats> stats_;
    std::vector<Result> lastRecordedResult_; // evita contabilizar repetidamente el mismo resultado
    std::mt19937 rng_;

    void maybeRecordResult(int idx);
};

} // namespace ttt

#endif // TICTACTOE_GAME_H
