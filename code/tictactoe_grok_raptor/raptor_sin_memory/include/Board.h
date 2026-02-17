#ifndef TICTACTOE_BOARD_H
#define TICTACTOE_BOARD_H

#include <array>
#include <vector>

namespace ttt {

enum class Cell { Empty = 0, X, O };
enum class Result { Ongoing = 0, X_Win, O_Win, Draw };

class Board {
public:
    Board(int id = 0);
    void reset();

    // Hace un movimiento según el turno actual; retorna true si se colocó la ficha
    bool makeMove(int row, int col);

    // Fuerza un movimiento con el jugador indicado (útil para IA/autocompletar)
    bool makeMoveForced(int row, int col, Cell who);

    std::vector<std::pair<int,int>> availableMoves() const;

    Result result() const;
    Cell cellAt(int row, int col) const;
    Cell currentTurn() const;
    void switchTurn();
    bool isFull() const;
    int movesMade() const;
    int id() const;
    void setId(int id);

private:
    std::array<std::array<Cell,3>,3> grid_;
    Cell turn_;
    Result result_;
    int moves_;
    int id_;

    void updateResult();
};

} // namespace ttt

#endif // TICTACTOE_BOARD_H
