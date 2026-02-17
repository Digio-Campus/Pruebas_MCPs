#ifndef TICTACTOE_BOARD_H
#define TICTACTOE_BOARD_H

class Board {
public:
    Board();
    char get(int r, int c) const;
    bool makeMove(int r, int c);         // coloca ficha del turno actual (X/O)
    bool makeRandomMove();               // mueve automáticamente según turno
    bool isFinished() const;             // true si hay ganador o empate
    char winner() const;                 // 'X', 'O' o ' ' (ninguno)
    bool isDraw() const;
    void reset();

    char turn; // 'X' o 'O'

    // estadísticas acumuladas por tablero
    int xWins;
    int oWins;
    int draws;

private:
    char cells[3][3];
    int moves;
    char computeWinner() const;
};

#endif // TICTACTOE_BOARD_H
