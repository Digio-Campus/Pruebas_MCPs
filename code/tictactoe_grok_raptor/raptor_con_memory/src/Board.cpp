#include "../include/Board.h"
#include <algorithm>
#include <cstdlib>

Board::Board() { reset(); xWins = oWins = draws = 0; }

void Board::reset() {
    moves = 0;
    turn = 'X';
    for (int r = 0; r < 3; ++r)
        for (int c = 0; c < 3; ++c)
            cells[r][c] = ' ';
}

char Board::get(int r, int c) const {
    if (r < 0 || r >= 3 || c < 0 || c >= 3) return ' ';
    return cells[r][c];
}

bool Board::makeMove(int r, int c) {
    if (r < 0 || r >= 3 || c < 0 || c >= 3) return false;
    if (cells[r][c] != ' ') return false;
    cells[r][c] = turn;
    ++moves;
    // alternancia estricta
    turn = (turn == 'X') ? 'O' : 'X';
    return true;
}

bool Board::makeRandomMove() {
    if (isFinished()) return false;
    // recoger casillas vacías
    int empties[9];
    int ec = 0;
    for (int r = 0; r < 3; ++r)
        for (int c = 0; c < 3; ++c)
            if (cells[r][c] == ' ') empties[ec++] = r*3 + c;
    if (ec == 0) return false;
    int pick = empties[std::rand() % ec];
    int rr = pick / 3;
    int cc = pick % 3;
    return makeMove(rr, cc);
}

char Board::computeWinner() const {
    // filas/columnas
    for (int i = 0; i < 3; ++i) {
        if (cells[i][0] != ' ' && cells[i][0] == cells[i][1] && cells[i][1] == cells[i][2])
            return cells[i][0];
        if (cells[0][i] != ' ' && cells[0][i] == cells[1][i] && cells[1][i] == cells[2][i])
            return cells[0][i];
    }
    // diagonales
    if (cells[0][0] != ' ' && cells[0][0] == cells[1][1] && cells[1][1] == cells[2][2]) return cells[0][0];
    if (cells[0][2] != ' ' && cells[0][2] == cells[1][1] && cells[1][1] == cells[2][0]) return cells[0][2];
    return ' ';
}

bool Board::isFinished() const {
    if (computeWinner() != ' ') return true;
    return moves >= 9;
}

char Board::winner() const { return computeWinner(); }

bool Board::isDraw() const { return computeWinner() == ' ' && moves >= 9; }
