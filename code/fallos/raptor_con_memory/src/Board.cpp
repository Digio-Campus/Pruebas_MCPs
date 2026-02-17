#include "../include/Board.h"
#include <random>
#include <ctime>

Board::Board() { reset(); xWins = oWins = draws = 0; }

void Board::reset() {
    for (auto &r : cells) for (auto &c : r) c = ' ';
    turn = 'X';
    winChar = ' ';
}

char Board::currentTurn() const { return turn; }

char Board::getCell(int r,int c) const { return cells[r][c]; }

bool Board::isFull() const {
    for (auto &row : cells) for (char c : row) if (c == ' ') return false;
    return true;
}

bool Board::checkWinFor(char p) const {
    for (int i=0;i<3;i++) {
        if (cells[i][0]==p && cells[i][1]==p && cells[i][2]==p) return true;
        if (cells[0][i]==p && cells[1][i]==p && cells[2][i]==p) return true;
    }
    if (cells[0][0]==p && cells[1][1]==p && cells[2][2]==p) return true;
    if (cells[0][2]==p && cells[1][1]==p && cells[2][0]==p) return true;
    return false;
}

bool Board::hasWinner() const { return winChar != ' '; }

char Board::winner() const { return winChar; }

bool Board::isDraw() const { return isFull() && winChar==' '; }

bool Board::makeMove(int r, int c) {
    if (r<0||r>2||c<0||c>2) return false;
    if (cells[r][c] != ' ') return false;
    if (winChar != ' ') return false; // already finished
    cells[r][c] = turn;
    // check for win
    if (checkWinFor(turn)) {
        winChar = turn;
        if (turn=='X') xWins++; else oWins++;
    } else if (isFull()) {
        draws++;
    }
    // alternate turn if board not finished
    if (winChar==' ') turn = (turn=='X') ? 'O' : 'X';
    return true;
}

bool Board::makeAutoMove() {
    if (winChar != ' ') return false;
    std::vector<std::pair<int,int>> freeCells;
    for (int r=0;r<3;r++) for (int c=0;c<3;c++) if (cells[r][c]==' ') freeCells.emplace_back(r,c);
    if (freeCells.empty()) return false;
    static std::mt19937 rng((unsigned)time(NULL));
    std::uniform_int_distribution<int> dist(0, (int)freeCells.size()-1);
    auto sel = freeCells[dist(rng)];
    return makeMove(sel.first, sel.second);
}
