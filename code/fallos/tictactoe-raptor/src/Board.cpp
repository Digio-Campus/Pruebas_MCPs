#include "../include/Board.h"
#include <random>
#include <algorithm>

Board::Board() {
    reset();
}

bool Board::placeAt(int pos) {
    if (pos < 0 || pos >= 9) return false;
    if (cells[pos] != EMPTY) return false;
    cells[pos] = static_cast<Cell>(currentTurn);

    char winner = checkWin();
    if (winner == 'X') statXWins++;
    else if (winner == 'O') statOWins++;
    else if (checkDraw()) statDraws++;

    toggleTurn();
    return true;
}

bool Board::makeAutoMove() {
    std::vector<int> emptyPos;
    for (int i = 0; i < 9; ++i) if (cells[i] == EMPTY) emptyPos.push_back(i);
    if (emptyPos.empty()) return false;
    static std::mt19937 rng((unsigned)std::random_device{}());
    std::uniform_int_distribution<int> dist(0, (int)emptyPos.size() - 1);
    int pick = emptyPos[dist(rng)];
    cells[pick] = static_cast<Cell>(currentTurn);

    char winner = checkWin();
    if (winner == 'X') statXWins++;
    else if (winner == 'O') statOWins++;
    else if (checkDraw()) statDraws++;

    toggleTurn();
    return true;
}

char Board::checkWin() const {
    const int LINES[8][3] = {
        {0,1,2},{3,4,5},{6,7,8}, // rows
        {0,3,6},{1,4,7},{2,5,8}, // cols
        {0,4,8},{2,4,6}          // diags
    };
    for (auto &line : LINES) {
        char a = cells[line[0]];
        char b = cells[line[1]];
        char c = cells[line[2]];
        if (a != EMPTY && a == b && b == c) return a;
    }
    return EMPTY;
}

bool Board::checkDraw() const {
    if (checkWin() != EMPTY) return false;
    for (auto &c : cells) if (c == EMPTY) return false;
    return true;
}

void Board::reset() {
    cells.fill(EMPTY);
    currentTurn = 'X';
    // keep statistics intact across resets (per spec)
    statXWins = statOWins = statDraws = 0;
}

char Board::cellAt(int pos) const { return cells[pos]; }

void Board::toggleTurn() { currentTurn = (currentTurn == 'X') ? 'O' : 'X'; }
