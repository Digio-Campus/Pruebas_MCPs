#include "../include/Board.h"

#include <algorithm>

namespace ttt {

Board::Board(int id) : turn_(Cell::X), result_(Result::Ongoing), moves_(0), id_(id) {
    reset();
}

void Board::reset() {
    for (auto &r : grid_) r.fill(Cell::Empty);
    turn_ = Cell::X;
    result_ = Result::Ongoing;
    moves_ = 0;
}

bool Board::makeMove(int row, int col) {
    if (row < 0 || row > 2 || col < 0 || col > 2) return false;
    if (result_ != Result::Ongoing) return false;
    if (grid_[row][col] != Cell::Empty) return false;

    grid_[row][col] = turn_;
    ++moves_;
    updateResult();
    if (result_ == Result::Ongoing) switchTurn();
    return true;
}

bool Board::makeMoveForced(int row, int col, Cell who) {
    if (row < 0 || row > 2 || col < 0 || col > 2) return false;
    if (result_ != Result::Ongoing) return false;
    if (grid_[row][col] != Cell::Empty) return false;

    grid_[row][col] = who;
    ++moves_;
    updateResult();
    if (result_ == Result::Ongoing) turn_ = (who == Cell::X ? Cell::O : Cell::X);
    return true;
}

std::vector<std::pair<int,int>> Board::availableMoves() const {
    std::vector<std::pair<int,int>> v;
    for (int r = 0; r < 3; ++r)
        for (int c = 0; c < 3; ++c)
            if (grid_[r][c] == Cell::Empty) v.emplace_back(r, c);
    return v;
}

Result Board::result() const { return result_; }
Cell Board::cellAt(int row, int col) const { return grid_[row][col]; }
Cell Board::currentTurn() const { return turn_; }
bool Board::isFull() const { return moves_ >= 9; }
int Board::movesMade() const { return moves_; }
int Board::id() const { return id_; }
void Board::setId(int id) { id_ = id; }

void Board::updateResult() {
    // filas
    for (int r = 0; r < 3; ++r) {
        if (grid_[r][0] != Cell::Empty && grid_[r][0] == grid_[r][1] && grid_[r][1] == grid_[r][2]) {
            result_ = (grid_[r][0] == Cell::X ? Result::X_Win : Result::O_Win);
            return;
        }
    }
    // columnas
    for (int c = 0; c < 3; ++c) {
        if (grid_[0][c] != Cell::Empty && grid_[0][c] == grid_[1][c] && grid_[1][c] == grid_[2][c]) {
            result_ = (grid_[0][c] == Cell::X ? Result::X_Win : Result::O_Win);
            return;
        }
    }
    // diagonales
    if (grid_[0][0] != Cell::Empty && grid_[0][0] == grid_[1][1] && grid_[1][1] == grid_[2][2]) {
        result_ = (grid_[0][0] == Cell::X ? Result::X_Win : Result::O_Win);
        return;
    }
    if (grid_[0][2] != Cell::Empty && grid_[0][2] == grid_[1][1] && grid_[1][1] == grid_[2][0]) {
        result_ = (grid_[0][2] == Cell::X ? Result::X_Win : Result::O_Win);
        return;
    }
    if (moves_ >= 9) {
        result_ = Result::Draw;
        return;
    }
    result_ = Result::Ongoing;
}

void Board::switchTurn() { turn_ = (turn_ == Cell::X ? Cell::O : Cell::X); }

} // namespace ttt
