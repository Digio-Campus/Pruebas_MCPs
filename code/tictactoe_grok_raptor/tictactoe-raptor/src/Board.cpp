#include "../include/Board.h"
#include <algorithm>
#include <chrono>

using namespace ttt;

Board::Board()
    : cells_{}
    , turn_(Cell::X)
    , result_(Result::Ongoing)
    , stats_{}
    , rng_(std::random_device{}())
{
    cells_.fill(Cell::Empty);
}

bool Board::makeMove(int row, int col)
{
    if (row < 0 || row > 2 || col < 0 || col > 2) return false;
    if (result_ != Result::Ongoing) return false;
    int idx = row * 3 + col;
    if (cells_[idx] != Cell::Empty) return false;

    cells_[idx] = turn_;
    updateResult();
    if (result_ == Result::Ongoing) {
        // toggle turn
        turn_ = (turn_ == Cell::X) ? Cell::O : Cell::X;
    } else {
        // record stats and keep turn as-is until reset
        if (result_ == Result::X_Win) stats_.xWins++;
        else if (result_ == Result::O_Win) stats_.oWins++;
        else if (result_ == Result::Draw) stats_.draws++;
    }
    return true;
}

bool Board::makeAutoMove()
{
    if (result_ != Result::Ongoing) return false;
    auto moves = availableMoves();
    if (moves.empty()) {
        updateResult();
        return false;
    }
    std::uniform_int_distribution<int> dist(0, (int)moves.size() - 1);
    int pick = moves[dist(rng_)];
    cells_[pick] = turn_;
    updateResult();
    if (result_ == Result::Ongoing) turn_ = (turn_ == Cell::X) ? Cell::O : Cell::X;
    else {
        if (result_ == Result::X_Win) stats_.xWins++;
        else if (result_ == Result::O_Win) stats_.oWins++;
        else if (result_ == Result::Draw) stats_.draws++;
    }
    return true;
}

void Board::reset()
{
    cells_.fill(Cell::Empty);
    turn_ = Cell::X; // always start with X
    result_ = Result::Ongoing;
}

std::vector<int> Board::availableMoves() const
{
    std::vector<int> out;
    for (int i = 0; i < 9; ++i) if (cells_[i] == Cell::Empty) out.push_back(i);
    return out;
}

Cell Board::cellAt(int row, int col) const
{
    int idx = row * 3 + col;
    return cells_[idx];
}

Result Board::checkWinInternal() const
{
    // rows
    auto checkLine = [&](int a, int b, int c) -> std::optional<Result> {
        if (cells_[a] == Cell::Empty) return std::nullopt;
        if (cells_[a] == cells_[b] && cells_[b] == cells_[c]) {
            return (cells_[a] == Cell::X) ? Result::X_Win : Result::O_Win;
        }
        return std::nullopt;
    };
    const int lines[8][3] = {{0,1,2},{3,4,5},{6,7,8},{0,3,6},{1,4,7},{2,5,8},{0,4,8},{2,4,6}};
    for (auto &ln : lines) {
        if (auto r = checkLine(ln[0], ln[1], ln[2])) return *r;
    }
    // draw?
    bool anyEmpty = false;
    for (auto &c : cells_) if (c == Cell::Empty) { anyEmpty = true; break; }
    if (!anyEmpty) return Result::Draw;
    return Result::Ongoing;
}

void Board::updateResult()
{
    result_ = checkWinInternal();
}
