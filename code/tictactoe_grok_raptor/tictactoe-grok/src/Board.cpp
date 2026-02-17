#include "Board.h"
#include <algorithm>

namespace ttt {

Board::Board()
    : grid_(3, std::vector<Cell>(3, Cell::Empty))
    , currentTurn_(Cell::X)
    , result_(Result::Ongoing)
    , lastRecordedResult_(std::nullopt)
    , rng_(std::random_device{}())
{
}

Cell Board::getCell(int row, int col) const {
    if (row < 0 || row >= 3 || col < 0 || col >= 3) {
        return Cell::Empty; // Invalid, but shouldn't happen
    }
    return grid_[row][col];
}

Cell Board::getCurrentTurn() const {
    return currentTurn_;
}

Result Board::getResult() const {
    return result_;
}

const BoardStats& Board::getStats() const {
    return stats_;
}

std::vector<std::pair<int, int>> Board::getAvailableMoves() const {
    std::vector<std::pair<int, int>> moves;
    for (int i = 0; i < 3; ++i) {
        for (int j = 0; j < 3; ++j) {
            if (grid_[i][j] == Cell::Empty) {
                moves.emplace_back(i, j);
            }
        }
    }
    return moves;
}

bool Board::makeMove(int row, int col) {
    if (row < 0 || row >= 3 || col < 0 || col >= 3 ||
        grid_[row][col] != Cell::Empty || result_ != Result::Ongoing) {
        return false;
    }
    grid_[row][col] = currentTurn_;
    updateResult();
    if (result_ != Result::Ongoing) {
        recordResult();
    } else {
        // Switch turn only if ongoing
        currentTurn_ = (currentTurn_ == Cell::X) ? Cell::O : Cell::X;
    }
    return true;
}

void Board::makeAutoMove() {
    auto moves = getAvailableMoves();
    if (moves.empty()) return;
    std::uniform_int_distribution<size_t> dist(0, moves.size() - 1);
    auto [row, col] = moves[dist(rng_)];
    makeMove(row, col);
}

void Board::reset() {
    for (auto& row : grid_) {
        std::fill(row.begin(), row.end(), Cell::Empty);
    }
    currentTurn_ = Cell::X;
    result_ = Result::Ongoing;
    lastRecordedResult_ = std::nullopt;
}

void Board::updateResult() {
    if (checkWin(Cell::X)) {
        result_ = Result::X_Win;
    } else if (checkWin(Cell::O)) {
        result_ = Result::O_Win;
    } else if (checkDraw()) {
        result_ = Result::Draw;
    } else {
        result_ = Result::Ongoing;
    }
}

bool Board::checkWin(Cell player) const {
    // Rows
    for (int i = 0; i < 3; ++i) {
        if (grid_[i][0] == player && grid_[i][1] == player && grid_[i][2] == player) {
            return true;
        }
    }
    // Columns
    for (int j = 0; j < 3; ++j) {
        if (grid_[0][j] == player && grid_[1][j] == player && grid_[2][j] == player) {
            return true;
        }
    }
    // Diagonals
    if (grid_[0][0] == player && grid_[1][1] == player && grid_[2][2] == player) {
        return true;
    }
    if (grid_[0][2] == player && grid_[1][1] == player && grid_[2][0] == player) {
        return true;
    }
    return false;
}

bool Board::checkDraw() const {
    for (const auto& row : grid_) {
        for (Cell cell : row) {
            if (cell == Cell::Empty) {
                return false;
            }
        }
    }
    return true;
}

void Board::recordResult() {
    if (!lastRecordedResult_ || *lastRecordedResult_ != result_) {
        switch (result_) {
            case Result::X_Win: ++stats_.xWins; break;
            case Result::O_Win: ++stats_.oWins; break;
            case Result::Draw: ++stats_.draws; break;
            default: break;
        }
        lastRecordedResult_ = result_;
    }
}

} // namespace ttt