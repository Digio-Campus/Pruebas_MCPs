#include "Board.h"
#include <algorithm>

namespace ttt {

Board::Board() 
    : grid_(3, std::vector<Cell>(3, Cell::Empty)),
      currentTurn_(Cell::X),
      result_(Result::Ongoing),
      lastRecordedResult_(Result::Ongoing) {
    std::random_device rd;
    rng_.seed(rd());
}

bool Board::makeMove(int row, int col, Cell player) {
    if (row < 0 || row >= 3 || col < 0 || col >= 3) {
        return false;
    }
    if (grid_[row][col] != Cell::Empty) {
        return false;
    }
    if (player != currentTurn_ || result_ != Result::Ongoing) {
        return false;
    }
    
    grid_[row][col] = player;
    
    // Update statistics if game result changed
    Result prevResult = result_;
    updateResult();
    
    if (result_ != prevResult && result_ != Result::Ongoing) {
        if (lastRecordedResult_ != result_) {
            if (result_ == Result::X_Win) {
                stats_.xWins++;
            } else if (result_ == Result::O_Win) {
                stats_.oWins++;
            } else if (result_ == Result::Draw) {
                stats_.draws++;
            }
            lastRecordedResult_ = result_;
        }
    }
    
    // Alternate turn only if game is ongoing
    if (result_ == Result::Ongoing) {
        currentTurn_ = (currentTurn_ == Cell::X) ? Cell::O : Cell::X;
    }
    
    return true;
}

bool Board::makeAutoMove() {
    if (result_ != Result::Ongoing) {
        return false;
    }
    
    auto moves = availableMoves();
    if (moves.empty()) {
        return false;
    }
    
    std::uniform_int_distribution<> dis(0, moves.size() - 1);
    int idx = dis(rng_);
    auto [row, col] = moves[idx];
    
    return makeMove(row, col, currentTurn_);
}

void Board::reset() {
    for (auto& row : grid_) {
        std::fill(row.begin(), row.end(), Cell::Empty);
    }
    currentTurn_ = Cell::X;
    result_ = Result::Ongoing;
    lastRecordedResult_ = Result::Ongoing;
}

Cell Board::getCell(int row, int col) const {
    if (row < 0 || row >= 3 || col < 0 || col >= 3) {
        return Cell::Empty;
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

std::vector<std::pair<int, int>> Board::availableMoves() const {
    std::vector<std::pair<int, int>> moves;
    for (int row = 0; row < 3; ++row) {
        for (int col = 0; col < 3; ++col) {
            if (grid_[row][col] == Cell::Empty) {
                moves.emplace_back(row, col);
            }
        }
    }
    return moves;
}

bool Board::isFull() const {
    for (const auto& row : grid_) {
        for (const auto cell : row) {
            if (cell == Cell::Empty) {
                return false;
            }
        }
    }
    return true;
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

bool Board::checkWin(Cell player) {
    // Check rows
    for (int row = 0; row < 3; ++row) {
        if (grid_[row][0] == player && grid_[row][1] == player && grid_[row][2] == player) {
            return true;
        }
    }
    
    // Check columns
    for (int col = 0; col < 3; ++col) {
        if (grid_[0][col] == player && grid_[1][col] == player && grid_[2][col] == player) {
            return true;
        }
    }
    
    // Check diagonals
    if (grid_[0][0] == player && grid_[1][1] == player && grid_[2][2] == player) {
        return true;
    }
    if (grid_[0][2] == player && grid_[1][1] == player && grid_[2][0] == player) {
        return true;
    }
    
    return false;
}

bool Board::checkDraw() {
    if (result_ != Result::Ongoing) {
        return false;
    }
    return isFull();
}

}  // namespace ttt
