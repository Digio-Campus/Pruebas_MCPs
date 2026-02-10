#include "Board.h"

#include <algorithm>
#include <chrono>
#include <vector>

Board::Board() {
  auto seed = static_cast<unsigned long>(
      std::chrono::high_resolution_clock::now().time_since_epoch().count());
  rng_.seed(seed);
  reset();
}

void Board::reset() {
  for (auto &row : grid_) {
    for (auto &c : row) c = Cell::Empty;
  }
  turn_ = Cell::X;
  result_ = Result::Ongoing;
}

bool Board::place(std::size_t r, std::size_t c) {
  if (r >= 3 || c >= 3) return false;
  if (result_ != Result::Ongoing) return false;
  if (grid_[r][c] != Cell::Empty) return false;

  grid_[r][c] = turn_;
  finishIfNeeded();
  if (result_ == Result::Ongoing) advanceTurn();
  return true;
}

bool Board::randomMove() {
  if (result_ != Result::Ongoing) return false;

  std::vector<std::pair<std::size_t, std::size_t>> free;
  free.reserve(9);
  for (std::size_t r = 0; r < 3; ++r) {
    for (std::size_t c = 0; c < 3; ++c) {
      if (grid_[r][c] == Cell::Empty) free.emplace_back(r, c);
    }
  }
  if (free.empty()) return false;

  std::shuffle(free.begin(), free.end(), rng_);
  return place(free[0].first, free[0].second);
}

void Board::updateResult() { finishIfNeeded(); }

bool Board::isFull() const {
  for (const auto &row : grid_) {
    for (auto c : row) {
      if (c == Cell::Empty) return false;
    }
  }
  return true;
}

std::optional<Board::Cell> Board::winner() const {
  auto line = [&](Cell a, Cell b, Cell c) -> std::optional<Cell> {
    if (a == Cell::Empty) return std::nullopt;
    if (a == b && b == c) return a;
    return std::nullopt;
  };

  for (std::size_t r = 0; r < 3; ++r) {
    if (auto w = line(grid_[r][0], grid_[r][1], grid_[r][2])) return w;
  }
  for (std::size_t c = 0; c < 3; ++c) {
    if (auto w = line(grid_[0][c], grid_[1][c], grid_[2][c])) return w;
  }
  if (auto w = line(grid_[0][0], grid_[1][1], grid_[2][2])) return w;
  if (auto w = line(grid_[0][2], grid_[1][1], grid_[2][0])) return w;

  return std::nullopt;
}

void Board::finishIfNeeded() {
  if (result_ != Result::Ongoing) return;

  if (auto w = winner()) {
    if (*w == Cell::X) {
      result_ = Result::XWin;
      ++xWins_;
    } else {
      result_ = Result::OWin;
      ++oWins_;
    }
    return;
  }

  if (isFull()) {
    result_ = Result::Draw;
    ++draws_;
  }
}

void Board::advanceTurn() {
  // Alternancia estricta X <-> O.
  turn_ = (turn_ == Cell::X) ? Cell::O : Cell::X;
}
