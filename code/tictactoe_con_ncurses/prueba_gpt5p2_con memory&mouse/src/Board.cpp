#include "Board.h"

#include <algorithm>

Board::Board() {
  reset();
}

void Board::reset() {
  for (auto& row : grid_) {
    row.fill(' ');
  }
  turn_ = 'X';
  result_ = Result::InProgress;
  winningLine_.reset();
}

char Board::cell(int r, int c) const {
  return grid_.at(static_cast<size_t>(r)).at(static_cast<size_t>(c));
}

bool Board::isCellEmpty(int r, int c) const {
  return cell(r, c) == ' ';
}

std::vector<std::pair<int, int>> Board::emptyCells() const {
  std::vector<std::pair<int, int>> out;
  out.reserve(9);
  for (int r = 0; r < 3; ++r) {
    for (int c = 0; c < 3; ++c) {
      if (cell(r, c) == ' ') out.emplace_back(r, c);
    }
  }
  return out;
}

bool Board::makeMove(int r, int c) {
  if (result_ != Result::InProgress) return false;
  if (r < 0 || r >= 3 || c < 0 || c >= 3) return false;
  if (!isCellEmpty(r, c)) return false;

  const char placed = turn_;
  grid_[static_cast<size_t>(r)][static_cast<size_t>(c)] = placed;

  updateResultAfterMove(placed);
  if (result_ == Result::InProgress) {
    turn_ = (turn_ == 'X') ? 'O' : 'X';
  }
  return true;
}

bool Board::makeRandomMove(std::mt19937& rng) {
  if (result_ != Result::InProgress) return false;

  auto empties = emptyCells();
  if (empties.empty()) return false;

  std::uniform_int_distribution<size_t> dist(0, empties.size() - 1);
  const auto [r, c] = empties[dist(rng)];
  return makeMove(r, c);
}

void Board::updateResultAfterMove(char placed) {
  auto winFor = [&](std::array<std::pair<int, int>, 3> line) {
    for (const auto& [r, c] : line) {
      if (cell(r, c) != placed) return false;
    }
    winningLine_ = line;
    return true;
  };

  const std::array<std::array<std::pair<int, int>, 3>, 8> lines = {
      std::array<std::pair<int, int>, 3>{{{0, 0}, {0, 1}, {0, 2}}},
      std::array<std::pair<int, int>, 3>{{{1, 0}, {1, 1}, {1, 2}}},
      std::array<std::pair<int, int>, 3>{{{2, 0}, {2, 1}, {2, 2}}},
      std::array<std::pair<int, int>, 3>{{{0, 0}, {1, 0}, {2, 0}}},
      std::array<std::pair<int, int>, 3>{{{0, 1}, {1, 1}, {2, 1}}},
      std::array<std::pair<int, int>, 3>{{{0, 2}, {1, 2}, {2, 2}}},
      std::array<std::pair<int, int>, 3>{{{0, 0}, {1, 1}, {2, 2}}},
      std::array<std::pair<int, int>, 3>{{{0, 2}, {1, 1}, {2, 0}}},
  };

  for (const auto& line : lines) {
    if (winFor(line)) {
      if (placed == 'X') {
        result_ = Result::XWins;
        ++xWins_;
      } else {
        result_ = Result::OWins;
        ++oWins_;
      }
      return;
    }
  }

  // Empate si no hay celdas vacías.
  for (int r = 0; r < 3; ++r) {
    for (int c = 0; c < 3; ++c) {
      if (cell(r, c) == ' ') return;
    }
  }

  result_ = Result::Draw;
  ++draws_;
}
