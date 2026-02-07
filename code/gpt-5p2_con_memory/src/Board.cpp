#include "Board.h"

Board::Board() {
  reset();
}

void Board::reset() {
  cells_.fill(Cell::Empty);
  turn_ = Cell::X;
  result_ = Result::InProgress;
}

Board::Cell Board::at(int r, int c) const {
  return cells_[idx(r, c)];
}

bool Board::place(int r, int c) {
  if (result_ != Result::InProgress) return false;
  if (r < 0 || r > 2 || c < 0 || c > 2) return false;

  int i = idx(r, c);
  if (cells_[i] != Cell::Empty) return false;

  cells_[i] = turn_;

  Result prev = result_;
  result_ = computeResult();
  updateResultAndStats(prev);

  // Alternancia estricta por tablero.
  if (result_ == Result::InProgress) {
    turn_ = (turn_ == Cell::X) ? Cell::O : Cell::X;
  }

  return true;
}

bool Board::placeRandom(std::mt19937& rng) {
  if (result_ != Result::InProgress) return false;

  auto empties = emptyIndices();
  if (empties.empty()) return false;

  std::uniform_int_distribution<size_t> dist(0, empties.size() - 1);
  int choice = empties[dist(rng)];

  int r = choice / 3;
  int c = choice % 3;
  return place(r, c);
}

bool Board::isFull() const {
  for (auto c : cells_) {
    if (c == Cell::Empty) return false;
  }
  return true;
}

std::vector<int> Board::emptyIndices() const {
  std::vector<int> out;
  out.reserve(9);
  for (int i = 0; i < 9; ++i) {
    if (cells_[i] == Cell::Empty) out.push_back(i);
  }
  return out;
}

static bool line3(Board::Cell a, Board::Cell b, Board::Cell c, Board::Cell who) {
  return a == who && b == who && c == who;
}

Board::Result Board::computeResult() const {
  // Filas
  for (int r = 0; r < 3; ++r) {
    if (line3(at(r,0), at(r,1), at(r,2), Cell::X)) return Result::WinX;
    if (line3(at(r,0), at(r,1), at(r,2), Cell::O)) return Result::WinO;
  }
  // Columnas
  for (int c = 0; c < 3; ++c) {
    if (line3(at(0,c), at(1,c), at(2,c), Cell::X)) return Result::WinX;
    if (line3(at(0,c), at(1,c), at(2,c), Cell::O)) return Result::WinO;
  }
  // Diagonales
  if (line3(at(0,0), at(1,1), at(2,2), Cell::X)) return Result::WinX;
  if (line3(at(0,0), at(1,1), at(2,2), Cell::O)) return Result::WinO;
  if (line3(at(0,2), at(1,1), at(2,0), Cell::X)) return Result::WinX;
  if (line3(at(0,2), at(1,1), at(2,0), Cell::O)) return Result::WinO;

  if (isFull()) return Result::Draw;
  return Result::InProgress;
}

void Board::updateResultAndStats(Result prev) {
  if (prev != Result::InProgress) return;

  if (result_ == Result::WinX) ++xWins_;
  else if (result_ == Result::WinO) ++oWins_;
  else if (result_ == Result::Draw) ++draws_;
}
