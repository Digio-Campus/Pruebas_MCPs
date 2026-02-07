#include "Board.h"

#include <algorithm>
#include <sstream>

Board::Board() { reset(); }

void Board::reset() {
  cells_.fill(' ');
  turn_ = 'X';
  outcome_ = Outcome::InProgress;
  statsCounted_ = false;
}

bool Board::placeAt(int idx) {
  if (idx < 0 || idx >= static_cast<int>(cells_.size())) return false;
  if (outcome_ != Outcome::InProgress) return false;
  if (cells_[idx] != ' ') return false;

  cells_[idx] = turn_;
  updateOutcomeAndStatsIfFinished();
  if (outcome_ == Outcome::InProgress) advanceTurn();
  return true;
}

bool Board::randomMove(std::mt19937 &rng) {
  if (outcome_ != Outcome::InProgress) return false;

  std::array<int, 9> empties{};
  int n = 0;
  for (int i = 0; i < 9; ++i) {
    if (cells_[i] == ' ') empties[n++] = i;
  }
  if (n == 0) return false;

  std::uniform_int_distribution<int> dist(0, n - 1);
  return placeAt(empties[dist(rng)]);
}

void Board::advanceTurn() { turn_ = (turn_ == 'X') ? 'O' : 'X'; }

Board::Outcome Board::evaluate(const std::array<char, 9> &c) {
  static const int lines[8][3] = {
      {0, 1, 2}, {3, 4, 5}, {6, 7, 8}, // filas
      {0, 3, 6}, {1, 4, 7}, {2, 5, 8}, // columnas
      {0, 4, 8}, {2, 4, 6}             // diagonales
  };

  for (const auto &ln : lines) {
    const char a = c[ln[0]];
    if (a != ' ' && a == c[ln[1]] && a == c[ln[2]]) {
      return (a == 'X') ? Outcome::XWin : Outcome::OWin;
    }
  }

  const bool full = std::all_of(c.begin(), c.end(), [](char ch) { return ch != ' '; });
  return full ? Outcome::Draw : Outcome::InProgress;
}

void Board::updateOutcomeAndStatsIfFinished() {
  outcome_ = evaluate(cells_);
  if (outcome_ == Outcome::InProgress) return;
  if (statsCounted_) return;

  switch (outcome_) {
  case Outcome::XWin:
    ++xWins_;
    break;
  case Outcome::OWin:
    ++oWins_;
    break;
  case Outcome::Draw:
    ++draws_;
    break;
  case Outcome::InProgress:
    break;
  }

  statsCounted_ = true;
}

std::string Board::outcomeText() const {
  switch (outcome_) {
  case Outcome::InProgress:
    return std::string("Turno: ") + turn_;
  case Outcome::XWin:
    return "Gana X";
  case Outcome::OWin:
    return "Gana O";
  case Outcome::Draw:
    return "Empate";
  }
  return "";
}
