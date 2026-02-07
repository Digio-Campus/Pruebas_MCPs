#include "Game.h"

#include <algorithm>

#include <ncurses.h>

Game::Game(const Settings& s)
    : settings_(s),
      rng_(std::random_device{}()) {
  settings_.clamp();
  ensureBoardCount();
}

void Game::ensureBoardCount() {
  settings_.clamp();
  boards_.resize(settings_.boardsCount);
  cursors_.resize(settings_.boardsCount, {0, 0});
  selectedBoard_ = std::clamp(selectedBoard_, 0, static_cast<int>(boards_.size()) - 1);
}

bool Game::canHumanPlay(const Board& b) const {
  if (b.result() != Board::Result::InProgress) return false;

  switch (settings_.playersMode) {
    case PlayersMode::Zero: return false;
    case PlayersMode::One:  return true;
    case PlayersMode::Two:  return b.currentTurn() == Board::Cell::O;
  }
  return false;
}

Board::Cell Game::humanPieceFor(const Board& b) const {
  // En modo 1, el humano juega lo que toque (X u O).
  // En modo 2, el humano solo juega O.
  if (settings_.playersMode == PlayersMode::Two) return Board::Cell::O;
  return b.currentTurn();
}

void Game::applyAutoMoves() {
  if (settings_.playersMode == PlayersMode::Zero) {
    if (autoPaused_) return;
    for (auto& b : boards_) {
      b.placeRandom(rng_);
    }
    return;
  }

  if (settings_.playersMode == PlayersMode::Two) {
    // Siempre que sea turno de X, juega la X automticamente (por tablero).
    for (auto& b : boards_) {
      if (b.result() == Board::Result::InProgress && b.currentTurn() == Board::Cell::X) {
        b.placeRandom(rng_);
      }
    }
  }
}

void Game::resetBoard(int i) {
  if (i < 0 || i >= static_cast<int>(boards_.size())) return;
  boards_[i].reset();
  cursors_[i] = {0, 0};
}

void Game::resetAllBoards() {
  for (int i = 0; i < static_cast<int>(boards_.size()); ++i) resetBoard(i);
}

void Game::run(const UI& ui) {
  // En modo auto, no bloquea; en manual, bloquea esperando tecla.
  while (true) {
    ensureBoardCount();
    int maxFit = ui.maxBoardsFit();
    if (settings_.boardsCount > maxFit) {
      settings_.boardsCount = maxFit;
      ensureBoardCount();
    }

    ui.drawGame(settings_, boards_, cursors_, selectedBoard_, autoPaused_, maxFit);

    if (settings_.playersMode == PlayersMode::Zero || settings_.playersMode == PlayersMode::Two) {
      timeout(120);
    } else {
      timeout(-1);
    }

    int ch = getch();

    if (ch == KEY_RESIZE) {
      // fuerza recalculo/redibujo
      continue;
    }

    if (ch == 'q' || ch == 'Q' || ch == 27) {
      timeout(-1);
      return;
    }

    // Controles globales
    if (ch == 'r') resetBoard(selectedBoard_);
    if (ch == 'R') resetAllBoards();
    if (ch == 'p' || ch == 'P') {
      if (settings_.playersMode == PlayersMode::Zero) autoPaused_ = !autoPaused_;
    }

    // Cambio de tablero
    if (ch == '\t' || ch == KEY_STAB || ch == KEY_CTAB) {
      selectedBoard_ = (selectedBoard_ + 1) % static_cast<int>(boards_.size());
    } else if (ch == KEY_BTAB) {
      selectedBoard_ = (selectedBoard_ - 1 + static_cast<int>(boards_.size())) % static_cast<int>(boards_.size());
    }

    // Movimiento del cursor dentro del tablero
    auto& cur = cursors_[selectedBoard_];
    if (ch == KEY_UP) cur.first = std::max(0, cur.first - 1);
    else if (ch == KEY_DOWN) cur.first = std::min(2, cur.first + 1);
    else if (ch == KEY_LEFT) cur.second = std::max(0, cur.second - 1);
    else if (ch == KEY_RIGHT) cur.second = std::min(2, cur.second + 1);

    // Jugada humana
    if ((ch == 10 || ch == KEY_ENTER || ch == ' ') && canHumanPlay(boards_[selectedBoard_])) {
      int r = cur.first;
      int c = cur.second;

      // En modo 2, solo se permite O; el Board ya aplica alternancia estricta,
      // así que solo validamos que sea el turno correcto.
      if (settings_.playersMode == PlayersMode::Two && boards_[selectedBoard_].currentTurn() != Board::Cell::O) {
        // nada
      } else {
        boards_[selectedBoard_].place(r, c);
      }
    }

    // Auto-movimientos (tras input o tick)
    applyAutoMoves();
  }
}
