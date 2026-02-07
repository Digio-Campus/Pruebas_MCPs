#pragma once

#include <array>
#include <cstddef>
#include <optional>
#include <random>

// Lógica de un tablero de Tres en Raya (3x3) con estadísticas.
class Board {
public:
  enum class Cell : char { Empty = ' ', X = 'X', O = 'O' };
  enum class Result { Ongoing, XWin, OWin, Draw };

  Board();

  void reset();

  // Intenta colocar la ficha del turno actual en (r,c). Devuelve true si se pudo.
  bool place(std::size_t r, std::size_t c);

  // Jugada aleatoria válida para el turno actual. Devuelve false si no hay movimientos.
  bool randomMove();

  Result result() const { return result_; }
  Cell turn() const { return turn_; }

  Cell at(std::size_t r, std::size_t c) const { return grid_[r][c]; }

  // Estadísticas acumuladas.
  int xWins() const { return xWins_; }
  int oWins() const { return oWins_; }
  int draws() const { return draws_; }

  // Recalcula y actualiza el resultado actual.
  void updateResult();

private:
  std::array<std::array<Cell, 3>, 3> grid_{};
  Cell turn_ = Cell::X;
  Result result_ = Result::Ongoing;

  int xWins_ = 0;
  int oWins_ = 0;
  int draws_ = 0;

  std::mt19937 rng_;

  bool isFull() const;
  std::optional<Cell> winner() const;

  void finishIfNeeded();
  void advanceTurn();
};
