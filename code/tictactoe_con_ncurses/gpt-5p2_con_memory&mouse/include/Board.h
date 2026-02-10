#pragma once

#include <array>
#include <random>

enum class BoardResult {
    InProgress,
    XWin,
    OWin,
    Draw,
};

class Board {
public:
    Board();

    void resetRound();

    BoardResult result() const { return m_result; }
    bool isFinished() const { return m_result != BoardResult::InProgress; }

    char nextTurn() const { return m_nextTurn; }

    // Coloca la marca del turno actual. Devuelve true si se pudo colocar.
    bool place(int row, int col);

    // Juega una jugada aleatoria para el turno actual. Devuelve true si movió.
    bool autoMove(std::mt19937& rng);

    char cell(int row, int col) const { return m_grid[row][col]; }

    int xWins() const { return m_xWins; }
    int oWins() const { return m_oWins; }
    int draws() const { return m_draws; }

private:
    void updateResultAndStats_();

    std::array<std::array<char, 3>, 3> m_grid{}; // 'X', 'O' o ' '
    char m_nextTurn{'X'};
    BoardResult m_result{BoardResult::InProgress};

    int m_xWins{0};
    int m_oWins{0};
    int m_draws{0};
};
