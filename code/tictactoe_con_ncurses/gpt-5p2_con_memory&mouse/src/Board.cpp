#include "Board.h"

Board::Board() {
    resetRound();
}

void Board::resetRound() {
    for (auto& row : m_grid) {
        row.fill(' ');
    }
    m_nextTurn = 'X';
    m_result = BoardResult::InProgress;
}

static BoardResult computeResult(const std::array<std::array<char, 3>, 3>& g) {
    auto win = [&](char m) {
        for (int r = 0; r < 3; ++r)
            if (g[r][0] == m && g[r][1] == m && g[r][2] == m) return true;
        for (int c = 0; c < 3; ++c)
            if (g[0][c] == m && g[1][c] == m && g[2][c] == m) return true;
        if (g[0][0] == m && g[1][1] == m && g[2][2] == m) return true;
        if (g[0][2] == m && g[1][1] == m && g[2][0] == m) return true;
        return false;
    };

    if (win('X')) return BoardResult::XWin;
    if (win('O')) return BoardResult::OWin;

    bool anyEmpty = false;
    for (int r = 0; r < 3; ++r)
        for (int c = 0; c < 3; ++c)
            if (g[r][c] == ' ') anyEmpty = true;

    return anyEmpty ? BoardResult::InProgress : BoardResult::Draw;
}

void Board::updateResultAndStats_() {
    BoardResult prev = m_result;
    m_result = computeResult(m_grid);

    // Estadísticas: contar una sola vez cuando pasa de InProgress a final.
    if (prev == BoardResult::InProgress && m_result != BoardResult::InProgress) {
        if (m_result == BoardResult::XWin) ++m_xWins;
        else if (m_result == BoardResult::OWin) ++m_oWins;
        else if (m_result == BoardResult::Draw) ++m_draws;
    }
}

bool Board::place(int row, int col) {
    if (row < 0 || row >= 3 || col < 0 || col >= 3) return false;
    if (m_result != BoardResult::InProgress) return false;
    if (m_grid[row][col] != ' ') return false;

    m_grid[row][col] = m_nextTurn;
    updateResultAndStats_();

    if (m_result == BoardResult::InProgress) {
        m_nextTurn = (m_nextTurn == 'X') ? 'O' : 'X';
    }

    return true;
}

bool Board::autoMove(std::mt19937& rng) {
    if (m_result != BoardResult::InProgress) return false;

    int empties[9];
    int n = 0;
    for (int r = 0; r < 3; ++r) {
        for (int c = 0; c < 3; ++c) {
            if (m_grid[r][c] == ' ') {
                empties[n++] = r * 3 + c;
            }
        }
    }
    if (n == 0) return false;

    std::uniform_int_distribution<int> dist(0, n - 1);
    int idx = empties[dist(rng)];
    return place(idx / 3, idx % 3);
}
