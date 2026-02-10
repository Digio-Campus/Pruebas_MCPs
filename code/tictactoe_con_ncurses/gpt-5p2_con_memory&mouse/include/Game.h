#pragma once

#include <random>
#include <string>
#include <vector>
#include "Board.h"
#include "Rect.h"
#include "Settings.h"

class UI;

class Game {
public:
    explicit Game(const Settings& settings);

    // Devuelve al menú cuando el usuario sale.
    void run(UI& ui, const Settings& settings);

private:
    void ensureBoards_(int n);
    void handleMouse_(int mx, int my, const std::vector<Rect>& rects, const Settings& settings);
    void tryHumanMove_(int boardIdx, int row, int col, const Settings& settings);
    void tickAuto_(const Settings& settings);

    static bool coordToCell_(const Rect& boardRect, int x, int y, int& outRow, int& outCol);

    std::vector<Board> m_boards;
    std::vector<int> m_finishTicks; // solo usado en modo 0 (auto-reset)

    int m_selectedBoard{0};
    int m_cursorRow{0};
    int m_cursorCol{0};

    std::mt19937 m_rng;
};
