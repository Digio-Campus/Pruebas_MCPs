#include "Settings.h"

static int clampInt(int v, int lo, int hi) {
    if (v < lo) return lo;
    if (v > hi) return hi;
    return v;
}

void Settings::setNumPlayers(int v) {
    m_numPlayers = clampInt(v, 0, 2);
}

void Settings::setNumBoards(int v) {
    m_numBoards = clampInt(v, 1, 9);
}
