#pragma once

class Settings {
public:
    // numPlayers:
    // 0 -> auto (X y O aleatorias)
    // 1 -> un jugador controla X y O manualmente
    // 2 -> jugador controla O, X automática
    int numPlayers() const { return m_numPlayers; }
    int numBoards() const { return m_numBoards; }

    void setNumPlayers(int v);
    void setNumBoards(int v);

private:
    int m_numPlayers{2};
    int m_numBoards{1};
};
