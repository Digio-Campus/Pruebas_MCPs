#pragma once

namespace ttt {

class Settings {
public:
    Settings();

    void setNumPlayers(int p);
    int numPlayers() const { return numPlayers_; }

    void setNumBoards(int b);
    int numBoards() const { return numBoards_; }

    bool isValid() const;

private:
    int numPlayers_; // 0..2
    int numBoards_;   // 1..9
};

} // namespace ttt
