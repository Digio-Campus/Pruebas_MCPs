#ifndef SETTINGS_H
#define SETTINGS_H

namespace ttt {

class Settings {
public:
    Settings() = default;
    ~Settings() = default;

    int getNumPlayers() const { return numPlayers_; }
    int getNumBoards() const { return numBoards_; }

    bool setNumPlayers(int n) {
        if (n >= 0 && n <= 2) {
            numPlayers_ = n;
            return true;
        }
        return false;
    }

    bool setNumBoards(int n) {
        if (n >= 1 && n <= 9) {
            numBoards_ = n;
            return true;
        }
        return false;
    }

    bool isValid() const {
        return numPlayers_ >= 0 && numPlayers_ <= 2 &&
               numBoards_ >= 1 && numBoards_ <= 9;
    }

private:
    int numPlayers_ = 2;
    int numBoards_ = 1;
};

} // namespace ttt

#endif // SETTINGS_H