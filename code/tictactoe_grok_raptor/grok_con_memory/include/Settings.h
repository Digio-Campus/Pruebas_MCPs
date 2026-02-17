#ifndef SETTINGS_H
#define SETTINGS_H

class Settings {
private:
    int numPlayers;  // 0, 1, or 2
    int numBoards;   // 1 to 9

public:
    Settings();
    void setNumPlayers(int players);
    void setNumBoards(int boards);
    int getNumPlayers() const;
    int getNumBoards() const;
    bool isValid() const;
};

#endif // SETTINGS_H