#ifndef SETTINGS_H
#define SETTINGS_H

class Settings {
public:
    Settings();
    int numPlayers() const;
    int numBoards() const;
    void setNumPlayers(int p);
    void setNumBoards(int n);
    bool isValid() const;
private:
    int players;
    int boards;
};

#endif
