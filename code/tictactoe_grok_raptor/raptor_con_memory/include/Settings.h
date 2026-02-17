#ifndef TICTACTOE_SETTINGS_H
#define TICTACTOE_SETTINGS_H

class Settings {
public:
    Settings();
    int numPlayers; // 0,1,2
    int numBoards;   // 1..9
    bool setNumPlayers(int v);
    bool setNumBoards(int v);
};

#endif // TICTACTOE_SETTINGS_H
