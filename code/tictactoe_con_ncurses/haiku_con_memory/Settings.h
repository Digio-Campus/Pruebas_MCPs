#ifndef SETTINGS_H
#define SETTINGS_H

enum class GameMode { ZERO_PLAYERS, ONE_PLAYER, TWO_PLAYERS };

class Settings {
private:
    GameMode gameMode;
    int numBoards;
    int boardsPerRow;

public:
    Settings();
    
    // Getters
    GameMode getGameMode() const;
    int getNumBoards() const;
    int getBoardsPerRow() const;
    
    // Setters
    void setGameMode(GameMode mode);
    void setNumBoards(int num);
    void calculateLayout(int termWidth, int termHeight);
};

#endif // SETTINGS_H
