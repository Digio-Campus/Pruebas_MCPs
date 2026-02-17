#ifndef SETTINGS_H
#define SETTINGS_H

class Settings {
private:
    int numPlayers;  // 0, 1, or 2
    int numBoards;   // 1 to 9

public:
    Settings();
    ~Settings() = default;

    // Getters
    int getNumPlayers() const { return numPlayers; }
    int getNumBoards() const { return numBoards; }

    // Setters with validation
    bool setNumPlayers(int players);
    bool setNumBoards(int boards);

    // Validation
    bool isValid() const;
};

#endif // SETTINGS_H