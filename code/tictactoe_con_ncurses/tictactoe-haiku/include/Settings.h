#ifndef SETTINGS_H
#define SETTINGS_H

namespace ttt {

class Settings {
public:
    Settings();
    
    int getNumPlayers() const;
    int getNumBoards() const;
    
    void setNumPlayers(int num);
    void setNumBoards(int num);
    
    bool isValid() const;

private:
    int numPlayers_;  // 0, 1, or 2
    int numBoards_;   // 1 to 9
};

}  // namespace ttt

#endif  // SETTINGS_H
