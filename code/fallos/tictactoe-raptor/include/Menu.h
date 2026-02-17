#pragma once

#include <string>
#include <vector>

struct MenuItem { std::string label; };

class Menu {
public:
    Menu();
    int run(); // blocks and returns selected index: 0=Play,1=Settings,2=Help,3=Quit

    // settings menu
    int runSettings(int &numPlayers, int &numBoards);

    // help text
    void showHelp();
};
