#include "../include/Game.h"
#include <iostream>

int main(int argc, char** argv)
{
    try {
        ttt::Game game;
        return game.run();
    } catch (const std::exception &ex) {
        std::cerr << "Fatal: " << ex.what() << "\n";
        return 2;
    }
}
