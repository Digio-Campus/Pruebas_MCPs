#include "../include/Game.h"
#include <iostream>
#include <ncurses.h>

int main() {
    Game game;
    try {
        game.run();
    } catch (const std::exception &e) {
        endwin();
        std::cerr << "Error: " << e.what() << std::endl;
        return 1;
    }
    return 0;
}
