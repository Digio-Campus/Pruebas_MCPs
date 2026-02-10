#include "Game.h"
#include <iostream>

int main() {
    try {
        Game game;
        game.run();
    } catch (const std::exception& e) {
        // Ensure ncurses is cleaned up before printing error
        endwin();
        std::cerr << "Error: " << e.what() << std::endl;
        return 1;
    }
    
    return 0;
}
