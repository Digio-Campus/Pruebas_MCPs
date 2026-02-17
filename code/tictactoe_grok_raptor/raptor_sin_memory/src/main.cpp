#include "../include/Game.h"
#include "../include/UI.h"
#include <iostream>

int main(int argc, char** argv) {
    try {
        // valores por defecto: 4 tableros, modo 1 (un jugador controla X y O)
        ttt::Game game(4, ttt::Mode::ManualBoth);
        ttt::UI ui(game);
        return ui.run();
    } catch (const std::exception &ex) {
        std::cerr << "Error: " << ex.what() << std::endl;
        return 1;
    }
}
