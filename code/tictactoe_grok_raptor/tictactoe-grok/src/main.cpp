#include "UI.h"
#include "Settings.h"
#include "Menu.h"
#include "Game.h"
#include <iostream>
#include <exception>

int main() {
    try {
        ttt::UI ui;
        ui.init();
        ttt::Settings settings;
        ttt::Menu menu(ui, settings);

        while (true) {
            auto result = menu.showMainMenu();
            if (result == ttt::Menu::MenuResult::Play) {
                ttt::Game game(ui, settings);
                game.run();
            } else if (result == ttt::Menu::MenuResult::Exit) {
                break;
            }
        }

        ui.cleanup();
    } catch (const std::exception& e) {
        endwin();
        std::cerr << "Error: " << e.what() << std::endl;
        return 1;
    }
    return 0;
}