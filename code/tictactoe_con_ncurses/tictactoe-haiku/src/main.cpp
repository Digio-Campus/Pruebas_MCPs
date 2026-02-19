#include "UI.h"
#include "Menu.h"
#include "Game.h"
#include "Settings.h"
#include <iostream>

using namespace ttt;

int main() {
    try {
        UI ui;
        
        if (ui.isTerminalTooSmall()) {
            std::cerr << "Error: Terminal is too small!\n";
            std::cerr << "Minimum required: 80x24 characters\n";
            std::cerr << "Current size: " << ui.getMaxX() << "x" << ui.getMaxY() << "\n";
            return 1;
        }
        
        Settings settings;
        Menu menu(ui, settings);
        
        MenuState state = MenuState::Main;
        
        while (state != MenuState::Exit) {
            if (state == MenuState::Main) {
                state = menu.showMainMenu();
            } else if (state == MenuState::Settings) {
                state = menu.showSettingsMenu();
            } else if (state == MenuState::Help) {
                state = menu.showHelpMenu();
            } else if (state == MenuState::Playing) {
                Game game(ui, settings);
                game.play();
                state = MenuState::Main;
            }
        }
        
        ui.cleanup();
        return 0;
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << "\n";
        return 1;
    }
}
