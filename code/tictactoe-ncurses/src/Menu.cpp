#include "Menu.h"
#include <ncurses.h>

namespace ttt {

Menu::Menu(UI& ui, Settings& settings) : ui_(ui), settings_(settings) {}

MenuState Menu::showMainMenu() {
    std::vector<std::string> options = {"Play", "Settings", "Help", "Exit"};
    int selected = 0;
    
    while (true) {
        ui_.drawMenu(options, selected);
        
        int ch = ui_.getInput();
        
        if (ch == KEY_UP) {
            selected = (selected - 1 + options.size()) % options.size();
        } else if (ch == KEY_DOWN) {
            selected = (selected + 1) % options.size();
        } else if (ch == '\n' || ch == KEY_ENTER) {
            switch (selected) {
                case 0: return MenuState::Playing;
                case 1: return showSettingsMenu();
                case 2: return showHelpMenu();
                case 3: return MenuState::Exit;
            }
        }
    }
}

MenuState Menu::showSettingsMenu() {
    int selectedOption = 0;
    bool editingValue = false;
    
    while (true) {
        ui_.drawSettingsMenu(settings_.getNumPlayers(), settings_.getNumBoards(), selectedOption, editingValue);
        
        int ch = ui_.getInput();
        
        if (ch == KEY_UP) {
            selectedOption = 0;
        } else if (ch == KEY_DOWN) {
            selectedOption = 1;
        } else if (ch == KEY_LEFT) {
            if (selectedOption == 0) {
                int np = settings_.getNumPlayers();
                if (np > 0) settings_.setNumPlayers(np - 1);
            } else if (selectedOption == 1) {
                int nb = settings_.getNumBoards();
                if (nb > 1) settings_.setNumBoards(nb - 1);
            }
        } else if (ch == KEY_RIGHT) {
            if (selectedOption == 0) {
                int np = settings_.getNumPlayers();
                if (np < 2) settings_.setNumPlayers(np + 1);
            } else if (selectedOption == 1) {
                int nb = settings_.getNumBoards();
                if (nb < 9) settings_.setNumBoards(nb + 1);
            }
        } else if (ch == '\n' || ch == KEY_ENTER) {
            return MenuState::Main;
        }
    }
}

MenuState Menu::showHelpMenu() {
    ui_.drawHelp();
    return MenuState::Main;
}

}  // namespace ttt
