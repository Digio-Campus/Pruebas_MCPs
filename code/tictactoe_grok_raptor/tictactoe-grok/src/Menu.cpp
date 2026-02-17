#include "Menu.h"

namespace ttt {

Menu::Menu(UI& ui, Settings& settings)
    : ui_(ui), settings_(settings) {}

Menu::MenuResult Menu::showMainMenu() {
    std::vector<std::string> options = {"Jugar", "Ajustes", "Ayuda", "Salir"};
    int selected = 0;

    while (true) {
        ui_.drawMenu(options, selected);
        int ch = getch();
        switch (ch) {
            case KEY_UP:
                selected = (selected - 1 + static_cast<int>(options.size())) % options.size();
                break;
            case KEY_DOWN:
                selected = (selected + 1) % options.size();
                break;
            case '\n':
            case KEY_ENTER:
                switch (selected) {
                    case 0: return MenuResult::Play;
                    case 1: showSettingsMenu(); break;
                    case 2: showHelp(); break;
                    case 3: return MenuResult::Exit;
                }
                break;
            case 'q':
            case 'Q':
                return MenuResult::Exit;
        }
    }
}

void Menu::showSettingsMenu() {
    int selectedOption = 0;

    while (true) {
        ui_.drawSettingsMenu(settings_, selectedOption);
        int ch = getch();
        switch (ch) {
            case KEY_UP:
                selectedOption = (selectedOption - 1 + 3) % 3;
                break;
            case KEY_DOWN:
                selectedOption = (selectedOption + 1) % 3;
                break;
            case KEY_LEFT:
                if (selectedOption == 0) {
                    int p = settings_.getNumPlayers() - 1;
                    settings_.setNumPlayers(p);
                } else if (selectedOption == 1) {
                    int b = settings_.getNumBoards() - 1;
                    settings_.setNumBoards(b);
                }
                break;
            case KEY_RIGHT:
                if (selectedOption == 0) {
                    int p = settings_.getNumPlayers() + 1;
                    settings_.setNumPlayers(p);
                } else if (selectedOption == 1) {
                    int b = settings_.getNumBoards() + 1;
                    settings_.setNumBoards(b);
                }
                break;
            case '\n':
            case KEY_ENTER:
                if (selectedOption == 2) return;
                break;
            case 'q':
            case 'Q':
                return;
        }
    }
}

void Menu::showHelp() {
    ui_.drawHelp();
    getch();
}

} // namespace ttt