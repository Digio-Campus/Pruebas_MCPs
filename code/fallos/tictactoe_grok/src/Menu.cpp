#include "Menu.h"
#include "UI.h"
#include "Settings.h"
#include <ncurses.h>

Menu::Menu(UI& uiRef, Settings& settingsRef) : ui(uiRef), settings(settingsRef) {}

int Menu::navigateMenu(const std::vector<std::string>& options, int initialSelection) {
    int selection = initialSelection;
    while (true) {
        ui.drawMenu(selection, options);
        int ch = ui.getInput();
        switch (ch) {
            case KEY_UP:
                selection = (selection - 1 + options.size()) % options.size();
                break;
            case KEY_DOWN:
                selection = (selection + 1) % options.size();
                break;
            case '\n':
            case KEY_ENTER:
                return selection;
        }
    }
}

int Menu::showMainMenu() {
    std::vector<std::string> options = {"Jugar", "Ajustes", "Ayuda", "Salir"};
    return navigateMenu(options);
}

void Menu::showSettingsMenu() {
    while (true) {
        std::vector<std::string> options = {
            "Jugadores: " + std::to_string(settings.getNumPlayers()),
            "Tableros: " + std::to_string(settings.getNumBoards()),
            "Volver"
        };

        int selection = 0;
        while (true) {
            ui.drawSettingsMenu(settings, selection);
            int ch = ui.getInput();
            switch (ch) {
                case KEY_UP:
                    selection = (selection - 1 + options.size()) % options.size();
                    break;
                case KEY_DOWN:
                    selection = (selection + 1) % options.size();
                    break;
                case KEY_LEFT:
                    if (selection == 0) {
                        int p = settings.getNumPlayers();
                        settings.setNumPlayers(p - 1);
                    } else if (selection == 1) {
                        int b = settings.getNumBoards();
                        settings.setNumBoards(b - 1);
                    }
                    break;
                case KEY_RIGHT:
                    if (selection == 0) {
                        int p = settings.getNumPlayers();
                        settings.setNumPlayers(p + 1);
                    } else if (selection == 1) {
                        int b = settings.getNumBoards();
                        settings.setNumBoards(b + 1);
                    }
                    break;
                case '\n':
                case KEY_ENTER:
                    if (selection == 2) {
                        return;  // Volver
                    }
                    // Stay in loop to update display
                    break;
            }
        }
    }
}

void Menu::showHelp() {
    ui.drawHelp();
    ui.getInput();  // Wait for any key
}