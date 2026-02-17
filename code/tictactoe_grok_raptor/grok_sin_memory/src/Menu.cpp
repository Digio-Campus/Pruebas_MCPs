#include "Menu.h"
#include <ncurses.h>

Menu::Menu(UI& ui, Settings& settings) : ui(ui), settings(settings) {}

int Menu::showMainMenu() {
    int selectedItem = 0;
    int ch;
    while (true) {
        ui.drawMenu(selectedItem);
        ch = ui.getInput();
        switch (ch) {
            case KEY_UP:
                selectedItem = (selectedItem - 1 + 4) % 4;
                break;
            case KEY_DOWN:
                selectedItem = (selectedItem + 1) % 4;
                break;
            case '\n':
            case KEY_ENTER:
                return selectedItem;
        }
    }
}

void Menu::showSettingsMenu() {
    int selectedItem = 0; // 0: players, 1: boards
    int ch;
    while (true) {
        ui.drawSettingsMenu(settings, selectedItem);
        ch = ui.getInput();
        switch (ch) {
            case KEY_LEFT:
                if (selectedItem == 0) {
                    settings.setNumPlayers(settings.getNumPlayers() - 1);
                } else {
                    settings.setNumBoards(settings.getNumBoards() - 1);
                }
                break;
            case KEY_RIGHT:
                if (selectedItem == 0) {
                    settings.setNumPlayers(settings.getNumPlayers() + 1);
                } else {
                    settings.setNumBoards(settings.getNumBoards() + 1);
                }
                break;
            case KEY_UP:
            case KEY_DOWN:
                selectedItem = 1 - selectedItem;
                break;
            case '\n':
            case KEY_ENTER:
                return;
        }
    }
}

void Menu::showHelp() {
    ui.drawHelp();
    ui.getInput(); // Esperar cualquier tecla
}