#include "Menu.h"

Menu::Menu(UI& u, Settings& s) : ui(u), settings(s) {}

int Menu::showMainMenu() {
    int selected = 0;
    while(true) {
        ui.drawMenu(selected);
        int ch = getch();
        if(ch == KEY_UP && selected > 0) selected--;
        else if(ch == KEY_DOWN && selected < 3) selected++;
        else if(ch == '\n' || ch == KEY_ENTER) return selected;
        // Mouse handling for menu could be added, but keeping simple
    }
    return selected;
}

void Menu::showSettingsMenu() {
    int selectedOption = 0;
    while(true) {
        ui.drawSettingsMenu(settings, selectedOption, -1);
        int ch = getch();
        if(ch == KEY_UP && selectedOption > 0) selectedOption--;
        else if(ch == KEY_DOWN && selectedOption < 1) selectedOption++;
        else if(ch == KEY_LEFT) {
            if(selectedOption == 0) {
                int p = settings.getNumPlayers();
                if(p > 0) settings.setNumPlayers(p - 1);
            } else {
                int b = settings.getNumBoards();
                if(b > 1) settings.setNumBoards(b - 1);
            }
        } else if(ch == KEY_RIGHT) {
            if(selectedOption == 0) {
                int p = settings.getNumPlayers();
                if(p < 2) settings.setNumPlayers(p + 1);
            } else {
                int b = settings.getNumBoards();
                if(b < 9) settings.setNumBoards(b + 1);
            }
        } else if(ch == '\n' || ch == KEY_ENTER) {
            return;
        }
    }
}