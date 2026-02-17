#include "UI.h"
#include "Settings.h"
#include "Menu.h"
#include "Game.h"

int main() {
    UI ui;
    ui.init();
    Settings settings;
    Menu menu(ui, settings);
    while(true) {
        int choice = menu.showMainMenu();
        if(choice == 0) { // Jugar
            Game game(ui, settings);
            game.run();
        } else if(choice == 1) { // Ajustes
            menu.showSettingsMenu();
        } else if(choice == 2) { // Ayuda
            ui.drawHelp();
            getch();
        } else if(choice == 3) { // Salir
            break;
        }
    }
    ui.cleanup();
    return 0;
}