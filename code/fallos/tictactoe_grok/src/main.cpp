#include "UI.h"
#include "Settings.h"
#include "Menu.h"
#include "Game.h"

int main() {
    UI ui;
    ui.init();

    Settings settings;
    Menu menu(ui, settings);

    bool running = true;
    while (running) {
        int choice = menu.showMainMenu();
        switch (choice) {
            case 0: {  // Jugar
                Game game(ui, settings);
                game.run();
                break;
            }
            case 1: {  // Ajustes
                menu.showSettingsMenu();
                break;
            }
            case 2: {  // Ayuda
                menu.showHelp();
                break;
            }
            case 3: {  // Salir
                running = false;
                break;
            }
        }
    }

    ui.cleanup();
    return 0;
}