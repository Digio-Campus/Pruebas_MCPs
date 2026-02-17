#include "UI.h"
#include "Settings.h"
#include "Menu.h"
#include "Game.h"
#include <cstdlib>
#include <ctime>

int main() {
    // Inicializar semilla para números aleatorios
    srand(time(NULL));

    UI ui;
    ui.init();

    Settings settings;
    Menu menu(ui, settings);
    Game game(settings, ui);

    bool running = true;
    while (running) {
        int choice = menu.showMainMenu();
        switch (choice) {
            case 0: // Jugar
                game.play();
                break;
            case 1: // Ajustes
                menu.showSettingsMenu();
                break;
            case 2: // Ayuda
                menu.showHelp();
                break;
            case 3: // Salir
                running = false;
                break;
        }
    }

    ui.cleanup();
    return 0;
}