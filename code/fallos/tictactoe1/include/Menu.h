#ifndef MENU_H
#define MENU_H

#include <string>
#include <vector>
#include "UI.h"
#include "Settings.h"

// Enumeración para las opciones del menú principal
enum MenuOption {
    MENU_PLAY,
    MENU_SETTINGS,
    MENU_HELP,
    MENU_QUIT
};

// Clase para gestionar los menús del juego
class Menu {
private:
    UI* ui;
    Settings* settings;
    int currentOption;
    std::vector<std::string> mainMenuOptions;
    
    // Métodos privados para cada menú
    MenuOption showMainMenu();
    void showSettingsMenu();
    void showHelp();

public:
    Menu(UI* userInterface, Settings* gameSettings);
    
    // Ejecutar el menú principal y retornar la opción seleccionada
    MenuOption run();
    
    // Gestión del menú de ajustes
    void handleSettingsInput(int& selected);
};

#endif
