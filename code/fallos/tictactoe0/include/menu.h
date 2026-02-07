#ifndef MENU_H
#define MENU_H

#include "game.h"
#include <string>
#include <vector>

// Opciones del menú principal
enum MenuOption {
    MENU_PLAY,
    MENU_SETTINGS,
    MENU_HELP,
    MENU_EXIT
};

// Gestiona los menús del juego
class Menu {
private:
    int selected_option;
    GameConfig config;

public:
    Menu();
    
    // Muestra el menú principal y devuelve la opción seleccionada
    MenuOption showMainMenu();
    
    // Muestra el menú de ajustes y permite configurar el juego
    void showSettings(GameConfig& cfg);
    
    // Muestra la pantalla de ayuda
    void showHelp();
    
    // Muestra las opciones y maneja la navegación
    int navigateOptions(const std::vector<std::string>& options, int current);
};

#endif
