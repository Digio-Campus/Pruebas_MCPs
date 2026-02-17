#ifndef MENU_H
#define MENU_H

#include "Settings.h"
#include "UI.h"

/**
 * @class Menu
 * @brief Gestiona la navegación de menús
 *
 * Maneja la lógica de menús principales, ajustes y ayuda.
 */
class Menu {
private:
    UI& ui;              ///< Referencia a la interfaz de usuario
    Settings& settings;  ///< Referencia a la configuración

public:
    /**
     * @brief Constructor
     * @param ui Referencia a UI
     * @param settings Referencia a Settings
     */
    Menu(UI& ui, Settings& settings);

    /**
     * @brief Muestra el menú principal y maneja la navegación
     * @return Opción seleccionada (0: Jugar, 1: Ajustes, 2: Ayuda, 3: Salir)
     */
    int showMainMenu();

    /**
     * @brief Muestra el menú de ajustes
     */
    void showSettingsMenu();

    /**
     * @brief Muestra la pantalla de ayuda
     */
    void showHelp();
};

#endif // MENU_H