#include <iostream>
#include "../include/UI.h"
#include "../include/Settings.h"
#include "../include/Menu.h"
#include "../include/Game.h"

int main() {
    // Inicializar componentes
    UI ui;
    Settings settings;
    
    // Inicializar ncurses
    if (!ui.init()) {
        std::cerr << "Error al inicializar ncurses" << std::endl;
        return 1;
    }
    
    // Crear menú y juego
    Menu menu(&ui, &settings);
    Game game(&ui, &settings);
    
    bool running = true;
    
    // Bucle principal
    while (running) {
        // Mostrar menú y obtener opción
        MenuOption option = menu.run();
        
        switch (option) {
            case MENU_PLAY:
                // Iniciar juego
                game.run();
                break;
                
            case MENU_SETTINGS:
                // Los ajustes se manejan dentro del menú
                break;
                
            case MENU_HELP:
                // La ayuda se maneja dentro del menú
                break;
                
            case MENU_QUIT:
                running = false;
                break;
        }
    }
    
    // Limpieza
    ui.cleanup();
    
    return 0;
}
