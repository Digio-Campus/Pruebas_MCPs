#include "../include/game.h"
#include "../include/menu.h"
#include "../include/ui.h"
#include <thread>
#include <chrono>
#include <stdexcept>

int main() {
    UI ui;
    
    try {
        ui.init();
        
        Menu menu;
        Game game;
        
        bool running = true;
        
        while (running) {
            MenuOption option = menu.showMainMenu();
            
            switch (option) {
                case MENU_PLAY: {
                    GameConfig cfg = game.getConfig();
                    game.configure(cfg);
                    game.start();
                    
                    ui.showMessage("Iniciando juego...", 1000);
                    
                    while (game.isRunning()) {
                        ui.renderGame(game);
                        
                        if (!ui.processGameInput(game)) {
                            game.stop();
                        }
                        
                        // Pequeña pausa para no sobrecargar la CPU
                        std::this_thread::sleep_for(std::chrono::milliseconds(50));
                    }
                    
                    break;
                }
                
                case MENU_SETTINGS: {
                    GameConfig cfg = game.getConfig();
                    menu.showSettings(cfg);
                    game.configure(cfg);
                    break;
                }
                
                case MENU_HELP: {
                    menu.showHelp();
                    break;
                }
                
                case MENU_EXIT: {
                    running = false;
                    break;
                }
            }
        }
        
        ui.cleanup();
        
    } catch (const std::exception& e) {
        ui.cleanup();
        fprintf(stderr, "Error: %s\n", e.what());
        return 1;
    }
    
    return 0;
}
