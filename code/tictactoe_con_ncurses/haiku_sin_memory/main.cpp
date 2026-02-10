#include <iostream>
#include <cstdlib>
#include <ncurses.h>
#include "game.h"
#include "ui.h"
#include <unistd.h>

// Variables globales para manejo de señales
volatile int shouldExit = 0;

// Función principal del juego
int runGameLoop(Game& game, UI& ui) {
    int numPlayers = 1;
    int numBoards = 1;
    
    ui.showSettingsMenu(numPlayers, numBoards);
    
    GameMode mode = (GameMode)numPlayers;
    game.initGame(numBoards, mode);
    
    // Bucle principal del juego
    while (game.getIsRunning()) {
        ui.updateWindowSize();
        ui.render();
        
        // Obtener entrada (con timeout)
        int ch = getch();
        
        // Si es modo automático, hacer movimientos automáticos
        if (mode == MODE_AUTO) {
            for (int i = 0; i < numBoards; i++) {
                Board* board = game.getBoard(i);
                if (board && !board->isGameOver() && board->getMoveCount() < 9) {
                    auto moves = board->getAvailableMoves();
                    if (!moves.empty()) {
                        int randomIdx = rand() % moves.size();
                        board->makeMove(moves[randomIdx].first, moves[randomIdx].second);
                    }
                }
            }
            usleep(500000);  // Esperar 500ms entre movimientos automáticos
        } else if (mode == MODE_MANUAL) {
            // Modo manual - permitir entrada de usuario
            int result = ui.handleInput(ch);
            if (result == -1) {  // ESC - volver al menú
                return 0;
            }
        } else if (mode == MODE_AI) {
            // Modo con IA
            int result = ui.handleInput(ch);
            if (result == -1) {  // ESC - volver al menú
                return 0;
            }
        }
    }
    
    return 0;
}

// Función principal
int main() {
    try {
        Game game;
        UI ui(&game);
        
        // Inicializar ncurses
        if (!ui.initNCurses()) {
            std::cerr << "Error inicializando ncurses" << std::endl;
            return 1;
        }
        
        // Bucle del menú principal
        while (true) {
            int choice = ui.showMainMenu();
            
            switch (choice) {
                case 1:  // Jugar
                    runGameLoop(game, ui);
                    break;
                case 2:  // Ajustes (ya está en runGameLoop)
                    ui.displayMessage("Ajustes accesibles desde el menú de Jugar");
                    ui.waitForKey();
                    break;
                case 3:  // Ayuda
                    ui.showHelpMenu();
                    break;
                case 4:  // Salir
                    game.setIsRunning(false);
                    ui.cleanupNCurses();
                    std::cout << "¡Hasta luego!" << std::endl;
                    return 0;
                default:
                    break;
            }
        }
        
        ui.cleanupNCurses();
    } catch (const std::exception& e) {
        endwin();  // Limpiar ncurses en caso de excepción
        std::cerr << "Error: " << e.what() << std::endl;
        return 1;
    }
    
    return 0;
}
