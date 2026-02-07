#ifndef GAME_H
#define GAME_H

#include <vector>
#include "Board.h"
#include "UI.h"
#include "Settings.h"

// Clase que controla el flujo principal del juego
class Game {
private:
    UI* ui;
    Settings* settings;
    std::vector<Board*> boards;
    
    int selectedBoard;   // Tablero actualmente seleccionado
    int cursorRow;       // Posición del cursor (fila)
    int cursorCol;       // Posición del cursor (columna)
    bool running;        // Estado del juego
    
    // Métodos de inicialización
    void createBoards();
    void clearBoards();
    
    // Métodos de juego
    void handleInput(int ch);
    void handleMouseInput(int y, int x, int button);
    void processMove();
    void processAutoMove();
    void switchBoard(int direction);
    void resetCurrentBoard();
    
    // Lógica según modo de juego
    void handleMode0();  // 0 jugadores (auto)
    void handleMode1();  // 1 jugador (manual completo)
    void handleMode2();  // 2 jugadores (jugador vs auto)
    
public:
    Game(UI* userInterface, Settings* gameSettings);
    ~Game();
    
    // Ejecutar el juego
    void run();
    
    // Detener el juego
    void stop();
};

#endif
