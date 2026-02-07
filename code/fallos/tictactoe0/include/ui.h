#ifndef UI_H
#define UI_H

#include "game.h"
#include <ncurses.h>

// Gestiona la interfaz de usuario con ncurses
class UI {
private:
    bool mouse_enabled;
    int term_width;
    int term_height;
    
    // Calcula el layout de los tableros según el tamaño de la terminal
    void calculateLayout(int num_boards, int& cols, int& rows);
    
    // Dibuja un tablero individual en una posición específica
    void drawBoard(const Board& board, int x, int y, int board_num, bool highlight);
    
    // Obtiene las coordenadas de un tablero y celda desde el clic del ratón
    bool getClickPosition(int mouse_y, int mouse_x, int num_boards, 
                         int& board_idx, int& row, int& col);

public:
    UI();
    ~UI();
    
    // Inicializa ncurses
    void init();
    
    // Limpia y cierra ncurses
    void cleanup();
    
    // Renderiza los tableros del juego
    void renderGame(const Game& game);
    
    // Procesa la entrada del usuario durante el juego
    bool processGameInput(Game& game);
    
    // Muestra un mensaje centrado
    void showMessage(const std::string& msg, int delay_ms = 0);
    
    // Limpia la pantalla
    void clear();
    
    // Refresca la pantalla
    void refresh();
    
    // Obtiene el tamaño de la terminal
    void getTerminalSize(int& width, int& height);
    
    // Maneja el redimensionado de la ventana
    void handleResize();
};

#endif
