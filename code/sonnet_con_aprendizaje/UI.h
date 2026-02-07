#ifndef UI_H
#define UI_H

#include <ncurses.h>
#include <string>
#include "GameManager.h"

// Opciones del menú principal
enum class MenuOption {
    PLAY,
    SETTINGS,
    HELP,
    EXIT
};

// Clase que maneja la interfaz de usuario con ncurses
class UI {
private:
    GameManager& gameManager;
    WINDOW* mainWin;
    int screenWidth;
    int screenHeight;
    MenuOption currentMenuOption;
    bool mouseEnabled;

public:
    UI(GameManager& gm);
    ~UI();
    
    // Inicializa ncurses
    void init();
    
    // Limpia y cierra ncurses
    void cleanup();
    
    // Muestra el menú principal
    void showMainMenu();
    
    // Muestra el menú de ajustes
    void showSettings();
    
    // Muestra la ayuda
    void showHelp();
    
    // Ejecuta el juego
    void playGame();
    
    // Ejecuta el bucle principal
    void run();
    
private:
    // Actualiza dimensiones de pantalla
    void updateScreenSize();
    
    // Dibuja un tablero en la posición especificada
    void drawBoard(int boardIndex, int startY, int startX, int width, int height, bool selected);
    
    // Dibuja todos los tableros
    void drawAllBoards();
    
    // Dibuja estadísticas
    void drawStats();
    
    // Dibuja la información del turno
    void drawTurnInfo();
    
    // Obtiene el símbolo de una celda
    char getCellSymbol(CellState state) const;
    
    // Maneja el clic del ratón en el juego
    void handleMouseClickGame(int y, int x);
    
    // Calcula el layout de los tableros
    void calculateBoardLayout(int numBoards, int& rows, int& cols);
    
    // Obtiene las coordenadas de un tablero
    bool getBoardCoordinates(int boardIndex, int& startY, int& startX, int& width, int& height);
    
    // Dibuja un cuadro centrado con título
    void drawBox(int height, int width, int startY, int startX, const std::string& title);
    
    // Muestra un mensaje y espera tecla
    void showMessage(const std::string& message);
};

#endif
