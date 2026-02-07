#ifndef UI_H
#define UI_H

#include <ncurses.h>
#include <vector>
#include <string>
#include "Board.h"

// Forward declaration
class Settings;

// Clase para gestionar la interfaz de usuario con ncurses
class UI {
private:
    bool mouseEnabled;
    int maxY, maxX;  // Dimensiones de la terminal
    
    // Colores
    void initColors();
    
public:
    UI();
    ~UI();
    
    // Inicialización y limpieza
    bool init();
    void cleanup();
    
    // Utilidades de pantalla
    void clearScreen();
    void refresh();
    void getTerminalSize(int& rows, int& cols);
    
    // Dibujo de menús
    void drawMenu(const std::vector<std::string>& options, int selected, const std::string& title);
    
    // Dibujo de tableros
    void drawBoards(const std::vector<Board*>& boards, int selectedBoard, 
                    int cursorRow, int cursorCol, const Settings& settings);
    void drawSingleBoard(const Board* board, int startY, int startX, 
                        bool isSelected, int cursorRow, int cursorCol);
    
    // Dibujo de información
    void drawStats(const std::vector<Board*>& boards, int startY);
    void drawHelp();
    void drawSettingsMenu(const Settings& settings, int selected);
    
    // Mensajes
    void showMessage(const std::string& message, int y, int x);
    void showCenteredMessage(const std::string& message, int y);
    
    // Input
    int getInput();
    bool getMouseEvent(int& y, int& x, int& button);
    
    // Conversión de coordenadas de ratón a casilla del tablero
    bool mouseToCell(int mouseY, int mouseX, int boardStartY, int boardStartX,
                     int& row, int& col);
    
    // Navegación
    void waitForKey();
};

#endif
