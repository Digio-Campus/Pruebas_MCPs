#ifndef UI_H
#define UI_H

#include <ncurses.h>
#include "game.h"
#include <vector>
#include <string>

// Estructura para manejar el estado de la UI
struct UIState {
    int screenWidth;
    int screenHeight;
    int selectedBoard;
    int cursorRow;
    int cursorCol;
    bool resize;
};

// Clase para manejar la interfaz con ncurses
class UI {
private:
    WINDOW* mainWindow;
    std::vector<WINDOW*> boardWindows;
    UIState state;
    Game* game;
    
public:
    UI(Game* gameInstance);
    ~UI();
    
    // Inicialización y limpieza
    bool initNCurses();
    void cleanupNCurses();
    
    // Menú y navegación
    int showMainMenu();
    void showSettingsMenu(int& numPlayers, int& numBoards);
    void showHelpMenu();
    
    // Renderizado
    void render();
    void renderAllBoards();
    void renderBoard(int boardIndex, int startX, int startY, int width, int height);
    void renderStatusBar();
    void renderCursor(int boardIndex);
    void clearScreen();
    
    // Manejo de entrada
    int handleInput(int ch);
    void handleBoardNavigation(int ch);
    void handleCursorMovement(int ch);
    
    // Utilidades
    void displayMessage(const std::string& message);
    void waitForKey();
    bool checkWindowResize();
    void updateWindowSize();
    std::pair<int, int> calculateBoardLayout();
    
    // Getters
    UIState getState() const;
    int getSelectedBoard() const;
    int getCursorRow() const;
    int getCursorCol() const;
};

#endif // UI_H
