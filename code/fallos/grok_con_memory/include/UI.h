#ifndef UI_H
#define UI_H

#include "Game.h"
#include <ncurses.h>

// Clase para la interfaz de usuario con ncurses
class UI {
private:
    Game* game;
    WINDOW* win;
    int height, width;
    enum State { MENU, SETTINGS, HELP, GAME };
    State currentState;
    int selectedOption; // Para menú
    int selectedBoard, selectedRow, selectedCol; // Para juego

public:
    UI(Game* g);
    ~UI();
    void init();
    void run();
    void drawMenu();
    void drawSettings();
    void drawHelp();
    void drawGame();
    void handleInput();
    void resize();
};

#endif // UI_H