#include "UI.h"
#include <string>
#include <sstream>

// Constructor
UI::UI(Game* g) : game(g), currentState(MENU), selectedOption(0), selectedBoard(0), selectedRow(0), selectedCol(0) {}

// Destructor
UI::~UI() {
    endwin();
}

// Inicializa ncurses
void UI::init() {
    initscr();
    cbreak();
    noecho();
    keypad(stdscr, TRUE);
    mousemask(ALL_MOUSE_EVENTS, NULL); // Habilitar mouse
    getmaxyx(stdscr, height, width);
    win = newwin(height, width, 0, 0);
}

// Bucle principal
void UI::run() {
    while (true) {
        clear();
        switch (currentState) {
            case MENU: drawMenu(); break;
            case SETTINGS: drawSettings(); break;
            case HELP: drawHelp(); break;
            case GAME: drawGame(); break;
        }
        refresh();
        handleInput();
        if (currentState == MENU && selectedOption == 3) break; // Salir
    }
}

// Dibuja el menú
void UI::drawMenu() {
    mvprintw(1, 1, "Tres en Raya con ncurses");
    mvprintw(3, 1, "1. Jugar");
    mvprintw(4, 1, "2. Ajustes");
    mvprintw(5, 1, "3. Ayuda");
    mvprintw(6, 1, "4. Salir");
    mvprintw(8, 1, "Usa flechas y Enter para navegar.");
    mvprintw(selectedOption + 3, 0, ">");
}

// Dibuja ajustes
void UI::drawSettings() {
    mvprintw(1, 1, "Ajustes");
    std::stringstream ss;
    ss << "Jugadores: " << game->getNumPlayers();
    mvprintw(3, 1, ss.str().c_str());
    ss.str("");
    ss << "Tableros: " << game->getNumBoards();
    mvprintw(4, 1, ss.str().c_str());
    mvprintw(6, 1, "Presiona 1-2 para jugadores, 3-9 para tableros, ESC para volver.");
}

// Dibuja ayuda
void UI::drawHelp() {
    mvprintw(1, 1, "Ayuda");
    mvprintw(3, 1, "Controles:");
    mvprintw(4, 1, "- Flechas: Navegar");
    mvprintw(5, 1, "- Enter: Seleccionar");
    mvprintw(6, 1, "- ESC: Volver");
    mvprintw(8, 1, "Modos:");
    mvprintw(9, 1, "- 0 jugadores: Auto");
    mvprintw(10, 1, "- 1 jugador: Manual X y O");
    mvprintw(11, 1, "- 2 jugadores: Jugador O vs Auto X");
    mvprintw(13, 1, "Presiona ESC para volver.");
}

// Dibuja el juego
void UI::drawGame() {
    int boardSize = 5; // Alto de cada tablero
    int boardsPerRow = width / 20; // Aproximado
    for (int i = 0; i < game->getNumBoards(); ++i) {
        int x = (i % boardsPerRow) * 20;
        int y = (i / boardsPerRow) * boardSize + 1;
        Board& board = game->getBoard(i);
        mvprintw(y, x, "Tablero %d", i+1);
        for (int r = 0; r < 3; ++r) {
            for (int c = 0; c < 3; ++c) {
                char cell = board.getCell(r, c);
                mvprintw(y + r + 1, x + c * 2, "%c", cell == ' ' ? '.' : cell);
            }
        }
        std::stringstream ss;
        ss << "X:" << board.getWinsX() << " O:" << board.getWinsO() << " D:" << board.getDraws();
        mvprintw(y + 4, x, ss.str().c_str());
    }
    mvprintw(height - 2, 1, "Tablero actual: %d, Turno: %c", selectedBoard + 1, game->getBoard(selectedBoard).getCurrentPlayer());
}

// Maneja entrada
void UI::handleInput() {
    int ch = getch();
    MEVENT event;
    if (ch == KEY_MOUSE && getmouse(&event) == OK) {
        // Manejar clic del mouse
        // Simplificado: asumir posiciones
        if (currentState == MENU) {
            if (event.y >= 3 && event.y <= 6) {
                selectedOption = event.y - 3;
                if (event.bstate & BUTTON1_CLICKED) {
                    if (selectedOption == 0) currentState = GAME;
                    else if (selectedOption == 1) currentState = SETTINGS;
                    else if (selectedOption == 2) currentState = HELP;
                    else if (selectedOption == 3) ; // Salir
                }
            }
        }
        // Más manejo de mouse si necesario
    } else {
        switch (currentState) {
            case MENU:
                if (ch == KEY_UP) selectedOption = (selectedOption - 1 + 4) % 4;
                else if (ch == KEY_DOWN) selectedOption = (selectedOption + 1) % 4;
                else if (ch == '\n') {
                    if (selectedOption == 0) currentState = GAME;
                    else if (selectedOption == 1) currentState = SETTINGS;
                    else if (selectedOption == 2) currentState = HELP;
                }
                break;
            case SETTINGS:
                if (ch >= '0' && ch <= '2') game->setSettings(ch - '0', game->getNumBoards());
                else if (ch >= '3' && ch <= '9') game->setSettings(game->getNumPlayers(), ch - '0');
                else if (ch == 27) currentState = MENU; // ESC
                break;
            case HELP:
                if (ch == 27) currentState = MENU;
                break;
            case GAME:
                if (ch == KEY_UP) selectedRow = (selectedRow - 1 + 3) % 3;
                else if (ch == KEY_DOWN) selectedRow = (selectedRow + 1) % 3;
                else if (ch == KEY_LEFT) selectedCol = (selectedCol - 1 + 3) % 3;
                else if (ch == KEY_RIGHT) selectedCol = (selectedCol + 1) % 3;
                else if (ch == '\t') { // Tab para cambiar tablero
                    selectedBoard = (selectedBoard + 1) % game->getNumBoards();
                    game->setCurrentBoard(selectedBoard);
                } else if (ch == '\n') {
                    game->playTurn(selectedBoard, selectedRow, selectedCol);
                } else if (ch == 'r') game->resetBoard(selectedBoard);
                else if (ch == 27) currentState = MENU;
                if (game->getNumPlayers() == 0) game->autoPlayAll();
                break;
        }
    }
}

// Redimensionar (simplificado)
void UI::resize() {
    getmaxyx(stdscr, height, width);
    wresize(win, height, width);
}