#include "../include/menu.h"
#include <ncurses.h>
#include <string>
#include <vector>

Menu::Menu() : selected_option(0) {
    config.num_players = 1;
    config.num_boards = 1;
}

int Menu::navigateOptions(const std::vector<std::string>& options, int current) {
    int choice = current;
    int ch;
    
    while (true) {
        clear();
        
        int start_y = (LINES - options.size()) / 2;
        
        for (size_t i = 0; i < options.size(); i++) {
            if (static_cast<int>(i) == choice) {
                attron(A_REVERSE);
                mvprintw(start_y + i, (COLS - options[i].length()) / 2, "%s", options[i].c_str());
                attroff(A_REVERSE);
            } else {
                mvprintw(start_y + i, (COLS - options[i].length()) / 2, "%s", options[i].c_str());
            }
        }
        
        refresh();
        
        ch = getch();
        
        switch (ch) {
            case KEY_UP:
                choice = (choice - 1 + options.size()) % options.size();
                break;
            case KEY_DOWN:
                choice = (choice + 1) % options.size();
                break;
            case 10: // Enter
            case KEY_ENTER:
                return choice;
            case 27: // ESC
                return -1;
        }
    }
}

MenuOption Menu::showMainMenu() {
    std::vector<std::string> options = {
        ">> JUGAR <<",
        ">> AJUSTES <<",
        ">> AYUDA <<",
        ">> SALIR <<"
    };
    
    clear();
    
    // Título
    attron(A_BOLD);
    mvprintw(LINES / 2 - 6, (COLS - 20) / 2, "TICTACTOE");
    attroff(A_BOLD);
    
    int choice = navigateOptions(options, selected_option);
    selected_option = choice;
    
    if (choice == -1) return MENU_EXIT;
    return static_cast<MenuOption>(choice);
}

void Menu::showSettings(GameConfig& cfg) {
    int option = 0;
    
    while (true) {
        clear();
        
        attron(A_BOLD);
        mvprintw(2, (COLS - 10) / 2, "AJUSTES");
        attroff(A_BOLD);
        
        int y = 5;
        
        // Opción de jugadores
        if (option == 0) attron(A_REVERSE);
        mvprintw(y++, 5, "Numero de jugadores: %d", cfg.num_players);
        if (option == 0) attroff(A_REVERSE);
        mvprintw(y++, 7, "0 = Automatico completo");
        mvprintw(y++, 7, "1 = Humano (X) vs Auto (O)");
        mvprintw(y++, 7, "2 = Humano (O) + Auto (X)");
        
        y++;
        
        // Opción de tableros
        if (option == 1) attron(A_REVERSE);
        mvprintw(y++, 5, "Numero de tableros: %d", cfg.num_boards);
        if (option == 1) attroff(A_REVERSE);
        
        y += 2;
        
        // Botón volver
        if (option == 2) attron(A_REVERSE);
        mvprintw(y, 5, "VOLVER");
        if (option == 2) attroff(A_REVERSE);
        
        refresh();
        
        int ch = getch();
        
        switch (ch) {
            case KEY_UP:
                option = (option - 1 + 3) % 3;
                break;
            case KEY_DOWN:
                option = (option + 1) % 3;
                break;
            case KEY_LEFT:
                if (option == 0 && cfg.num_players > 0) cfg.num_players--;
                if (option == 1 && cfg.num_boards > 1) cfg.num_boards--;
                break;
            case KEY_RIGHT:
                if (option == 0 && cfg.num_players < 2) cfg.num_players++;
                if (option == 1 && cfg.num_boards < 9) cfg.num_boards++;
                break;
            case 10: // Enter
            case KEY_ENTER:
                if (option == 2) return;
                break;
            case 27: // ESC
                return;
        }
    }
}

void Menu::showHelp() {
    clear();
    
    attron(A_BOLD);
    mvprintw(1, (COLS - 6) / 2, "AYUDA");
    attroff(A_BOLD);
    
    int y = 3;
    
    mvprintw(y++, 2, "=== CONTROLES ===");
    mvprintw(y++, 2, "- Flechas: Navegar menus");
    mvprintw(y++, 2, "- Enter: Seleccionar opcion");
    mvprintw(y++, 2, "- Raton: Clic en casillas durante el juego");
    mvprintw(y++, 2, "- ESC o Q: Salir del juego");
    
    y++;
    mvprintw(y++, 2, "=== MODOS DE JUEGO ===");
    mvprintw(y++, 2, "0 jugadores: Partidas automaticas continuas");
    mvprintw(y++, 2, "1 jugador: Tu controlas X, Auto juega O");
    mvprintw(y++, 2, "2 jugadores: Tu controlas O, Auto juega X");
    
    y++;
    mvprintw(y++, 2, "=== REGLAS ===");
    mvprintw(y++, 2, "- Consigue 3 en linea (horizontal, vertical o diagonal)");
    mvprintw(y++, 2, "- Si el tablero se llena sin ganador: empate");
    mvprintw(y++, 2, "- Las puntuaciones se acumulan durante la sesion");
    
    y += 2;
    mvprintw(y, 2, "Presiona cualquier tecla para volver...");
    
    refresh();
    getch();
}
