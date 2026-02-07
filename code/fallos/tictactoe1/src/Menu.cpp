#include "../include/Menu.h"

Menu::Menu(UI* userInterface, Settings* gameSettings) 
    : ui(userInterface), settings(gameSettings), currentOption(0) {
    
    // Inicializar opciones del menú principal
    mainMenuOptions.push_back("Jugar");
    mainMenuOptions.push_back("Ajustes");
    mainMenuOptions.push_back("Ayuda");
    mainMenuOptions.push_back("Salir");
}

MenuOption Menu::run() {
    return showMainMenu();
}

MenuOption Menu::showMainMenu() {
    currentOption = 0;
    
    while (true) {
        // Dibujar menú
        ui->drawMenu(mainMenuOptions, currentOption, "=== TICTACTOE ===");
        
        // Obtener input
        int ch = ui->getInput();
        
        switch (ch) {
            case KEY_UP:
                currentOption = (currentOption - 1 + mainMenuOptions.size()) % mainMenuOptions.size();
                break;
                
            case KEY_DOWN:
                currentOption = (currentOption + 1) % mainMenuOptions.size();
                break;
                
            case 10:  // Enter
            case KEY_ENTER:
                switch (currentOption) {
                    case 0:
                        return MENU_PLAY;
                    case 1:
                        showSettingsMenu();
                        break;
                    case 2:
                        showHelp();
                        break;
                    case 3:
                        return MENU_QUIT;
                }
                break;
                
            case 'q':
            case 'Q':
                return MENU_QUIT;
        }
    }
}

void Menu::showSettingsMenu() {
    int selected = 0;
    
    while (true) {
        // Dibujar menú de ajustes
        ui->drawSettingsMenu(*settings, selected);
        
        // Obtener input
        int ch = ui->getInput();
        
        switch (ch) {
            case KEY_UP:
                selected = (selected - 1 + 3) % 3;
                break;
                
            case KEY_DOWN:
                selected = (selected + 1) % 3;
                break;
                
            case KEY_LEFT:
                if (selected == 0) {
                    // Disminuir número de jugadores
                    int players = settings->getNumPlayers();
                    if (players > 0) {
                        settings->setNumPlayers(players - 1);
                    }
                } else if (selected == 1) {
                    // Disminuir número de tableros
                    int boards = settings->getNumBoards();
                    if (boards > 1) {
                        settings->setNumBoards(boards - 1);
                    }
                }
                break;
                
            case KEY_RIGHT:
                if (selected == 0) {
                    // Aumentar número de jugadores
                    int players = settings->getNumPlayers();
                    if (players < 2) {
                        settings->setNumPlayers(players + 1);
                    }
                } else if (selected == 1) {
                    // Aumentar número de tableros
                    int boards = settings->getNumBoards();
                    if (boards < 9) {
                        settings->setNumBoards(boards + 1);
                    }
                }
                break;
                
            case 10:  // Enter
            case KEY_ENTER:
                if (selected == 2) {
                    return;  // Volver al menú principal
                }
                break;
                
            case 'q':
            case 'Q':
                return;  // Volver al menú principal
        }
    }
}

void Menu::showHelp() {
    ui->drawHelp();
    ui->waitForKey();
}

void Menu::handleSettingsInput(int& selected) {
    // Este método se puede usar si se necesita lógica adicional
    // para el manejo de inputs en el menú de ajustes
}
