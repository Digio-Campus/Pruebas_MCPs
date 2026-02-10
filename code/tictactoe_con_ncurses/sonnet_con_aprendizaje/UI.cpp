#include "UI.h"
#include <algorithm>
#include <cmath>
#include <sstream>

UI::UI(GameManager& gm) : gameManager(gm), 
                          mainWin(nullptr), 
                          screenWidth(0), 
                          screenHeight(0),
                          currentMenuOption(MenuOption::PLAY),
                          mouseEnabled(false) {
}

UI::~UI() {
    cleanup();
}

void UI::init() {
    // Inicializar ncurses
    mainWin = initscr();
    
    // Configurar ncurses
    cbreak();              // Deshabilitar buffering de línea
    noecho();              // No mostrar caracteres escritos
    keypad(stdscr, TRUE);  // Habilitar teclas especiales (flechas, etc.)
    curs_set(0);           // Ocultar cursor
    
    // Habilitar soporte de ratón
    mousemask(ALL_MOUSE_EVENTS | REPORT_MOUSE_POSITION, NULL);
    mouseEnabled = true;
    
    // Habilitar colores si está disponible
    if (has_colors()) {
        start_color();
        init_pair(1, COLOR_CYAN, COLOR_BLACK);    // Título
        init_pair(2, COLOR_GREEN, COLOR_BLACK);   // X
        init_pair(3, COLOR_RED, COLOR_BLACK);     // O
        init_pair(4, COLOR_YELLOW, COLOR_BLACK);  // Selección
        init_pair(5, COLOR_WHITE, COLOR_BLUE);    // Menú seleccionado
        init_pair(6, COLOR_MAGENTA, COLOR_BLACK); // Victoria
    }
    
    updateScreenSize();
}

void UI::cleanup() {
    if (mainWin) {
        endwin();
        mainWin = nullptr;
    }
}

void UI::updateScreenSize() {
    getmaxyx(stdscr, screenHeight, screenWidth);
}

void UI::showMainMenu() {
    while (true) {
        clear();
        updateScreenSize();
        
        // Título
        int titleY = screenHeight / 4;
        std::string title = "=== TICTACTOE MULTI-TABLERO ===";
        
        attron(COLOR_PAIR(1) | A_BOLD);
        mvprintw(titleY, (screenWidth - title.length()) / 2, "%s", title.c_str());
        attroff(COLOR_PAIR(1) | A_BOLD);
        
        // Opciones del menú
        std::vector<std::string> options = {"Jugar", "Ajustes", "Ayuda", "Salir"};
        int startY = titleY + 3;
        
        for (size_t i = 0; i < options.size(); i++) {
            int y = startY + i * 2;
            int x = (screenWidth - 20) / 2;
            
            if ((int)currentMenuOption == i) {
                attron(COLOR_PAIR(5) | A_BOLD);
                mvprintw(y, x, "> %s <", options[i].c_str());
                attroff(COLOR_PAIR(5) | A_BOLD);
            } else {
                mvprintw(y, x, "  %s  ", options[i].c_str());
            }
        }
        
        mvprintw(screenHeight - 2, 2, "Usa flechas o ratón para navegar, ENTER para seleccionar");
        
        refresh();
        
        // Manejar entrada
        int ch = getch();
        
        if (ch == KEY_UP) {
            int opt = (int)currentMenuOption;
            opt = (opt - 1 + 4) % 4;
            currentMenuOption = (MenuOption)opt;
        } else if (ch == KEY_DOWN) {
            int opt = (int)currentMenuOption;
            opt = (opt + 1) % 4;
            currentMenuOption = (MenuOption)opt;
        } else if (ch == '\n' || ch == KEY_ENTER || ch == 10) {
            if (currentMenuOption == MenuOption::PLAY) {
                playGame();
            } else if (currentMenuOption == MenuOption::SETTINGS) {
                showSettings();
            } else if (currentMenuOption == MenuOption::HELP) {
                showHelp();
            } else if (currentMenuOption == MenuOption::EXIT) {
                return;
            }
        } else if (ch == KEY_MOUSE) {
            MEVENT event;
            if (getmouse(&event) == OK) {
                if (event.bstate & BUTTON1_CLICKED) {
                    // Detectar en qué opción se hizo clic
                    for (size_t i = 0; i < options.size(); i++) {
                        int y = startY + i * 2;
                        int x = (screenWidth - 20) / 2;
                        
                        if (event.y == y && event.x >= x && event.x < x + 20) {
                            currentMenuOption = (MenuOption)i;
                            
                            // Ejecutar la acción
                            if (currentMenuOption == MenuOption::PLAY) {
                                playGame();
                            } else if (currentMenuOption == MenuOption::SETTINGS) {
                                showSettings();
                            } else if (currentMenuOption == MenuOption::HELP) {
                                showHelp();
                            } else if (currentMenuOption == MenuOption::EXIT) {
                                return;
                            }
                            break;
                        }
                    }
                }
            }
        } else if (ch == 'q' || ch == 'Q') {
            return;
        }
    }
}

void UI::showSettings() {
    int selectedSetting = 0;
    int numPlayers = (int)gameManager.getGameMode();
    int numBoards = gameManager.getNumBoards();
    
    while (true) {
        clear();
        updateScreenSize();
        
        // Título
        std::string title = "=== AJUSTES ===";
        attron(COLOR_PAIR(1) | A_BOLD);
        mvprintw(2, (screenWidth - title.length()) / 2, "%s", title.c_str());
        attroff(COLOR_PAIR(1) | A_BOLD);
        
        int startY = 5;
        
        // Opción 1: Número de jugadores
        if (selectedSetting == 0) {
            attron(COLOR_PAIR(4) | A_BOLD);
        }
        mvprintw(startY, 5, "> Modo de juego: ");
        if (numPlayers == 0) {
            printw("0 jugadores (Auto)");
        } else if (numPlayers == 1) {
            printw("1 jugador (Manual)");
        } else {
            printw("2 jugadores (Usuario O, Auto X)");
        }
        if (selectedSetting == 0) {
            attroff(COLOR_PAIR(4) | A_BOLD);
        }
        
        // Opción 2: Número de tableros
        if (selectedSetting == 1) {
            attron(COLOR_PAIR(4) | A_BOLD);
        }
        mvprintw(startY + 2, 5, "> Numero de tableros: %d", numBoards);
        if (selectedSetting == 1) {
            attroff(COLOR_PAIR(4) | A_BOLD);
        }
        
        // Opción 3: Volver
        if (selectedSetting == 2) {
            attron(COLOR_PAIR(4) | A_BOLD);
        }
        mvprintw(startY + 4, 5, "> Volver al menu principal");
        if (selectedSetting == 2) {
            attroff(COLOR_PAIR(4) | A_BOLD);
        }
        
        mvprintw(screenHeight - 4, 2, "Flechas arriba/abajo: navegar");
        mvprintw(screenHeight - 3, 2, "Flechas izq/der: cambiar valor");
        mvprintw(screenHeight - 2, 2, "ENTER: aplicar y volver | ESC: cancelar");
        
        refresh();
        
        int ch = getch();
        
        if (ch == KEY_UP) {
            selectedSetting = (selectedSetting - 1 + 3) % 3;
        } else if (ch == KEY_DOWN) {
            selectedSetting = (selectedSetting + 1) % 3;
        } else if (ch == KEY_LEFT) {
            if (selectedSetting == 0) {
                numPlayers = (numPlayers - 1 + 3) % 3;
            } else if (selectedSetting == 1) {
                numBoards = std::max(1, numBoards - 1);
            }
        } else if (ch == KEY_RIGHT) {
            if (selectedSetting == 0) {
                numPlayers = (numPlayers + 1) % 3;
            } else if (selectedSetting == 1) {
                numBoards = std::min(12, numBoards + 1);
            }
        } else if (ch == '\n' || ch == KEY_ENTER || ch == 10) {
            if (selectedSetting == 2) {
                // Aplicar cambios
                gameManager.setGameMode((GameMode)numPlayers);
                gameManager.setNumBoards(numBoards);
                return;
            }
        } else if (ch == 27) { // ESC
            return;
        }
    }
}

void UI::showHelp() {
    clear();
    updateScreenSize();
    
    std::string title = "=== AYUDA ===";
    attron(COLOR_PAIR(1) | A_BOLD);
    mvprintw(1, (screenWidth - title.length()) / 2, "%s", title.c_str());
    attroff(COLOR_PAIR(1) | A_BOLD);
    
    int y = 3;
    
    mvprintw(y++, 2, "CONTROLES:");
    mvprintw(y++, 4, "- Flechas: Navegar por el menu y mover seleccion en el tablero");
    mvprintw(y++, 4, "- TAB: Cambiar entre tableros");
    mvprintw(y++, 4, "- ENTER o ESPACIO: Realizar jugada en la casilla seleccionada");
    mvprintw(y++, 4, "- R: Reiniciar tablero actual");
    mvprintw(y++, 4, "- ESC o Q: Volver al menu principal");
    mvprintw(y++, 4, "- Raton: Clic en casillas para jugar directamente");
    y++;
    
    mvprintw(y++, 2, "MODOS DE JUEGO:");
    mvprintw(y++, 4, "- 0 jugadores: Todos los tableros juegan automaticamente.");
    mvprintw(y++, 4, "  Las jugadas son completamente aleatorias.");
    y++;
    mvprintw(y++, 4, "- 1 jugador: El jugador controla tanto X como O.");
    mvprintw(y++, 4, "  Debe jugar manualmente en todos los tableros.");
    mvprintw(y++, 4, "  Cada tablero alterna X -> O -> X -> O estrictamente.");
    y++;
    mvprintw(y++, 4, "- 2 jugadores: El jugador controla O, X se genera automaticamente.");
    mvprintw(y++, 4, "  Tras cada jugada de O, se genera una X aleatoria.");
    y++;
    
    mvprintw(y++, 2, "REGLAS:");
    mvprintw(y++, 4, "- Cada tablero es independiente con su propio estado y turnos.");
    mvprintw(y++, 4, "- Gana quien consigue 3 en linea (fila, columna o diagonal).");
    mvprintw(y++, 4, "- Si se llena el tablero sin ganador, es empate.");
    mvprintw(y++, 4, "- Los tableros se reinician automaticamente tras cada partida.");
    y++;
    
    mvprintw(y++, 2, "ESTADISTICAS:");
    mvprintw(y++, 4, "- Cada tablero muestra victorias de X, O y empates.");
    mvprintw(y++, 4, "- Las estadisticas se mantienen durante la sesion.");
    
    mvprintw(screenHeight - 2, 2, "Presiona cualquier tecla para volver...");
    
    refresh();
    getch();
}

char UI::getCellSymbol(CellState state) const {
    switch (state) {
        case CellState::X: return 'X';
        case CellState::O: return 'O';
        default: return ' ';
    }
}

void UI::calculateBoardLayout(int numBoards, int& rows, int& cols) {
    // Calcular el mejor layout para los tableros
    cols = (int)ceil(sqrt(numBoards));
    rows = (int)ceil((double)numBoards / cols);
}

bool UI::getBoardCoordinates(int boardIndex, int& startY, int& startX, int& width, int& height) {
    int numBoards = gameManager.getNumBoards();
    int rows, cols;
    calculateBoardLayout(numBoards, rows, cols);
    
    int boardRow = boardIndex / cols;
    int boardCol = boardIndex % cols;
    
    // Reservar espacio para estadísticas en la parte inferior
    int gameHeight = screenHeight - 8;
    
    width = screenWidth / cols;
    height = gameHeight / rows;
    
    // Mínimo tamaño para mostrar el tablero
    if (width < 15 || height < 10) {
        return false;
    }
    
    startX = boardCol * width;
    startY = boardRow * height;
    
    return true;
}

void UI::drawBoard(int boardIndex, int startY, int startX, int width, int height, bool selected) {
    const Board& board = gameManager.getBoard(boardIndex);
    
    // Calcular centro del área
    int centerY = startY + height / 2;
    int centerX = startX + width / 2;
    
    // Dibujar marco del tablero si está seleccionado
    if (selected) {
        attron(COLOR_PAIR(4) | A_BOLD);
        mvprintw(startY, startX, "Tablero %d [ACTIVO]", boardIndex + 1);
        attroff(COLOR_PAIR(4) | A_BOLD);
    } else {
        mvprintw(startY, startX, "Tablero %d", boardIndex + 1);
    }
    
    // Dibujar el tablero 3x3
    int boardStartY = centerY - 4;
    int boardStartX = centerX - 6;
    
    // Dibujar líneas del tablero
    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 3; j++) {
            int y = boardStartY + i * 2;
            int x = boardStartX + j * 4;
            
            // Resaltar celda seleccionada
            bool cellSelected = selected && 
                               gameManager.getSelectedBoard() == boardIndex &&
                               gameManager.getSelectedRow() == i &&
                               gameManager.getSelectedCol() == j;
            
            if (cellSelected) {
                attron(COLOR_PAIR(4) | A_BOLD);
            }
            
            CellState cell = board.getCell(i, j);
            char symbol = getCellSymbol(cell);
            
            // Color según el símbolo
            if (cell == CellState::X) {
                attron(COLOR_PAIR(2));
            } else if (cell == CellState::O) {
                attron(COLOR_PAIR(3));
            }
            
            mvprintw(y, x, "[ %c ]", symbol);
            
            if (cell == CellState::X || cell == CellState::O) {
                attroff(COLOR_PAIR(cell == CellState::X ? 2 : 3));
            }
            
            if (cellSelected) {
                attroff(COLOR_PAIR(4) | A_BOLD);
            }
        }
    }
    
    // Mostrar estado del juego
    GameState state = board.getState();
    int statusY = boardStartY + 7;
    
    if (state == GameState::X_WINS) {
        attron(COLOR_PAIR(6) | A_BOLD);
        mvprintw(statusY, boardStartX, "  X GANA!  ");
        attroff(COLOR_PAIR(6) | A_BOLD);
    } else if (state == GameState::O_WINS) {
        attron(COLOR_PAIR(6) | A_BOLD);
        mvprintw(statusY, boardStartX, "  O GANA!  ");
        attroff(COLOR_PAIR(6) | A_BOLD);
    } else if (state == GameState::DRAW) {
        attron(COLOR_PAIR(1));
        mvprintw(statusY, boardStartX, "  EMPATE  ");
        attroff(COLOR_PAIR(1));
    } else {
        // Mostrar turno actual
        CellState turn = board.getCurrentTurn();
        if (turn == CellState::X) {
            attron(COLOR_PAIR(2));
            mvprintw(statusY, boardStartX, " Turno: X ");
        } else {
            attron(COLOR_PAIR(3));
            mvprintw(statusY, boardStartX, " Turno: O ");
        }
        attroff(COLOR_PAIR(turn == CellState::X ? 2 : 3));
    }
}

void UI::drawAllBoards() {
    int numBoards = gameManager.getNumBoards();
    
    for (int i = 0; i < numBoards; i++) {
        int startY, startX, width, height;
        if (getBoardCoordinates(i, startY, startX, width, height)) {
            bool selected = (i == gameManager.getSelectedBoard());
            drawBoard(i, startY, startX, width, height, selected);
        }
    }
}

void UI::drawStats() {
    int statsY = screenHeight - 6;
    int numBoards = gameManager.getNumBoards();
    
    mvprintw(statsY, 2, "ESTADISTICAS:");
    
    int totalXWins = 0, totalOWins = 0, totalDraws = 0, totalGames = 0;
    
    for (int i = 0; i < numBoards; i++) {
        const BoardStats& stats = gameManager.getStats(i);
        totalXWins += stats.xWins;
        totalOWins += stats.oWins;
        totalDraws += stats.draws;
        totalGames += stats.gamesPlayed;
    }
    
    mvprintw(statsY + 1, 2, "Total - X: %d | O: %d | Empates: %d | Partidas: %d",
             totalXWins, totalOWins, totalDraws, totalGames);
}

void UI::drawTurnInfo() {
    int infoY = screenHeight - 4;
    
    GameMode mode = gameManager.getGameMode();
    std::string modeStr;
    
    if (mode == GameMode::ZERO_PLAYERS) {
        modeStr = "0 jugadores (Auto)";
    } else if (mode == GameMode::ONE_PLAYER) {
        modeStr = "1 jugador (Manual)";
    } else {
        modeStr = "2 jugadores (O manual, X auto)";
    }
    
    mvprintw(infoY, 2, "Modo: %s", modeStr.c_str());
}

void UI::handleMouseClickGame(int y, int x) {
    int numBoards = gameManager.getNumBoards();
    
    for (int i = 0; i < numBoards; i++) {
        int startY, startX, width, height;
        if (!getBoardCoordinates(i, startY, startX, width, height)) {
            continue;
        }
        
        // Verificar si el clic está dentro de este tablero
        if (y >= startY && y < startY + height && x >= startX && x < startX + width) {
            gameManager.selectBoard(i);
            
            // Calcular posición dentro del tablero
            int centerY = startY + height / 2;
            int centerX = startX + width / 2;
            int boardStartY = centerY - 4;
            int boardStartX = centerX - 6;
            
            // Calcular celda
            int relY = y - boardStartY;
            int relX = x - boardStartX;
            
            int row = relY / 2;
            int col = relX / 4;
            
            if (row >= 0 && row < 3 && col >= 0 && col < 3) {
                gameManager.selectCell(row, col);
                gameManager.makeMove();
            }
            
            break;
        }
    }
}

void UI::playGame() {
    gameManager.resetAll();
    
    // Configurar timeout para actualizaciones automáticas
    timeout(100); // 100ms
    
    while (true) {
        clear();
        updateScreenSize();
        
        // Dibujar todos los tableros
        drawAllBoards();
        
        // Dibujar estadísticas
        drawStats();
        
        // Dibujar información del turno
        drawTurnInfo();
        
        // Instrucciones
        mvprintw(screenHeight - 2, 2, "Flechas:mover | TAB:cambiar tablero | ENTER:jugar | R:reiniciar | ESC:salir");
        
        refresh();
        
        // Actualizar tableros automáticos en modo 0 jugadores
        gameManager.updateAutoBoards();
        
        int ch = getch();
        
        if (ch == ERR) {
            // Timeout, continuar el bucle para actualizar tableros automáticos
            continue;
        }
        
        if (ch == 27 || ch == 'q' || ch == 'Q') { // ESC
            break;
        } else if (ch == KEY_UP) {
            gameManager.moveSelection(-1, 0);
        } else if (ch == KEY_DOWN) {
            gameManager.moveSelection(1, 0);
        } else if (ch == KEY_LEFT) {
            gameManager.moveSelection(0, -1);
        } else if (ch == KEY_RIGHT) {
            gameManager.moveSelection(0, 1);
        } else if (ch == '\t') { // TAB
            int nextBoard = (gameManager.getSelectedBoard() + 1) % gameManager.getNumBoards();
            gameManager.selectBoard(nextBoard);
        } else if (ch == '\n' || ch == KEY_ENTER || ch == 10 || ch == ' ') {
            gameManager.makeMove();
        } else if (ch == 'r' || ch == 'R') {
            gameManager.resetBoard(gameManager.getSelectedBoard());
        } else if (ch == KEY_MOUSE) {
            MEVENT event;
            if (getmouse(&event) == OK) {
                if (event.bstate & BUTTON1_CLICKED) {
                    handleMouseClickGame(event.y, event.x);
                }
            }
        }
        
        // Verificar si todos los tableros han terminado en modo 0 jugadores
        if (gameManager.getGameMode() == GameMode::ZERO_PLAYERS) {
            bool allFinished = true;
            for (int i = 0; i < gameManager.getNumBoards(); i++) {
                if (gameManager.getBoard(i).getState() == GameState::PLAYING) {
                    allFinished = false;
                    break;
                }
            }
            
            if (allFinished) {
                // Esperar un momento antes de reiniciar
                timeout(-1);
                clear();
                drawAllBoards();
                drawStats();
                mvprintw(screenHeight / 2, (screenWidth - 40) / 2, "Todas las partidas terminaron!");
                mvprintw(screenHeight / 2 + 1, (screenWidth - 40) / 2, "Presiona cualquier tecla para reiniciar...");
                refresh();
                getch();
                gameManager.resetAll();
                timeout(100);
            }
        }
    }
    
    timeout(-1); // Restaurar comportamiento bloqueante
}

void UI::run() {
    init();
    showMainMenu();
    cleanup();
}
