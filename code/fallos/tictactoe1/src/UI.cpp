#include "../include/UI.h"
#include "../include/Settings.h"
#include <sstream>
#include <algorithm>

UI::UI() : mouseEnabled(false), maxY(0), maxX(0) {}

UI::~UI() {
    cleanup();
}

bool UI::init() {
    // Inicializar ncurses
    initscr();
    
    // Configuración básica
    cbreak();              // Deshabilitar buffering de línea
    noecho();              // No mostrar caracteres escritos
    keypad(stdscr, TRUE);  // Habilitar teclas especiales (flechas, F1, etc.)
    curs_set(0);           // Ocultar cursor
    
    // Intentar habilitar ratón
    if (mousemask(ALL_MOUSE_EVENTS | REPORT_MOUSE_POSITION, NULL)) {
        mouseEnabled = true;
    }
    
    // Inicializar colores si están disponibles
    if (has_colors()) {
        start_color();
        initColors();
    }
    
    // Obtener tamaño de la terminal
    getmaxyx(stdscr, maxY, maxX);
    
    return true;
}

void UI::initColors() {
    // Definir pares de colores
    init_pair(1, COLOR_CYAN, COLOR_BLACK);    // Texto normal
    init_pair(2, COLOR_YELLOW, COLOR_BLACK);  // Seleccionado
    init_pair(3, COLOR_GREEN, COLOR_BLACK);   // X
    init_pair(4, COLOR_RED, COLOR_BLACK);     // O
    init_pair(5, COLOR_WHITE, COLOR_BLUE);    // Destacado
    init_pair(6, COLOR_MAGENTA, COLOR_BLACK); // Título
}

void UI::cleanup() {
    endwin();
}

void UI::clearScreen() {
    clear();
}

void UI::refresh() {
    ::refresh();
}

void UI::getTerminalSize(int& rows, int& cols) {
    getmaxyx(stdscr, rows, cols);
    maxY = rows;
    maxX = cols;
}

void UI::drawMenu(const std::vector<std::string>& options, int selected, const std::string& title) {
    clearScreen();
    
    int rows, cols;
    getTerminalSize(rows, cols);
    
    // Dibujar título centrado
    int titleY = rows / 2 - options.size() - 2;
    attron(COLOR_PAIR(6) | A_BOLD);
    showCenteredMessage(title, titleY);
    attroff(COLOR_PAIR(6) | A_BOLD);
    
    // Dibujar opciones
    for (size_t i = 0; i < options.size(); i++) {
        int y = rows / 2 - options.size() / 2 + i;
        int x = cols / 2 - options[i].length() / 2;
        
        if ((int)i == selected) {
            attron(COLOR_PAIR(2) | A_BOLD);
            mvprintw(y, x - 2, "> %s <", options[i].c_str());
            attroff(COLOR_PAIR(2) | A_BOLD);
        } else {
            attron(COLOR_PAIR(1));
            mvprintw(y, x, "%s", options[i].c_str());
            attroff(COLOR_PAIR(1));
        }
    }
    
    // Instrucciones
    attron(COLOR_PAIR(1) | A_DIM);
    showCenteredMessage("Usa flechas y Enter para navegar", rows - 2);
    attroff(COLOR_PAIR(1) | A_DIM);
    
    refresh();
}

void UI::drawBoards(const std::vector<Board*>& boards, int selectedBoard, 
                    int cursorRow, int cursorCol, const Settings& settings) {
    clearScreen();
    
    int rows, cols;
    getTerminalSize(rows, cols);
    
    int numBoards = boards.size();
    
    // Calcular disposición de tableros (filas x columnas)
    int boardCols = (numBoards <= 3) ? numBoards : 3;
    int boardRows = (numBoards + boardCols - 1) / boardCols;
    
    // Dimensiones de cada tablero (ajustado al tamaño de terminal)
    int boardWidth = 15;   // Ancho de un tablero
    int boardHeight = 7;   // Alto de un tablero
    
    // Espaciado
    int spacingX = 2;
    int spacingY = 1;
    
    // Calcular punto de inicio para centrar
    int totalWidth = boardCols * boardWidth + (boardCols - 1) * spacingX;
    int totalHeight = boardRows * boardHeight + (boardRows - 1) * spacingY;
    int startY = std::max(1, (rows - totalHeight) / 2 - 3);
    int startX = std::max(1, (cols - totalWidth) / 2);
    
    // Título
    attron(COLOR_PAIR(6) | A_BOLD);
    showCenteredMessage("=== TICTACTOE ===", 0);
    attroff(COLOR_PAIR(6) | A_BOLD);
    
    // Dibujar cada tablero
    for (int i = 0; i < numBoards; i++) {
        int boardRow = i / boardCols;
        int boardCol = i % boardCols;
        
        int y = startY + boardRow * (boardHeight + spacingY);
        int x = startX + boardCol * (boardWidth + spacingX);
        
        bool isSelected = (i == selectedBoard);
        drawSingleBoard(boards[i], y, x, isSelected, cursorRow, cursorCol);
    }
    
    // Información de modo
    int infoY = startY + totalHeight + 2;
    attron(COLOR_PAIR(1));
    std::string modeStr = "Modo: ";
    switch (settings.getNumPlayers()) {
        case 0: modeStr += "0 jugadores (Auto)"; break;
        case 1: modeStr += "1 jugador (Manual)"; break;
        case 2: modeStr += "2 jugadores (vs Auto)"; break;
    }
    showCenteredMessage(modeStr, infoY);
    attroff(COLOR_PAIR(1));
    
    // Estadísticas
    drawStats(boards, infoY + 2);
    
    // Controles
    attron(COLOR_PAIR(1) | A_DIM);
    showCenteredMessage("Flechas: mover | Enter: jugar | Tab: cambiar tablero | R: reiniciar | Q: salir", rows - 1);
    attroff(COLOR_PAIR(1) | A_DIM);
    
    refresh();
}

void UI::drawSingleBoard(const Board* board, int startY, int startX, 
                        bool isSelected, int cursorRow, int cursorCol) {
    // Marco del tablero
    if (isSelected) {
        attron(COLOR_PAIR(2) | A_BOLD);
    } else {
        attron(COLOR_PAIR(1));
    }
    
    // ID del tablero
    mvprintw(startY, startX + 4, "Tablero %d", board->getBoardId());
    
    // Turno actual
    if (!board->isGameOver()) {
        mvprintw(startY + 1, startX + 3, "Turno: %s", board->getCurrentTurnSymbol().c_str());
    } else {
        // Mostrar resultado
        switch (board->getResult()) {
            case X_WINS:
                attron(COLOR_PAIR(3));
                mvprintw(startY + 1, startX + 3, "Gana X!");
                attroff(COLOR_PAIR(3));
                break;
            case O_WINS:
                attron(COLOR_PAIR(4));
                mvprintw(startY + 1, startX + 3, "Gana O!");
                attroff(COLOR_PAIR(4));
                break;
            case DRAW:
                mvprintw(startY + 1, startX + 3, "Empate!");
                break;
            default:
                break;
        }
    }
    
    if (isSelected) {
        attroff(COLOR_PAIR(2) | A_BOLD);
    } else {
        attroff(COLOR_PAIR(1));
    }
    
    // Dibujar el tablero 3x3
    int boardStartY = startY + 2;
    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 3; j++) {
            int cellY = boardStartY + i;
            int cellX = startX + j * 4 + 1;
            
            // Resaltar celda del cursor si este tablero está seleccionado
            bool isCursor = isSelected && (i == cursorRow) && (j == cursorCol);
            
            if (isCursor) {
                attron(COLOR_PAIR(5) | A_BOLD);
            }
            
            // Obtener símbolo de la celda
            std::string symbol = board->getCellSymbol(i, j);
            
            // Colorear X y O
            if (symbol == "X") {
                attron(COLOR_PAIR(3) | A_BOLD);
                mvprintw(cellY, cellX, " %s ", symbol.c_str());
                attroff(COLOR_PAIR(3) | A_BOLD);
            } else if (symbol == "O") {
                attron(COLOR_PAIR(4) | A_BOLD);
                mvprintw(cellY, cellX, " %s ", symbol.c_str());
                attroff(COLOR_PAIR(4) | A_BOLD);
            } else {
                mvprintw(cellY, cellX, " %s ", symbol.c_str());
            }
            
            if (isCursor) {
                attroff(COLOR_PAIR(5) | A_BOLD);
            }
            
            // Separadores verticales
            if (j < 2) {
                mvprintw(cellY, cellX + 3, "|");
            }
        }
        
        // Separadores horizontales
        if (i < 2) {
            mvprintw(boardStartY + i + 1, startX, "   ---+---+---");
        }
    }
}

void UI::drawStats(const std::vector<Board*>& boards, int startY) {
    int totalXWins = 0, totalOWins = 0, totalDraws = 0;
    
    for (const auto& board : boards) {
        totalXWins += board->getXWins();
        totalOWins += board->getOWins();
        totalDraws += board->getDraws();
    }
    
    std::stringstream ss;
    ss << "Estadísticas totales - X: " << totalXWins 
       << " | O: " << totalOWins 
       << " | Empates: " << totalDraws;
    
    attron(COLOR_PAIR(1));
    showCenteredMessage(ss.str(), startY);
    attroff(COLOR_PAIR(1));
}

void UI::drawHelp() {
    clearScreen();
    
    int rows, cols;
    getTerminalSize(rows, cols);
    
    attron(COLOR_PAIR(6) | A_BOLD);
    showCenteredMessage("=== AYUDA - TICTACTOE ===", 2);
    attroff(COLOR_PAIR(6) | A_BOLD);
    
    attron(COLOR_PAIR(1));
    
    int y = 4;
    mvprintw(y++, 5, "REGLAS DEL JUEGO:");
    mvprintw(y++, 7, "- Alinea tres símbolos iguales (horizontal, vertical o diagonal)");
    mvprintw(y++, 7, "- Cada tablero es independiente con su propio turno");
    mvprintw(y++, 7, "- Alternancia estricta: X -> O -> X -> O en cada tablero");
    y++;
    
    mvprintw(y++, 5, "MODOS DE JUEGO:");
    mvprintw(y++, 7, "0 jugadores: Todos los tableros se llenan automáticamente (aleatorio)");
    mvprintw(y++, 7, "1 jugador:   Controlas X y O manualmente, respetando turnos alternos");
    mvprintw(y++, 7, "2 jugadores: Controlas O, X se genera automáticamente tras cada turno");
    y++;
    
    mvprintw(y++, 5, "CONTROLES:");
    mvprintw(y++, 7, "Flechas:     Mover cursor dentro del tablero seleccionado");
    mvprintw(y++, 7, "Enter:       Colocar ficha en la posición del cursor");
    mvprintw(y++, 7, "Tab:         Cambiar al siguiente tablero");
    mvprintw(y++, 7, "R:           Reiniciar el tablero seleccionado");
    mvprintw(y++, 7, "Q:           Salir del juego");
    mvprintw(y++, 7, "Ratón:       Clic en casilla para jugar (si está disponible)");
    y++;
    
    mvprintw(y++, 5, "TABLEROS MÚLTIPLES:");
    mvprintw(y++, 7, "- Puedes configurar de 1 a 9 tableros simultáneos");
    mvprintw(y++, 7, "- Cada tablero mantiene su propio estado y estadísticas");
    mvprintw(y++, 7, "- Los tableros se adaptan al tamaño de la terminal");
    
    attroff(COLOR_PAIR(1));
    
    attron(COLOR_PAIR(2) | A_BOLD);
    showCenteredMessage("Presiona cualquier tecla para volver", rows - 2);
    attroff(COLOR_PAIR(2) | A_BOLD);
    
    refresh();
}

void UI::drawSettingsMenu(const Settings& settings, int selected) {
    clearScreen();
    
    int rows, cols;
    getTerminalSize(rows, cols);
    
    attron(COLOR_PAIR(6) | A_BOLD);
    showCenteredMessage("=== AJUSTES ===", 2);
    attroff(COLOR_PAIR(6) | A_BOLD);
    
    int y = rows / 2 - 2;
    
    // Opción 1: Número de jugadores
    if (selected == 0) {
        attron(COLOR_PAIR(2) | A_BOLD);
        mvprintw(y, cols / 2 - 20, "> Numero de jugadores: %d <", settings.getNumPlayers());
        attroff(COLOR_PAIR(2) | A_BOLD);
    } else {
        attron(COLOR_PAIR(1));
        mvprintw(y, cols / 2 - 18, "  Numero de jugadores: %d  ", settings.getNumPlayers());
        attroff(COLOR_PAIR(1));
    }
    
    y += 2;
    
    // Opción 2: Número de tableros
    if (selected == 1) {
        attron(COLOR_PAIR(2) | A_BOLD);
        mvprintw(y, cols / 2 - 20, "> Numero de tableros: %d <", settings.getNumBoards());
        attroff(COLOR_PAIR(2) | A_BOLD);
    } else {
        attron(COLOR_PAIR(1));
        mvprintw(y, cols / 2 - 18, "  Numero de tableros: %d  ", settings.getNumBoards());
        attroff(COLOR_PAIR(1));
    }
    
    y += 2;
    
    // Opción 3: Volver
    if (selected == 2) {
        attron(COLOR_PAIR(2) | A_BOLD);
        showCenteredMessage("> Volver al menu principal <", y);
        attroff(COLOR_PAIR(2) | A_BOLD);
    } else {
        attron(COLOR_PAIR(1));
        showCenteredMessage("  Volver al menu principal  ", y);
        attroff(COLOR_PAIR(1));
    }
    
    // Descripción del modo seleccionado
    attron(COLOR_PAIR(1) | A_DIM);
    y = rows / 2 + 5;
    switch (settings.getNumPlayers()) {
        case 0:
            showCenteredMessage("Modo 0: Todos los tableros se llenan automáticamente", y);
            break;
        case 1:
            showCenteredMessage("Modo 1: Controlas X y O manualmente en todos los tableros", y);
            break;
        case 2:
            showCenteredMessage("Modo 2: Controlas O, X se genera automáticamente", y);
            break;
    }
    attroff(COLOR_PAIR(1) | A_DIM);
    
    // Instrucciones
    attron(COLOR_PAIR(1) | A_DIM);
    showCenteredMessage("Flechas: navegar | Izquierda/Derecha: cambiar valor | Enter: confirmar", rows - 2);
    attroff(COLOR_PAIR(1) | A_DIM);
    
    refresh();
}

void UI::showMessage(const std::string& message, int y, int x) {
    mvprintw(y, x, "%s", message.c_str());
}

void UI::showCenteredMessage(const std::string& message, int y) {
    int rows, cols;
    getTerminalSize(rows, cols);
    int x = cols / 2 - message.length() / 2;
    mvprintw(y, x, "%s", message.c_str());
}

int UI::getInput() {
    return getch();
}

bool UI::getMouseEvent(int& y, int& x, int& button) {
    MEVENT event;
    if (getmouse(&event) == OK) {
        y = event.y;
        x = event.x;
        button = event.bstate;
        return true;
    }
    return false;
}

bool UI::mouseToCell(int mouseY, int mouseX, int boardStartY, int boardStartX,
                     int& row, int& col) {
    // Calcular posición relativa al tablero
    int relY = mouseY - boardStartY - 2;  // -2 por el encabezado del tablero
    int relX = mouseX - boardStartX - 1;
    
    // Verificar si está dentro del área del tablero
    if (relY < 0 || relY >= 5 || relX < 0 || relX >= 13) {
        return false;
    }
    
    // Calcular fila (evitar líneas divisorias)
    if (relY == 1 || relY == 3) {
        return false;  // Línea divisoria horizontal
    }
    row = (relY > 3) ? 2 : (relY > 1) ? 1 : 0;
    
    // Calcular columna (evitar líneas divisorias)
    if (relX == 3 || relX == 7) {
        return false;  // Línea divisoria vertical
    }
    col = (relX > 7) ? 2 : (relX > 3) ? 1 : 0;
    
    return true;
}

void UI::waitForKey() {
    getch();
}
