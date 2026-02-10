#include "ui.h"
#include <cstring>
#include <cstdio>
#include <algorithm>

// Constructor
UI::UI(Game* gameInstance) : mainWindow(nullptr), game(gameInstance) {
    state.screenWidth = 0;
    state.screenHeight = 0;
    state.selectedBoard = 0;
    state.cursorRow = 1;
    state.cursorCol = 1;
    state.resize = false;
}

// Destructor
UI::~UI() {
    cleanupNCurses();
}

// Inicializa ncurses
bool UI::initNCurses() {
    mainWindow = initscr();
    if (!mainWindow) {
        return false;
    }
    
    // Configuración de ncurses
    cbreak();                   // Desactiva buffering de línea
    noecho();                   // No muestra lo que se escribe
    curs_set(0);               // Oculta el cursor
    
    // Habilitar soporte para mouse
    mousemask(ALL_MOUSE_EVENTS, nullptr);
    
    // Inicializar colores
    if (has_colors()) {
        start_color();
        init_pair(1, COLOR_WHITE, COLOR_BLACK);      // Normal
        init_pair(2, COLOR_GREEN, COLOR_BLACK);      // X
        init_pair(3, COLOR_BLUE, COLOR_BLACK);       // O
        init_pair(4, COLOR_YELLOW, COLOR_BLACK);     // Cursor
        init_pair(5, COLOR_RED, COLOR_BLACK);        // Error
        init_pair(6, COLOR_CYAN, COLOR_BLACK);       // Información
    }
    
    getmaxyx(mainWindow, state.screenHeight, state.screenWidth);
    
    return true;
}

// Limpia ncurses
void UI::cleanupNCurses() {
    for (auto& window : boardWindows) {
        if (window) {
            delwin(window);
        }
    }
    boardWindows.clear();
    
    if (mainWindow) {
        endwin();
        mainWindow = nullptr;
    }
}

// Muestra el menú principal
int UI::showMainMenu() {
    clearScreen();
    
    int choice = 0;
    const char* menu[] = {
        "=== TIC TAC TOE ===",
        "",
        "1. JUGAR",
        "2. AJUSTES",
        "3. AYUDA",
        "4. SALIR",
        "",
        "Selecciona una opción (1-4):"
    };
    
    int startY = state.screenHeight / 2 - 5;
    int startX = state.screenWidth / 2 - 15;
    
    for (int i = 0; i < 8; i++) {
        mvprintw(startY + i, startX, "%s", menu[i]);
    }
    
    refresh();
    
    int ch;
    while (true) {
        ch = getch();
        if (ch >= '1' && ch <= '4') {
            choice = ch - '0';
            break;
        }
    }
    
    return choice;
}

// Muestra el menú de ajustes
void UI::showSettingsMenu(int& numPlayers, int& numBoards) {
    clearScreen();
    
    const char* header = "=== AJUSTES ===";
    const char* playersMenu[] = {
        "",
        "Número de Jugadores:",
        "1. 0 jugadores (Automático aleatorio)",
        "2. 1 jugador (Control manual X y O)",
        "3. 2 jugadores (Jugador=O, IA=X)",
        ""
    };
    
    mvprintw(2, state.screenWidth / 2 - 10, "%s", header);
    
    for (int i = 0; i < 6; i++) {
        mvprintw(4 + i, 5, "%s", playersMenu[i]);
    }
    
    mvprintw(11, 5, "Selecciona (1-3): ");
    refresh();
    
    while (true) {
        int ch = getch();
        if (ch >= '1' && ch <= '3') {
            numPlayers = ch - '1';
            break;
        }
    }
    
    clearScreen();
    
    const char* boardsMenu[] = {
        "",
        "Número de Tableros:",
        "1. 1 tablero",
        "2. 2 tableros (1x2)",
        "3. 4 tableros (2x2)",
        "4. 6 tableros (2x3)",
        "5. 9 tableros (3x3)",
        ""
    };
    
    mvprintw(2, state.screenWidth / 2 - 12, "%s", header);
    
    for (int i = 0; i < 8; i++) {
        mvprintw(4 + i, 5, "%s", boardsMenu[i]);
    }
    
    mvprintw(13, 5, "Selecciona (1-5): ");
    refresh();
    
    int boardChoices[] = {1, 2, 4, 6, 9};
    
    while (true) {
        int ch = getch();
        if (ch >= '1' && ch <= '5') {
            numBoards = boardChoices[ch - '1'];
            break;
        }
    }
}

// Muestra el menú de ayuda
void UI::showHelpMenu() {
    clearScreen();
    
    const char* helpText[] = {
        "=== AYUDA ===",
        "",
        "CONTROLES:",
        "  Flechas/WASD: Mover cursor",
        "  ENTER: Colocar marca",
        "  TAB: Cambiar entre tableros",
        "  R: Reiniciar tablero actual",
        "  ESC: Volver al menú",
        "",
        "MODOS DE JUEGO:",
        "  0 Jugadores: Tableros se rellenan automáticamente",
        "  1 Jugador: Controlas X y O manualmente",
        "  2 Jugadores: Tú eres O, la IA es X",
        "",
        "CADA TABLERO ES INDEPENDIENTE:",
        "  - Tu turno alterno en cada uno: X → O → X → O",
        "  - Las victorias se cuentan por tablero",
        "  - Los empates también se registran",
        "",
        "Presiona cualquier tecla para volver..."
    };
    
    for (int i = 0; i < 20; i++) {
        mvprintw(1 + i, 2, "%s", helpText[i]);
    }
    
    refresh();
    getch();
}

// Renderiza todo
void UI::render() {
    clear();
    renderAllBoards();
    renderStatusBar();
    refresh();
}

// Renderiza todos los tableros
void UI::renderAllBoards() {
    if (!game) return;
    
    int numBoards = game->getNumBoards();
    int boardWidth = 12;
    int boardHeight = 7;
    
    // Calcular layout
    int cols = (state.screenWidth - 2) / (boardWidth + 2);
    if (cols < 1) cols = 1;
    int rows = (numBoards + cols - 1) / cols;
    
    // Ajustar tamaño si es necesario
    if ((rows * (boardHeight + 1)) > (state.screenHeight - 3)) {
        boardHeight = std::max(5, (state.screenHeight - 3) / rows - 1);
    }
    
    for (int i = 0; i < numBoards; i++) {
        int row = (i / cols) * (boardHeight + 1) + 1;
        int col = (i % cols) * (boardWidth + 2) + 1;
        
        renderBoard(i, col, row, boardWidth, boardHeight);
    }
}

// Renderiza un tablero individual
void UI::renderBoard(int boardIndex, int startX, int startY, int /* width */, int /* height */) {
    if (!game || boardIndex >= game->getNumBoards()) return;
    
    Board* board = game->getBoard(boardIndex);
    if (!board) return;
    
    // Marco del tablero
    mvprintw(startY - 1, startX, "Board %d", boardIndex + 1);
    mvprintw(startY, startX, "┌─────────┐");
    
    for (int i = 0; i < 3; i++) {
        mvprintw(startY + 1 + i * 2, startX, "│");
        for (int j = 0; j < 3; j++) {
            int attr = A_NORMAL;
            
            // Resaltar cursor si es el tablero seleccionado
            if (boardIndex == state.selectedBoard && 
                i == state.cursorRow && j == state.cursorCol) {
                attr = A_REVERSE;
            }
            
            attron(attr);
            
            CellState cell = board->getCell(i, j);
            if (cell == EMPTY) {
                printw(" ");
            } else if (cell == PLAYER_X) {
                printw("X");
            } else {
                printw("O");
            }
            
            attroff(attr);
            
            if (j < 2) printw(" ");
        }
        mvprintw(startY + 1 + i * 2, startX + 10, "│");
        
        if (i < 2) {
            mvprintw(startY + 2 + i * 2, startX, "├─────────┤");
        }
    }
    
    mvprintw(startY + 7, startX, "└─────────┘");
    
    // Estadísticas
    mvprintw(startY + 8, startX, "X:%d O:%d E:%d", 
             board->getXWins(), board->getOWins(), board->getDraws());
}

// Renderiza la barra de estado
void UI::renderStatusBar() {
    mvprintw(state.screenHeight - 2, 2, 
             "Board: %d | Cursor: (%d,%d) | TAB: cambiar | R: reiniciar | ESC: menú",
             state.selectedBoard + 1, state.cursorRow, state.cursorCol);
}

// Maneja la entrada del usuario
int UI::handleInput(int ch) {
    if (ch == 27) {  // ESC
        return -1;
    }
    
    if (ch == '\t') {  // TAB - cambiar tablero
        state.selectedBoard = (state.selectedBoard + 1) % game->getNumBoards();
        return 0;
    }
    
    if (ch == 'r' || ch == 'R') {  // R - reiniciar tablero
        game->resetBoard(state.selectedBoard);
        return 0;
    }
    
    // Movimiento del cursor
    if (ch == KEY_UP || ch == 'w' || ch == 'W') {
        state.cursorRow = (state.cursorRow - 1 + 3) % 3;
        return 0;
    }
    if (ch == KEY_DOWN || ch == 's' || ch == 'S') {
        state.cursorRow = (state.cursorRow + 1) % 3;
        return 0;
    }
    if (ch == KEY_LEFT || ch == 'a' || ch == 'A') {
        state.cursorCol = (state.cursorCol - 1 + 3) % 3;
        return 0;
    }
    if (ch == KEY_RIGHT || ch == 'd' || ch == 'D') {
        state.cursorCol = (state.cursorCol + 1) % 3;
        return 0;
    }
    
    // ENTER - hacer movimiento
    if (ch == '\n' || ch == ' ') {
        game->makeMove(state.selectedBoard, state.cursorRow, state.cursorCol);
        return 0;
    }
    
    // Detección de redimensionamiento
    if (ch == KEY_RESIZE) {
        checkWindowResize();
        return 0;
    }
    
    return 0;
}

// Muestra un mensaje
void UI::displayMessage(const std::string& message) {
    mvprintw(state.screenHeight / 2, state.screenWidth / 2 - (int)message.length() / 2,
             "%s", message.c_str());
    refresh();
}

// Espera por una tecla
void UI::waitForKey() {
    getch();
}

// Verifica si la ventana fue redimensionada
bool UI::checkWindowResize() {
    int newWidth, newHeight;
    getmaxyx(mainWindow, newHeight, newWidth);
    
    if (newWidth != state.screenWidth || newHeight != state.screenHeight) {
        state.screenWidth = newWidth;
        state.screenHeight = newHeight;
        state.resize = true;
        return true;
    }
    
    state.resize = false;
    return false;
}

// Actualiza el tamaño de la ventana
void UI::updateWindowSize() {
    int newWidth, newHeight;
    getmaxyx(mainWindow, newHeight, newWidth);
    state.screenWidth = newWidth;
    state.screenHeight = newHeight;
}

// Calcula el layout de los tableros
std::pair<int, int> UI::calculateBoardLayout() {
    int numBoards = game->getNumBoards();
    int cols = 1;
    
    if (numBoards == 1) cols = 1;
    else if (numBoards == 2) cols = 2;
    else if (numBoards == 4) cols = 2;
    else if (numBoards == 6) cols = 3;
    else if (numBoards == 9) cols = 3;
    
    int rows = (numBoards + cols - 1) / cols;
    return {rows, cols};
}

// Limpia la pantalla
void UI::clearScreen() {
    clear();
    refresh();
}

// Getters
UIState UI::getState() const {
    return state;
}

int UI::getSelectedBoard() const {
    return state.selectedBoard;
}

int UI::getCursorRow() const {
    return state.cursorRow;
}

int UI::getCursorCol() const {
    return state.cursorCol;
}
