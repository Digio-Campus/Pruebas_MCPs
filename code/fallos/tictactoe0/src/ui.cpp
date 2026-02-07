#include "../include/ui.h"
#include <string>
#include <cmath>
#include <algorithm>

UI::UI() : mouse_enabled(false), term_width(80), term_height(24) {}

UI::~UI() {
    cleanup();
}

void UI::init() {
    initscr();
    cbreak();
    noecho();
    keypad(stdscr, TRUE);
    curs_set(0);
    
    // Habilitar ratón
    mousemask(ALL_MOUSE_EVENTS | REPORT_MOUSE_POSITION, NULL);
    mouse_enabled = true;
    
    // Inicializar colores si están disponibles
    if (has_colors()) {
        start_color();
        init_pair(1, COLOR_CYAN, COLOR_BLACK);    // X
        init_pair(2, COLOR_YELLOW, COLOR_BLACK);  // O
        init_pair(3, COLOR_GREEN, COLOR_BLACK);   // Ganador
        init_pair(4, COLOR_RED, COLOR_BLACK);     // Empate
    }
    
    getmaxyx(stdscr, term_height, term_width);
}

void UI::cleanup() {
    if (!isendwin()) {
        endwin();
    }
}

void UI::calculateLayout(int num_boards, int& cols, int& rows) {
    // Cada tablero necesita aproximadamente 15 caracteres de ancho y 9 de alto
    int board_width = 15;
    int board_height = 9;
    
    cols = std::max(1, term_width / board_width);
    rows = (num_boards + cols - 1) / cols;
    
    // Ajustar si no caben verticalmente
    while (rows * board_height > term_height - 3 && cols < num_boards) {
        cols++;
        rows = (num_boards + cols - 1) / cols;
    }
}

void UI::drawBoard(const Board& board, int x, int y, int board_num, bool highlight) {
    // Título del tablero
    attron(A_BOLD);
    if (highlight) attron(COLOR_PAIR(3));
    mvprintw(y, x, "Tablero %d", board_num + 1);
    if (highlight) attroff(COLOR_PAIR(3));
    attroff(A_BOLD);
    
    // Dibujar el tablero
    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 3; j++) {
            int cell_y = y + 2 + i * 2;
            int cell_x = x + j * 4;
            
            char cell = board.getCell(i, j);
            
            if (cell == 'X') {
                attron(COLOR_PAIR(1));
                mvprintw(cell_y, cell_x, " X ");
                attroff(COLOR_PAIR(1));
            } else if (cell == 'O') {
                attron(COLOR_PAIR(2));
                mvprintw(cell_y, cell_x, " O ");
                attroff(COLOR_PAIR(2));
            } else {
                mvprintw(cell_y, cell_x, " . ");
            }
            
            if (j < 2) mvprintw(cell_y, cell_x + 3, "|");
        }
        if (i < 2) {
            mvprintw(y + 3 + i * 2, x, "---+---+---");
        }
    }
    
    // Mostrar resultado si el juego ha terminado
    if (board.isGameOver()) {
        int result_y = y + 8;
        char winner = board.getWinner();
        
        if (winner == 'X') {
            attron(COLOR_PAIR(1) | A_BOLD);
            mvprintw(result_y, x, "X GANA!");
            attroff(COLOR_PAIR(1) | A_BOLD);
        } else if (winner == 'O') {
            attron(COLOR_PAIR(2) | A_BOLD);
            mvprintw(result_y, x, "O GANA!");
            attroff(COLOR_PAIR(2) | A_BOLD);
        } else {
            attron(COLOR_PAIR(4));
            mvprintw(result_y, x, "EMPATE");
            attroff(COLOR_PAIR(4));
        }
    }
}

void UI::renderGame(const Game& game) {
    clear();
    
    int num_boards = game.getNumBoards();
    int cols, rows;
    calculateLayout(num_boards, cols, rows);
    
    int board_width = 15;
    int board_height = 9;
    
    // Dibujar todos los tableros
    for (int i = 0; i < num_boards; i++) {
        int row = i / cols;
        int col = i % cols;
        
        int x = col * board_width + 2;
        int y = row * board_height + 1;
        
        drawBoard(game.getBoard(i), x, y, i, false);
    }
    
    // Mostrar información del juego en la parte inferior
    int info_y = term_height - 2;
    
    int score_x, score_o, draws;
    game.getScores(score_x, score_o, draws);
    
    attron(A_BOLD);
    mvprintw(info_y, 2, "Turno: %c", game.getCurrentPlayer());
    mvprintw(info_y, 15, "| X: %d", score_x);
    mvprintw(info_y, 25, "| O: %d", score_o);
    mvprintw(info_y, 35, "| Empates: %d", draws);
    mvprintw(info_y, 52, "| ESC/Q: Salir");
    attroff(A_BOLD);
    
    refresh();
}

bool UI::getClickPosition(int mouse_y, int mouse_x, int num_boards, 
                         int& board_idx, int& row, int& col) {
    int cols_layout, rows_layout;
    calculateLayout(num_boards, cols_layout, rows_layout);
    
    int board_width = 15;
    int board_height = 9;
    
    for (int i = 0; i < num_boards; i++) {
        int board_row = i / cols_layout;
        int board_col = i % cols_layout;
        
        int x = board_col * board_width + 2;
        int y = board_row * board_height + 1;
        
        // Verificar si el clic está dentro de este tablero
        if (mouse_y >= y + 2 && mouse_y <= y + 6 &&
            mouse_x >= x && mouse_x <= x + 11) {
            
            board_idx = i;
            
            // Calcular fila y columna
            int relative_y = mouse_y - (y + 2);
            int relative_x = mouse_x - x;
            
            row = relative_y / 2;
            col = relative_x / 4;
            
            if (row >= 0 && row < 3 && col >= 0 && col < 3) {
                return true;
            }
        }
    }
    
    return false;
}

bool UI::processGameInput(Game& game) {
    int ch = getch();
    
    if (ch == 27 || ch == 'q' || ch == 'Q') {
        return false;
    }
    
    if (ch == KEY_RESIZE) {
        handleResize();
        return true;
    }
    
    if (ch == KEY_MOUSE) {
        MEVENT event;
        if (getmouse(&event) == OK) {
            if (event.bstate & BUTTON1_CLICKED) {
                int board_idx, row, col;
                if (getClickPosition(event.y, event.x, game.getNumBoards(),
                                    board_idx, row, col)) {
                    
                    // Solo procesar si el jugador actual es humano
                    const GameConfig& cfg = game.getConfig();
                    char current = game.getCurrentPlayer();
                    
                    if ((cfg.num_players == 1 && current == 'X') ||
                        (cfg.num_players == 2 && current == 'O')) {
                        game.processMove(board_idx, row, col);
                    }
                }
            }
        }
    }
    
    // Procesar movimientos automáticos y actualizar estado
    game.processAutoMoves();
    game.update();
    
    return true;
}

void UI::showMessage(const std::string& msg, int delay_ms) {
    clear();
    mvprintw(LINES / 2, (COLS - msg.length()) / 2, "%s", msg.c_str());
    refresh();
    
    if (delay_ms > 0) {
        napms(delay_ms);
    }
}

void UI::clear() {
    ::clear();
}

void UI::refresh() {
    ::refresh();
}

void UI::getTerminalSize(int& width, int& height) {
    getmaxyx(stdscr, height, width);
    term_height = height;
    term_width = width;
}

void UI::handleResize() {
    endwin();
    refresh();
    getmaxyx(stdscr, term_height, term_width);
    clear();
}
