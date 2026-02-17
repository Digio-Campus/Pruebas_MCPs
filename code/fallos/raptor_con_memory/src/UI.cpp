#include "../include/UI.h"
#include "../include/Game.h"
#include <ncurses.h>
#include <string>
#include <sstream>
#include <unistd.h>

UI::UI(Game* g, Settings* s): game(g), settings(s), selBoard(0), cursorR(0), cursorC(0), screen(MENU) {}
UI::~UI() { endwin(); }

void UI::init() {
    initscr(); cbreak(); noecho(); keypad(stdscr, TRUE);
    curs_set(0);
    start_color();
    mousemask(ALL_MOUSE_EVENTS | REPORT_MOUSE_POSITION, NULL);
    init_pair(1, COLOR_CYAN, COLOR_BLACK);
    init_pair(2, COLOR_YELLOW, COLOR_BLACK);
    init_pair(3, COLOR_GREEN, COLOR_BLACK);
    refreshLayout();
}

void UI::refreshLayout() {
    getmaxyx(stdscr, rows, cols);
    computeLayout();
}

void UI::computeLayout() {
    boardRects.clear();
    int n = settings->numBoards();
    int colsGrid = (n<=1)?1: (n<=3? n: (n<=4?2:3));
    int rowsGrid = (n + colsGrid -1)/colsGrid;
    int bW = cols / colsGrid;
    int bH = (rows-4) / rowsGrid; // reserve space for header
    for (int i=0;i<n;i++) {
        int r = i / colsGrid;
        int c = i % colsGrid;
        Rect rc;
        rc.top = 2 + r*bH;
        rc.left = c*bW;
        rc.height = bH;
        rc.width = bW;
        boardRects.push_back(rc);
    }
}

void UI::draw() {
    clear();
    if (screen==MENU) drawMenuScreen();
    else if (screen==HELP) drawHelpScreen();
    else if (screen==SETTINGS) drawSettingsScreen();
    else drawBoards();
    refresh();
}

void UI::drawMenuScreen() {
    attron(COLOR_PAIR(1));
    mvprintw(1,2,"TRES EN RAYA - ncurses (GitHub Copilot template)");
    attroff(COLOR_PAIR(1));
    const char *opts[] = {"Jugar","Ajustes","Ayuda","Salir"};
    static int sel = 0;
    int start = 5;
    for (int i=0;i<4;i++) {
        if (i==sel) attron(A_REVERSE);
        mvprintw(start + i*2, (cols/2)-10, opts[i]);
        if (i==sel) attroff(A_REVERSE);
    }
    mvprintw(rows-2,2,"Usa flechas o ratón; Enter para seleccionar. (Q para salir en cualquier pantalla)");
    int ch = getch();
    if (ch==KEY_UP) sel = (sel-1+4)%4;
    else if (ch==KEY_DOWN) sel = (sel+1)%4;
    else if (ch==10 || ch==KEY_ENTER) {
        if (sel==0) { // Jugar
            screen = PLAY;
            game->gameSettings();
            game->ensureBoards();
            return;
        } else if (sel==1) screen = SETTINGS;
        else if (sel==2) screen = HELP;
        else if (sel==3) { endwin(); exit(0); }
    } else if (ch==KEY_MOUSE) handleMouse();
}

void UI::drawHelpScreen() {
    mvprintw(1,2,"Ayuda - Controles y reglas");
    mvprintw(3,2,"Controles:");
    mvprintw(4,4,"Flechas: mover selección dentro del tablero");
    mvprintw(5,4,"Tab: cambiar entre tableros");
    mvprintw(6,4,"Enter: colocar ficha");
    mvprintw(7,4,"R: reiniciar tablero seleccionado");
    mvprintw(8,4,"Q: salir al menú o cerrar la aplicación");
    mvprintw(10,2,"Modos de juego:");
    mvprintw(11,4,"0 jugadores: todos los tableros se completan automáticamente con jugadas aleatorias");
    mvprintw(12,4,"1 jugador: el mismo jugador controla X y O manualmente (sin IA)");
    mvprintw(13,4,"2 jugadores: el jugador controla O, la X aparece automáticamente tras cada turno");
    mvprintw(rows-2,2,"Pulsa cualquier tecla para volver al menú");
    getch();
    screen = MENU;
}

void UI::drawSettingsScreen() {
    int sel = 0;
    while (true) {
        clear();
        mvprintw(1,2,"Ajustes");
        mvprintw(3,4,"Número de jugadores (0-2): %d", settings->numPlayers());
        mvprintw(4,4,"Número de tableros (1-9): %d", settings->numBoards());
        mvprintw(6,2,"Usa izquierda/derecha para cambiar valores. Esc para volver.");
        int ch = getch();
        if (ch==KEY_LEFT) {
            if (sel==0) settings->setNumPlayers( settings->numPlayers()-1 );
            else settings->setNumBoards( settings->numBoards()-1 );
        } else if (ch==KEY_RIGHT) {
            if (sel==0) settings->setNumPlayers( settings->numPlayers()+1 );
            else settings->setNumBoards( settings->numBoards()+1 );
        } else if (ch==KEY_DOWN) sel = (sel+1)%2;
        else if (ch==KEY_UP) sel = (sel-1+2)%2;
        else if (ch==27) { screen = MENU; break; }
        else if (ch==10 || ch==KEY_ENTER) { /* accept and back */ screen = MENU; break; }
    }
}

void UI::drawBoards() {
    computeLayout();
    mvprintw(0,2,"Modo: %d jugadores | Tableros: %d | Seleccionado: %d  (Tab para cambiar)", settings->numPlayers(), settings->numBoards(), game->selectedBoard()+1);
    for (int i=0;i<settings->numBoards();i++) {
        bool highlight = (i==game->selectedBoard());
        drawSingleBoard(i, boardRects[i], highlight);
    }
    mvprintw(rows-2,2,"Enter para jugar, R reinicia tablero, Tab cambia tablero, Q vuelve/al menú");
}

void UI::drawSingleBoard(int idx, const Rect &rc, bool highlight) {
    int top = rc.top, left = rc.left, h = rc.height, w = rc.width;
    Board &b = game->getBoard(idx);
    // draw border
    for (int x=left; x<left+w && x<cols; x++) mvaddch(top, x, '-');
    for (int y=top; y<top+h && y<rows; y++) mvaddch(y, left, '|');
    mvprintw(top, left+2, "Tablero %d", idx+1);
    // compute cell positions
    int cellH = std::max(1, (h-3)/3);
    int cellW = std::max(3, (w-2)/3);
    int baseY = top+1;
    for (int r=0;r<3;r++) for (int c=0;c<3;c++) {
        int y = baseY + r*cellH;
        int x = left + 1 + c*cellW;
        char ch = b.getCell(r,c);
        if (highlight && r==cursorR && c==cursorC && idx==game->selectedBoard()) attron(A_STANDOUT);
        mvprintw(y, x, " %c ", ch== ' ' ? '.' : ch);
        if (highlight && r==cursorR && c==cursorC && idx==game->selectedBoard()) attroff(A_STANDOUT);
    }
    // stats
    std::stringstream ss;
    ss << "X:" << b.xWins << " O:" << b.oWins << " Empates:" << b.draws;
    mvprintw(top + h -1, left+1, ss.str().c_str());
}

void UI::processInput(int ch) {
    if (ch==9) { // Tab
        game->nextBoard();
        cursorR = cursorC = 0;
        return;
    }
    if (ch==KEY_MOUSE) { handleMouse(); return; }
    if (ch==KEY_LEFT) { cursorC = (cursorC-1+3)%3; }
    else if (ch==KEY_RIGHT) { cursorC = (cursorC+1)%3; }
    else if (ch==KEY_UP) { cursorR = (cursorR-1+3)%3; }
    else if (ch==KEY_DOWN) { cursorR = (cursorR+1)%3; }
    else if (ch=='r' || ch=='R') { game->resetBoard(game->selectedBoard()); }
    else if (ch==10 || ch==KEY_ENTER) {
        int bidx = game->selectedBoard();
        // settings: mode 1: human controls both X and O (manual); mode 2: player controls O, X auto
        if (settings->numPlayers()==1) {
            // manual both
            game->makeHumanMove(bidx, cursorR, cursorC);
        } else if (settings->numPlayers()==2) {
            // human controls O only
            // we expect board turn to reflect that; if current turn is O allow move, else ignore
            Board &b = game->getBoard(bidx);
            if (b.currentTurn()=='O') game->makeHumanMove(bidx, cursorR, cursorC);
        }
    }
}

void UI::handleMouse() {
    MEVENT event;
    if (getmouse(&event) == OK) {
        int mx = event.x, my = event.y;
        int b,r,c;
        mapClickToCell(mx,my,b,r,c);
        if (b>=0) {
            game->selectBoard(b);
            cursorR = r; cursorC = c;
            // emulate enter
            if (event.bstate & BUTTON1_CLICKED) {
                int bidx = b;
                if (settings->numPlayers()==1) game->makeHumanMove(bidx,r,c);
                else if (settings->numPlayers()==2) {
                    Board &bd = game->getBoard(bidx);
                    if (bd.currentTurn()=='O') game->makeHumanMove(bidx,r,c);
                }
            }
        }
    }
}

void UI::mapClickToCell(int mx,int my,int &b,int &r,int &c) {
    b = r = c = -1;
    for (int i=0;i<(int)boardRects.size();i++) {
        auto rc = boardRects[i];
        if (mx >= rc.left && mx < rc.left + rc.width && my >= rc.top && my < rc.top + rc.height) {
            int relx = mx - rc.left - 1;
            int rely = my - (rc.top+1);
            int cellW = std::max(3, (rc.width-2)/3);
            int cellH = std::max(1, (rc.height-3)/3);
            int cc = std::min(2, std::max(0, relx / cellW));
            int rr = std::min(2, std::max(0, rely / cellH));
            b = i; r = rr; c = cc; return;
        }
    }
}
