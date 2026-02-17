#include "../include/Menu.h"
#include <ncurses.h>
#include <vector>
#include <string>

Menu::Menu() {}

static void drawCentered(int y, const std::string &s) {
    int w = COLS;
    mvprintw(y, (w - (int)s.size())/2, "%s", s.c_str());
}

int Menu::run() {
    // simple keyboard menu
    const std::vector<std::string> options = {"Jugar", "Ajustes", "Ayuda", "Salir"};
    int idx = 0;
    keypad(stdscr, TRUE);
    while (true) {
        clear();
        drawCentered(3, "TicTacToe - ncurses");
        for (size_t i = 0; i < options.size(); ++i) {
            if ((int)i == idx) attron(A_REVERSE);
            drawCentered(6 + (int)i*2, options[i]);
            if ((int)i == idx) attroff(A_REVERSE);
        }
        refresh();
        int ch = getch();
        if (ch == KEY_UP) idx = (idx - 1 + (int)options.size()) % (int)options.size();
        else if (ch == KEY_DOWN) idx = (idx + 1) % (int)options.size();
        else if (ch == 10 || ch == KEY_ENTER) return idx;
        else if (ch == 'q' || ch == 'Q') return 3;
    }
}

int Menu::runSettings(int &numPlayers, int &numBoards) {
    const std::vector<std::string> players = {"0 (auto)", "1 (manual X+O)", "2 (player=O, X auto)"};
    const std::vector<int> boards = {1,2,4,6,9};
    int pIdx = std::max(0, std::min(2, numPlayers));
    int bIdx = 0; for (size_t i=0;i<boards.size();++i) if (boards[i]==numBoards) bIdx = (int)i;

    int field = 0; // 0 players, 1 boards, 2 accept
    while (true) {
        clear();
        mvprintw(2,2,"Ajustes");
        mvprintw(5,4,"Jugadores: %s" , players[pIdx].c_str());
        mvprintw(7,4,"Tableros: %d" , boards[bIdx]);
        mvprintw(10,4,"Enter para guardar, Esc para cancelar");
        if (field == 0) mvprintw(5,2,">"); else mvprintw(5,2," ");
        if (field == 1) mvprintw(7,2,">"); else mvprintw(7,2," ");
        int ch = getch();
        if (ch == KEY_UP) field = std::max(0, field-1);
        else if (ch == KEY_DOWN) field = std::min(1, field+1);
        else if (ch == KEY_LEFT) {
            if (field == 0) pIdx = std::max(0, pIdx-1);
            else bIdx = std::max(0, bIdx-1);
        }
        else if (ch == KEY_RIGHT) {
            if (field == 0) pIdx = std::min((int)players.size()-1, pIdx+1);
            else bIdx = std::min((int)boards.size()-1, bIdx+1);
        }
        else if (ch == 10 || ch == KEY_ENTER) {
            numPlayers = pIdx; numBoards = boards[bIdx];
            return 0;
        }
        else if (ch == 27) return -1;
    }
}

void Menu::showHelp() {
    clear();
    mvprintw(1,2,"Ayuda - Controles y reglas");
    mvprintw(3,2,"Controles:");
    mvprintw(4,4,"Flechas: mover cursor en tablero");
    mvprintw(5,4,"Tab: cambiar tablero seleccionado");
    mvprintw(6,4,"Enter: colocar ficha (respeta turno)");
    mvprintw(7,4,"R: reiniciar tablero actual");
    mvprintw(8,4,"Q: salir al menú principal desde la partida");
    mvprintw(10,2,"Modos de juego:");
    mvprintw(11,4,"0 = ambos jugadores automáticos (llenado aleatorio)");
    mvprintw(12,4,"1 = un jugador controla X y O manualmente (se alterna)");
    mvprintw(13,4,"2 = jugador controla O; X es generado automáticamente tras cada turno");
    mvprintw(15,2,"Pulse cualquier tecla para volver...");
    refresh();
    getch();
}
