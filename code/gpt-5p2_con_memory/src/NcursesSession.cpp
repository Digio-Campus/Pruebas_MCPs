#include "NcursesSession.h"

#include <clocale>

NcursesSession::NcursesSession() {
  // Soporte básico de locale (bordes ACS, etc.).
  std::setlocale(LC_ALL, "");

  if (!initscr()) {
    throw std::runtime_error("Error inicializando ncurses (initscr devolvió nullptr)");
  }

  cbreak();
  noecho();
  keypad(stdscr, TRUE);
  curs_set(0);

  // Permite detectar resize como KEY_RESIZE.
  // (ncurses lo entrega automáticamente en getch()).

  if (has_colors()) {
    start_color();
    use_default_colors();

    // Pares básicos.
    init_pair(1, COLOR_CYAN, -1);   // títulos
    init_pair(2, COLOR_YELLOW, -1); // selección
    init_pair(3, COLOR_GREEN, -1);  // X
    init_pair(4, COLOR_RED, -1);    // O
    init_pair(5, COLOR_MAGENTA, -1);// borde tablero seleccionado
  }
}

NcursesSession::~NcursesSession() {
  endwin();
}
