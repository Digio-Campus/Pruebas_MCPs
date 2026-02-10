#include "Ui.h"

#include <ncurses.h>

#include <algorithm>
#include <cctype>
#include <cmath>
#include <string>

namespace {

struct NcursesSession {
  NcursesSession() {
    initscr();
    cbreak();
    noecho();
    keypad(stdscr, TRUE);
    curs_set(0);
    nodelay(stdscr, FALSE);

    if (has_colors()) {
      start_color();
      use_default_colors();
      init_pair(1, COLOR_CYAN, -1);
      init_pair(2, COLOR_YELLOW, -1);
      init_pair(3, COLOR_GREEN, -1);
    }
  }
  ~NcursesSession() { endwin(); }
};

const char *playersLabel(int p) {
  switch (p) {
    case 0:
      return "0 jugadores (auto)";
    case 1:
      return "1 jugador (manual X/O)";
    case 2:
      return "2 jugadores (O humano, X auto)";
    default:
      return "?";
  }
}

std::string resultLabel(Board::Result r) {
  switch (r) {
    case Board::Result::Ongoing:
      return "En curso";
    case Board::Result::XWin:
      return "Gana X";
    case Board::Result::OWin:
      return "Gana O";
    case Board::Result::Draw:
      return "Empate";
  }
  return "";
}

}  // namespace

Ui::Ui(Settings &settings) : settings_(settings) {}

int Ui::run() {
  NcursesSession session;

  while (true) {
    int choice = mainMenu();
    if (choice == 0) {
      gameLoop();
    } else if (choice == 1) {
      settingsMenu();
    } else if (choice == 2) {
      helpScreen();
    } else {
      break;
    }
  }
  return 0;
}

int Ui::mainMenu() {
  const char *items[] = {"Jugar", "Ajustes", "Ayuda", "Salir"};
  int selected = 0;

  while (true) {
    clear();
    drawFrameTitle("Tres en Raya (ncurses)");

    int h, w;
    getmaxyx(stdscr, h, w);

    drawCenteredText(2, "Selecciona una opción", true);

    int startY = 5;
    for (int i = 0; i < 4; ++i) {
      bool active = (i == selected);
      if (active) attron(A_REVERSE);
      drawCenteredText(startY + i * 2, items[i]);
      if (active) attroff(A_REVERSE);
    }

    mvprintw(h - 2, 2, "↑/↓ mover  Enter aceptar");
    refresh();

    int ch = getch();
    if (ch == KEY_RESIZE) {
      handleResize();
      continue;
    }

    if (ch == KEY_UP) {
      selected = (selected + 3) % 4;
    } else if (ch == KEY_DOWN) {
      selected = (selected + 1) % 4;
    } else if (ch == '\n' || ch == KEY_ENTER) {
      return selected;
    }
  }
}

void Ui::settingsMenu() {
  int selected = 0;  // 0 players, 1 boards, 2 back

  while (true) {
    settings_.clamp();

    clear();
    drawFrameTitle("Ajustes");

    int h, w;
    getmaxyx(stdscr, h, w);

    int y = 3;

    // Opción: jugadores
    {
      bool active = (selected == 0);
      if (active) attron(A_REVERSE);
      mvprintw(y, 2, "Número de jugadores: %s", playersLabel(settings_.numPlayers));
      if (active) attroff(A_REVERSE);
      y += 2;
    }

    // Opción: tableros
    {
      bool active = (selected == 1);
      if (active) attron(A_REVERSE);
      mvprintw(y, 2, "Tableros simultáneos: %zu", settings_.numBoards);
      if (active) attroff(A_REVERSE);
      y += 2;
    }

    // Volver
    {
      bool active = (selected == 2);
      if (active) attron(A_REVERSE);
      mvprintw(y, 2, "Volver");
      if (active) attroff(A_REVERSE);
    }

    mvprintw(h - 3, 2, "↑/↓ elegir  ←/→ cambiar  Enter aceptar");
    mvprintw(h - 2, 2, "Límites: tableros %zu..%zu", Settings::kMinBoards, Settings::kMaxBoards);
    refresh();

    int ch = getch();
    if (ch == KEY_RESIZE) {
      handleResize();
      continue;
    }

    if (ch == KEY_UP) {
      selected = (selected + 2) % 3;
    } else if (ch == KEY_DOWN) {
      selected = (selected + 1) % 3;
    } else if (ch == KEY_LEFT) {
      if (selected == 0) settings_.numPlayers = std::max(0, settings_.numPlayers - 1);
      if (selected == 1 && settings_.numBoards > Settings::kMinBoards) --settings_.numBoards;
    } else if (ch == KEY_RIGHT) {
      if (selected == 0) settings_.numPlayers = std::min(2, settings_.numPlayers + 1);
      if (selected == 1 && settings_.numBoards < Settings::kMaxBoards) ++settings_.numBoards;
    } else if (ch == '\n' || ch == KEY_ENTER) {
      if (selected == 2) return;
    }
  }
}

void Ui::helpScreen() {
  while (true) {
    clear();
    drawFrameTitle("Ayuda");

    int h, w;
    getmaxyx(stdscr, h, w);

    int y = 2;
    mvprintw(y++, 2, "Objetivo: formar una línea de 3 (horizontal, vertical o diagonal).");
    y++;

    mvprintw(y++, 2, "Controles en menús:");
    mvprintw(y++, 4, "- Flechas ↑/↓ para navegar");
    mvprintw(y++, 4, "- Enter para aceptar");
    y++;

    mvprintw(y++, 2, "Controles en partida:");
    mvprintw(y++, 4, "- Flechas: mover la casilla seleccionada dentro del tablero");
    mvprintw(y++, 4, "- Tab / Shift+Tab: cambiar entre tableros");
    mvprintw(y++, 4, "- Enter o Espacio: colocar ficha (si el modo lo permite)");
    mvprintw(y++, 4, "- r: reiniciar tablero seleccionado cuando haya terminado");
    mvprintw(y++, 4, "- q: volver al menú principal");
    y++;

    mvprintw(y++, 2, "Modos (Ajustes):");
    mvprintw(y++, 4, "- 0 jugadores: jugadas aleatorias en todos los tableros");
    mvprintw(y++, 4, "- 1 jugador: control manual de X y O (turno alterno por tablero)");
    mvprintw(y++, 4, "- 2 jugadores: O manual, X automático cuando le toca a X");
    y++;

    mvprintw(h - 2, 2, "Pulsa 'q' para volver");

    refresh();

    int ch = getch();
    if (ch == KEY_RESIZE) {
      handleResize();
      continue;
    }
    if (ch == 'q' || ch == 'Q' || ch == 27) return;
  }
}

Ui::Layout Ui::computeLayout(int termH, int termW, std::size_t boards) const {
  Layout L;

  // Tamaño por tablero (en caracteres) para la rejilla + stats.
  L.boardW = 18;
  L.boardH = 8;
  L.marginTop = 2;
  L.marginLeft = 1;
  L.gapX = 2;
  L.gapY = 1;

  int usableW = termW - L.marginLeft * 2;

  int perW = L.boardW + L.gapX;
  L.cols = std::max(1, usableW / perW);
  L.cols = std::min<int>(L.cols, static_cast<int>(boards));
  L.rows = static_cast<int>(std::ceil(boards / static_cast<double>(L.cols)));

  int needW = L.marginLeft * 2 + L.cols * L.boardW + (L.cols - 1) * L.gapX;
  int needH = (L.marginTop + 2) + L.rows * L.boardH + (L.rows - 1) * L.gapY;

  L.fits = (needW <= termW && needH <= termH);
  return L;
}

void Ui::drawBoards(const std::vector<Board> &boards, std::size_t currentBoard,
                    std::size_t selR, std::size_t selC) {
  int h, w;
  getmaxyx(stdscr, h, w);

  Layout L = computeLayout(h, w, boards.size());

  if (!L.fits) {
    attron(A_BOLD);
    drawCenteredText(3, "Terminal demasiado pequeña para mostrar todos los tableros.");
    attroff(A_BOLD);
    drawCenteredText(5, "Reduce el número de tableros en Ajustes o agranda la ventana.");
    return;
  }

  for (std::size_t i = 0; i < boards.size(); ++i) {
    int r = static_cast<int>(i) / L.cols;
    int c = static_cast<int>(i) % L.cols;

    int x = L.marginLeft + c * (L.boardW + L.gapX);
    int y = L.marginTop + r * (L.boardH + L.gapY);

    drawSingleBoard(y, x, static_cast<int>(i), boards[i], i == currentBoard, selR, selC);
  }
}

void Ui::drawSingleBoard(int y, int x, int idx, const Board &b, bool active,
                         std::size_t selR, std::size_t selC) {
  // Encabezado
  if (active) attron(COLOR_PAIR(1));
  mvprintw(y, x, "Tablero %d", idx + 1);
  if (active) attroff(COLOR_PAIR(1));

  mvprintw(y, x + 11, "%s", resultLabel(b.result()).c_str());

  // Rejilla
  auto cellChar = [&](std::size_t r, std::size_t c) -> char {
    return static_cast<char>(b.at(r, c));
  };

  int gy = y + 1;
  for (int r = 0; r < 3; ++r) {
    int lineY = gy + r * 2;

    for (int c = 0; c < 3; ++c) {
      int cx = x + c * 4;
      bool highlight = active && (selR == static_cast<std::size_t>(r)) && (selC == static_cast<std::size_t>(c));

      if (highlight) attron(A_REVERSE);
      mvprintw(lineY, cx, " %c ", cellChar(r, c));
      if (highlight) attroff(A_REVERSE);

      if (c < 2) mvaddch(lineY, cx + 3, '|');
    }

    if (r < 2) mvprintw(lineY + 1, x, "---+---+---");
  }

  // Turno y stats
  const char turnCh = static_cast<char>(b.turn());
  mvprintw(y + 6, x, "Turno: %c", (b.result() == Board::Result::Ongoing) ? turnCh : '-');
  mvprintw(y + 7, x, "X:%d O:%d D:%d", b.xWins(), b.oWins(), b.draws());

  // Marcador visual de tablero activo
  if (active) {
    attron(A_BOLD);
    mvaddch(y + 1, x + 13, '<');
    attroff(A_BOLD);
  }
}

void Ui::gameLoop() {
  settings_.clamp();
  std::vector<Board> boards(settings_.numBoards);

  std::size_t currentBoard = 0;
  std::size_t selR = 0;
  std::size_t selC = 0;

  // Para detectar Shift+Tab en algunos terminals: KEY_BTAB.
  while (true) {
    clear();
    drawFrameTitle("Partida");

    // Auto-movimientos según modo
    if (settings_.numPlayers == 0) {
      // Avanza una jugada aleatoria por tablero y reinicia al terminar.
      for (auto &b : boards) {
        if (b.result() == Board::Result::Ongoing) {
          b.randomMove();
        } else {
          b.reset();
        }
      }
    } else if (settings_.numPlayers == 2) {
      // X automático: ejecuta una jugada en cualquier tablero que esté en turno X.
      for (auto &b : boards) {
        if (b.result() == Board::Result::Ongoing && b.turn() == Board::Cell::X) {
          b.randomMove();
        }
      }
    }

    drawBoards(boards, currentBoard, selR, selC);

    int h, w;
    getmaxyx(stdscr, h, w);

    mvprintw(h - 2, 1, "Tab: tablero  Enter/Espacio: jugar  r: reset  q: menú  (modo: %s)",
             playersLabel(settings_.numPlayers));
    mvprintw(h - 1, 1, "Nota: alternancia X↔O estricta e independiente por tablero.");

    refresh();

    // En modo 0 jugadores, la partida "corre" continuamente.
    if (settings_.numPlayers == 0) {
      timeout(120);
    } else {
      timeout(-1);
    }

    int ch = getch();
    timeout(-1);

    if (ch == ERR) continue;

    if (ch == KEY_RESIZE) {
      handleResize();
      continue;
    }

    if (ch == 'q' || ch == 'Q' || ch == 27) return;

    if (ch == '\t' || ch == KEY_RIGHT) {
      // Tab: siguiente tablero (si no estamos usando RIGHT para mover). Para evitar
      // ambigüedad, mantenemos Tab como primario; RIGHT se usa para casillas.
    }

    // Cambiar tablero
    if (ch == '\t') {
      currentBoard = (currentBoard + 1) % boards.size();
      selR = selC = 0;
      continue;
    }
    if (ch == KEY_BTAB) {
      currentBoard = (currentBoard + boards.size() - 1) % boards.size();
      selR = selC = 0;
      continue;
    }

    Board &b = boards[currentBoard];

    // Reset individual
    if ((ch == 'r' || ch == 'R') && b.result() != Board::Result::Ongoing) {
      b.reset();
      continue;
    }

    // Movimiento dentro del tablero
    if (ch == KEY_UP && selR > 0) selR--;
    if (ch == KEY_DOWN && selR < 2) selR++;
    if (ch == KEY_LEFT && selC > 0) selC--;
    if (ch == KEY_RIGHT && selC < 2) selC++;

    // Colocar ficha
    if (ch == '\n' || ch == KEY_ENTER || ch == ' ') {
      if (b.result() != Board::Result::Ongoing) continue;

      if (settings_.numPlayers == 2) {
        // Humano solo juega O.
        if (b.turn() != Board::Cell::O) continue;
      }

      (void)b.place(selR, selC);
    }
  }
}

void Ui::drawCenteredText(int y, const char *text, bool bold) {
  int h, w;
  getmaxyx(stdscr, h, w);
  (void)h;

  int len = static_cast<int>(std::string(text).size());
  int x = std::max(0, (w - len) / 2);
  if (bold) attron(A_BOLD);
  mvprintw(y, x, "%s", text);
  if (bold) attroff(A_BOLD);
}

void Ui::drawFrameTitle(const char *title) {
  int h, w;
  getmaxyx(stdscr, h, w);

  attron(A_BOLD);
  mvhline(0, 0, ' ', w);
  mvprintw(0, 2, "%s", title);
  attroff(A_BOLD);

  mvhline(1, 0, ACS_HLINE, w);
  mvhline(h - 3, 0, ACS_HLINE, w);
}

void Ui::handleResize() {
  // Re-sincroniza ncurses tras redimensionado.
  endwin();
  refresh();
  clear();
}
