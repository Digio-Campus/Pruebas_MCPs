#include "UI.h"

#include <algorithm>
#include <sstream>

#include <ncurses.h>

static constexpr int kGapX = 2;
static constexpr int kGapY = 1;
static constexpr int kBoardW = 20;
static constexpr int kBoardH = 10;

static int colorOr(int pair, int attr) {
  if (has_colors()) return attr | COLOR_PAIR(pair);
  return attr;
}

UI::UI() {}

void UI::drawCentered(int y, const std::string& text, int attr) {
  int h, w;
  getmaxyx(stdscr, h, w);
  (void)h;
  int x = std::max(0, (w - static_cast<int>(text.size())) / 2);
  attron(attr);
  mvaddnstr(y, x, text.c_str(), w - x);
  attroff(attr);
}

void UI::drawMainMenu(int selected) const {
  erase();
  int h, w;
  getmaxyx(stdscr, h, w);

  drawCentered(1, "Tres en Raya (ncurses) - Multi-tablero", colorOr(1, A_BOLD));

  const char* items[] = {"Jugar", "Ajustes", "Ayuda", "Salir"};
  int n = 4;
  int startY = h / 2 - n;

  for (int i = 0; i < n; ++i) {
    int attr = (i == selected) ? colorOr(2, A_REVERSE) : A_NORMAL;
    drawCentered(startY + i * 2, items[i], attr);
  }

  drawCentered(h - 2, "Flechas ↑/↓ y Enter. (q/Esc para salir)", A_DIM);
  refresh();
}

void UI::drawHelp() const {
  erase();
  int h, w;
  getmaxyx(stdscr, h, w);

  drawCentered(1, "Ayuda", colorOr(1, A_BOLD));

  int y = 3;
  auto line = [&](const std::string& s) {
    if (y < h - 2) mvaddnstr(y++, 2, s.c_str(), w - 4);
  };

  line("Controles (menú): Flechas arriba/abajo para seleccionar, Enter para aceptar, q/Esc para volver.");
  line("");
  line("Controles (partida):");
  line("  - Flechas: mover cursor dentro del tablero actual");
  line("  - Enter o Espacio: colocar ficha (si el modo lo permite)");
  line("  - Tab / Shift+Tab: cambiar de tablero");
  line("  - r: reiniciar tablero actual (mantiene estadísticas)");
  line("  - R: reiniciar todos los tableros (mantiene estadísticas)");
  line("  - p: pausar/reanudar auto-juego (solo modo 0 jugadores)");
  line("  - q: volver al menú principal");
  line("");
  line("Reglas: Cada tablero es independiente. En cada tablero el turno alterna estrictamente X → O.");
  line("Victorias: 3 en línea en filas, columnas o diagonales. Empate: tablero lleno sin victoria.");
  line("");
  line("Modos (Ajustes):");
  line("  - 0 jugadores: X y O se juegan automáticamente (aleatorio) en todos los tableros.");
  line("  - 1 jugador: el usuario juega manualmente tanto X como O en cada tablero.");
  line("  - 2 jugadores: el usuario juega O; X se coloca automáticamente tras cada O.");

  drawCentered(h - 2, "Pulsa cualquier tecla para volver...", A_DIM);
  refresh();
}

void UI::drawSettings(const Settings& s, int selectedItem, int maxBoardsFit) const {
  erase();
  int h, w;
  getmaxyx(stdscr, h, w);

  drawCentered(1, "Ajustes", colorOr(1, A_BOLD));

  auto pmToStr = [](PlayersMode pm) {
    switch (pm) {
      case PlayersMode::Zero: return "0 (auto aleatorio)";
      case PlayersMode::One:  return "1 (manual X y O)";
      case PlayersMode::Two:  return "2 (O manual, X auto)";
    }
    return "?";
  };

  int y = 4;
  auto drawItem = [&](int idx, const std::string& label, const std::string& value) {
    int attr = (idx == selectedItem) ? colorOr(2, A_REVERSE) : A_NORMAL;
    attron(attr);
    mvaddnstr(y, 4, label.c_str(), w - 8);
    mvaddnstr(y, 28, value.c_str(), w - 32);
    attroff(attr);
    y += 2;
  };

  drawItem(0, "Jugadores", pmToStr(s.playersMode));

  {
    std::ostringstream oss;
    oss << s.boardsCount;
    if (maxBoardsFit > 0) oss << "  (max visible ~" << maxBoardsFit << ")";
    drawItem(1, "Tableros", oss.str());
  }

  int attrBack = (2 == selectedItem) ? colorOr(2, A_REVERSE) : A_NORMAL;
  drawCentered(y + 1, "Volver", attrBack);

  drawCentered(h - 2, "↑/↓ seleccionar, ←/→ cambiar, Enter para volver.", A_DIM);
  refresh();
}

std::vector<BoardRect> UI::layoutBoards(int count) const {
  int h, w;
  getmaxyx(stdscr, h, w);

  int top = 2;
  int bottomBar = 3;
  int usableH = h - top - bottomBar;
  int usableW = w - 2;

  int cols = std::max(1, (usableW + kGapX) / (kBoardW + kGapX));
  int rows = (count + cols - 1) / cols;

  // Ajusta cols si no caben en altura.
  while (rows > 0 && rows * (kBoardH + kGapY) - kGapY > usableH && cols > 1) {
    --cols;
    rows = (count + cols - 1) / cols;
  }

  std::vector<BoardRect> out;
  out.reserve(count);

  for (int i = 0; i < count; ++i) {
    int r = i / cols;
    int c = i % cols;
    BoardRect br;
    br.y = top + r * (kBoardH + kGapY);
    br.x = 1 + c * (kBoardW + kGapX);
    br.h = kBoardH;
    br.w = kBoardW;
    out.push_back(br);
  }
  return out;
}

int UI::maxBoardsFit() const {
  int h, w;
  getmaxyx(stdscr, h, w);

  int top = 2;
  int bottomBar = 3;
  int usableH = h - top - bottomBar;
  int usableW = w - 2;

  int cols = std::max(1, (usableW + kGapX) / (kBoardW + kGapX));
  int rows = std::max(1, (usableH + kGapY) / (kBoardH + kGapY));

  return std::max(1, cols * rows);
}

static char cellChar(Board::Cell c) {
  if (c == Board::Cell::X) return 'X';
  if (c == Board::Cell::O) return 'O';
  return ' ';
}

void UI::drawGame(const Settings& s,
                  const std::vector<Board>& boards,
                  const std::vector<std::pair<int, int>>& cursors,
                  int selectedBoard,
                  bool autoPaused,
                  int maxBoardsFit) const {
  erase();
  int h, w;
  getmaxyx(stdscr, h, w);

  std::string mode;
  switch (s.playersMode) {
    case PlayersMode::Zero: mode = "0 jugadores (auto)"; break;
    case PlayersMode::One:  mode = "1 jugador (manual X/O)"; break;
    case PlayersMode::Two:  mode = "2 jugadores (O manual, X auto)"; break;
  }

  std::ostringstream title;
  title << "Jugar - " << mode;
  if (s.playersMode == PlayersMode::Zero) title << (autoPaused ? " [PAUSA]" : "");
  if (maxBoardsFit > 0 && static_cast<int>(boards.size()) > maxBoardsFit) title << " (no caben todos)";

  drawCentered(0, title.str(), colorOr(1, A_BOLD));

  auto rects = layoutBoards(static_cast<int>(boards.size()));

  mvaddnstr(h - 2, 1,
            "Arrows: mover  Enter/Esp: jugar  Tab: tablero  r/R: reset  q: salir  (p: pausa auto)",
            w - 2);

  // Evita parpadeo: no hagas refresh por tablero (mostraba frames vacíos entre updates).
  wnoutrefresh(stdscr);

  for (int i = 0; i < static_cast<int>(boards.size()); ++i) {
    const auto& br = rects[i];

    WINDOW* win = newwin(br.h, br.w, br.y, br.x);
    if (!win) continue;

    int borderAttr = A_NORMAL;
    if (i == selectedBoard) borderAttr = colorOr(5, A_BOLD);
    wattron(win, borderAttr);
    box(win, 0, 0);
    wattroff(win, borderAttr);

    // Header
    std::ostringstream head;
    head << "Tab " << (i + 1) << " ";

    std::string status;
    switch (boards[i].result()) {
      case Board::Result::InProgress:
        status = (boards[i].currentTurn() == Board::Cell::X) ? "Turno: X" : "Turno: O";
        break;
      case Board::Result::WinX: status = "Gana X"; break;
      case Board::Result::WinO: status = "Gana O"; break;
      case Board::Result::Draw: status = "Empate"; break;
    }

    mvwaddnstr(win, 1, 2, head.str().c_str(), br.w - 4);
    mvwaddnstr(win, 1, br.w - 2 - static_cast<int>(status.size()), status.c_str(), br.w - 4);

    // Grid (3x3)
    int gy = 3;
    int gx[3] = {3, 9, 15};

    for (int r = 0; r < 3; ++r) {
      // Separadores horizontales
      if (r > 0) {
        int sy = gy - 1;
        for (int x = 2; x < br.w - 2; ++x) mvwaddch(win, sy, x, ACS_HLINE);
        mvwaddch(win, sy, 7, ACS_PLUS);
        mvwaddch(win, sy, 13, ACS_PLUS);
      }

      // Separadores verticales
      for (int y = gy - 1; y <= gy; ++y) {
        if (y >= 2 && y < br.h - 2) {
          mvwaddch(win, y, 7, ACS_VLINE);
          mvwaddch(win, y, 13, ACS_VLINE);
        }
      }

      for (int c = 0; c < 3; ++c) {
        int attr = A_NORMAL;
        if (i == selectedBoard && cursors[i].first == r && cursors[i].second == c) {
          attr |= A_REVERSE;
        }

        Board::Cell cell = boards[i].at(r, c);
        int pieceAttr = A_BOLD;
        if (cell == Board::Cell::X) pieceAttr = colorOr(3, A_BOLD);
        else if (cell == Board::Cell::O) pieceAttr = colorOr(4, A_BOLD);

        wattron(win, attr | pieceAttr);
        mvwaddch(win, gy, gx[c], cellChar(cell));
        wattroff(win, attr | pieceAttr);
      }

      gy += 2;
    }

    // Footer stats
    std::ostringstream stats;
    stats << "X:" << boards[i].xWins() << " O:" << boards[i].oWins() << " =:" << boards[i].draws();
    mvwaddnstr(win, br.h - 2, 2, stats.str().c_str(), br.w - 4);

    wnoutrefresh(win);
    delwin(win);
  }

  doupdate();
}
