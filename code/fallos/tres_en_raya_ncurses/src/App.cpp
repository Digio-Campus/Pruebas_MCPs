#include "App.h"

#include "Board.h"
#include "UI.h"

#include <algorithm>
#include <chrono>
#include <cstdint>
#include <locale>
#include <ncurses.h>
#include <random>
#include <string>
#include <vector>

namespace {
constexpr int kBoardAreaW = 15; // incluye margen
constexpr int kBoardAreaH = 11;
constexpr int kGridW = 13;
constexpr int kGridH = 7;

int clampInt(int v, int lo, int hi) { return std::max(lo, std::min(hi, v)); }

void drawCentered(int y, const std::string &s, int attr = 0) {
  int h, w;
  getmaxyx(stdscr, h, w);
  (void)h;
  const int x = (w - static_cast<int>(s.size())) / 2;
  if (attr) attron(attr);
  mvaddnstr(y, std::max(0, x), s.c_str(), w - 1);
  if (attr) attroff(attr);
}

void drawGrid(int topY, int leftX, bool active) {
  const int attr = active ? A_BOLD : 0;
  if (attr) attron(attr);

  const int x0 = leftX;
  const int x1 = leftX + 4;
  const int x2 = leftX + 8;
  const int x3 = leftX + 12;

  const int y0 = topY;
  const int y1 = topY + 2;
  const int y2 = topY + 4;
  const int y3 = topY + 6;

  // horizontales
  for (int x = x0 + 1; x < x3; ++x) mvaddch(y0, x, ACS_HLINE);
  for (int x = x0 + 1; x < x3; ++x) mvaddch(y1, x, ACS_HLINE);
  for (int x = x0 + 1; x < x3; ++x) mvaddch(y2, x, ACS_HLINE);
  for (int x = x0 + 1; x < x3; ++x) mvaddch(y3, x, ACS_HLINE);

  // verticales
  for (int y = y0 + 1; y < y3; ++y) mvaddch(y, x0, ACS_VLINE);
  for (int y = y0 + 1; y < y3; ++y) mvaddch(y, x1, ACS_VLINE);
  for (int y = y0 + 1; y < y3; ++y) mvaddch(y, x2, ACS_VLINE);
  for (int y = y0 + 1; y < y3; ++y) mvaddch(y, x3, ACS_VLINE);

  // esquinas
  mvaddch(y0, x0, ACS_ULCORNER);
  mvaddch(y0, x3, ACS_URCORNER);
  mvaddch(y3, x0, ACS_LLCORNER);
  mvaddch(y3, x3, ACS_LRCORNER);

  // intersecciones
  mvaddch(y0, x1, ACS_TTEE);
  mvaddch(y0, x2, ACS_TTEE);
  mvaddch(y3, x1, ACS_BTEE);
  mvaddch(y3, x2, ACS_BTEE);

  mvaddch(y1, x0, ACS_LTEE);
  mvaddch(y2, x0, ACS_LTEE);
  mvaddch(y1, x3, ACS_RTEE);
  mvaddch(y2, x3, ACS_RTEE);

  mvaddch(y1, x1, ACS_PLUS);
  mvaddch(y1, x2, ACS_PLUS);
  mvaddch(y2, x1, ACS_PLUS);
  mvaddch(y2, x2, ACS_PLUS);

  if (attr) attroff(attr);
}

int cellToIndex(int row, int col) { return row * 3 + col; }

void indexToCell(int idx, int &row, int &col) {
  row = idx / 3;
  col = idx % 3;
}

struct Layout {
  bool fits = false;
  std::vector<Rect> rects;
};

Layout computeLayout(int termH, int termW, int nBoards) {
  Layout l;
  if (nBoards <= 0) return l;
  if (termW < kBoardAreaW || termH < (kBoardAreaH + 2)) {
    l.fits = false;
    return l;
  }

  int cols = std::max(1, termW / kBoardAreaW);
  cols = std::min(cols, nBoards);
  const int rows = (nBoards + cols - 1) / cols;

  // 2 líneas reservadas para estado/controles
  if (rows * kBoardAreaH + 2 > termH) {
    l.fits = false;
    return l;
  }

  l.fits = true;
  l.rects.resize(nBoards);
  for (int i = 0; i < nBoards; ++i) {
    const int r = i / cols;
    const int c = i % cols;
    l.rects[i] = Rect{r * kBoardAreaH, c * kBoardAreaW, kBoardAreaH, kBoardAreaW};
  }
  return l;
}

void drawBoard(const Board &b, const Rect &rc, int idx, bool active, int cursorIdx) {
  const int headerY = rc.y;
  const int gridY = rc.y + 1;
  const int gridX = rc.x + 1;

  std::string title = "Tablero " + std::to_string(idx + 1);
  if (active) title += " *";

  mvaddnstr(headerY, rc.x, title.c_str(), rc.w);
  drawGrid(gridY, gridX, active);

  // celdas
  for (int i = 0; i < 9; ++i) {
    int r, c;
    indexToCell(i, r, c);
    const int y = gridY + 1 + r * 2;
    const int x = gridX + 2 + c * 4;

    const bool hl = active && (i == cursorIdx) && (b.outcome() == Board::Outcome::InProgress);
    if (hl) attron(A_REVERSE);
    mvaddch(y, x, b.cells()[i]);
    if (hl) attroff(A_REVERSE);
  }

  // stats y estado
  const std::string stats = "X:" + std::to_string(b.xWins()) + "  O:" + std::to_string(b.oWins()) +
                            "  E:" + std::to_string(b.draws());
  mvaddnstr(rc.y + 8, rc.x, stats.c_str(), rc.w);

  const std::string st = b.outcomeText();
  mvaddnstr(rc.y + 9, rc.x, st.c_str(), rc.w);
}

std::uint32_t defaultSeed(std::uint32_t seedFromSettings) {
  if (seedFromSettings != 0) return seedFromSettings;
  const auto now = std::chrono::high_resolution_clock::now().time_since_epoch().count();
  return static_cast<std::uint32_t>(now);
}

} // namespace

int App::run() {
  while (!shouldExit_) {
    screenMainMenu();
  }
  return 0;
}

void App::screenMainMenu() {
  const int choice = UI::runMenu("Tres en Raya (ncurses)", {"Jugar", "Ajustes", "Ayuda", "Salir"});
  switch (choice) {
  case 0:
    screenGame();
    break;
  case 1:
    screenSettings();
    break;
  case 2:
    screenHelp();
    break;
  case 3:
  case -1:
  default:
    shouldExit_ = true;
    break;
  }
}

void App::screenSettings() {
  keypad(stdscr, TRUE);
  nodelay(stdscr, FALSE);

  settings_.numPlayers = clampInt(settings_.numPlayers, 0, 2);
  settings_.numBoards = clampInt(settings_.numBoards, Settings::kMinBoards, Settings::kMaxBoards);

  int field = 0; // 0 jugadores, 1 tableros
  while (true) {
    clear();
    int h, w;
    getmaxyx(stdscr, h, w);
    (void)h;

    drawCentered(1, "Ajustes", A_BOLD);
    mvhline(2, 0, ACS_HLINE, w);

    const std::string playersLine = "Numero de jugadores: " + std::to_string(settings_.numPlayers);
    const std::string boardsLine = "Numero de tableros:  " + std::to_string(settings_.numBoards);

    if (field == 0) attron(A_REVERSE);
    mvaddnstr(5, 4, playersLine.c_str(), w - 8);
    if (field == 0) attroff(A_REVERSE);

    if (field == 1) attron(A_REVERSE);
    mvaddnstr(7, 4, boardsLine.c_str(), w - 8);
    if (field == 1) attroff(A_REVERSE);

    mvaddnstr(10, 4, "Controles: Arriba/Abajo selecciona | Izq/Der cambia | Enter/Esc vuelve", w - 8);

    // Ayuda contextual
    std::vector<std::string> hints;
    hints.push_back("0 jugadores: X y O aleatorias (autoplay)");
    hints.push_back("1 jugador: juegas X y O manualmente (turno alterno por tablero)");
    hints.push_back("2 jugadores: juegas O, X automatica");
    int y = 12;
    for (const auto &s : hints) mvaddnstr(y++, 4, s.c_str(), w - 8);

    refresh();

    const int ch = getch();
    if (ch == KEY_UP) {
      field = (field - 1 + 2) % 2;
    } else if (ch == KEY_DOWN) {
      field = (field + 1) % 2;
    } else if (ch == KEY_LEFT) {
      if (field == 0) settings_.numPlayers = clampInt(settings_.numPlayers - 1, 0, 2);
      if (field == 1) settings_.numBoards = clampInt(settings_.numBoards - 1, Settings::kMinBoards, Settings::kMaxBoards);
    } else if (ch == KEY_RIGHT) {
      if (field == 0) settings_.numPlayers = clampInt(settings_.numPlayers + 1, 0, 2);
      if (field == 1) settings_.numBoards = clampInt(settings_.numBoards + 1, Settings::kMinBoards, Settings::kMaxBoards);
    } else if (ch == 10 || ch == KEY_ENTER || ch == 27) {
      break;
    } else if (ch == KEY_RESIZE) {
      continue;
    }
  }
}

void App::screenHelp() {
  UI::showMessageBox(
      "Ayuda",
      {
          "Objetivo: alinear 3 fichas (fila, columna o diagonal) en un tablero 3x3.",
          "",
          "Menus: Flechas arriba/abajo y Enter.",
          "",
          "Partida:",
          "  - Mover cursor: flechas",
          "  - Jugar: Enter o espacio",
          "  - Cambiar tablero: Tab / Shift+Tab o [ y ]",
          "  - Reiniciar tablero: r (actual) | R (todos)",
          "  - Volver al menu: q",
          "",
          "Modos:",
          "  0 jugadores: los tableros se completan solos (movimientos aleatorios).",
          "  1 jugador: controlas X y O manualmente; cada tablero alterna X->O independiente.",
          "  2 jugadores: controlas O; X se genera automaticamente en cada tablero.",
          "",
          "Notas: cada tablero mantiene su turno y sus estadisticas de forma independiente.",
      });
}

void App::screenGame() {
  keypad(stdscr, TRUE);
  timeout(50); // permite autoplay/resize sin bloquear la UI

  settings_.numPlayers = clampInt(settings_.numPlayers, 0, 2);
  settings_.numBoards = clampInt(settings_.numBoards, Settings::kMinBoards, Settings::kMaxBoards);

  std::mt19937 rng(defaultSeed(settings_.rngSeed));

  std::vector<Board> boards(settings_.numBoards);
  std::vector<int> autoResetCountdown(settings_.numBoards, 0);

  auto resetBoard = [&](int i) {
    boards[i].reset();
    autoResetCountdown[i] = 0;
  };

  auto ensureAutoTurn = [&]() {
    if (settings_.numPlayers == 2) {
      for (auto &b : boards) {
        if (b.outcome() == Board::Outcome::InProgress && b.turn() == 'X') {
          b.randomMove(rng);
        }
      }
    }
  };

  // En modo 2 jugadores, X comienza automáticamente en tableros nuevos.
  if (settings_.numPlayers == 2) ensureAutoTurn();

  int activeBoard = 0;
  int cursorIdx = 0;

  while (true) {
    int termH, termW;
    getmaxyx(stdscr, termH, termW);
    const Layout layout = computeLayout(termH, termW, static_cast<int>(boards.size()));

    if (!layout.fits) {
      UI::showMessageBox("Terminal pequena",
                         {"No hay espacio para mostrar todos los tableros.",
                          "Aumenta la ventana o reduce el numero de tableros en Ajustes."});
      return;
    }

    // Autoplay (0 jugadores) y turnos automáticos (2 jugadores)
    if (settings_.numPlayers == 0) {
      for (int i = 0; i < static_cast<int>(boards.size()); ++i) {
        auto &b = boards[i];
        if (b.outcome() == Board::Outcome::InProgress) {
          b.randomMove(rng);
        } else {
          if (autoResetCountdown[i] == 0) autoResetCountdown[i] = 12;
          if (--autoResetCountdown[i] <= 0) resetBoard(i);
        }
      }
    } else {
      ensureAutoTurn();
    }

    // Dibujo
    clear();
    for (int i = 0; i < static_cast<int>(boards.size()); ++i) {
      drawBoard(boards[i], layout.rects[i], i, i == activeBoard, cursorIdx);
    }

    // Barra inferior
    const std::string mode = (settings_.numPlayers == 0)
                                 ? "0 jugadores (autoplay)"
                                 : (settings_.numPlayers == 1) ? "1 jugador (X y O manual)" : "2 jugadores (O manual, X auto)";

    mvaddnstr(termH - 2, 0,
              ("Modo: " + mode + " | Tableros: " + std::to_string(settings_.numBoards)).c_str(),
              termW - 1);
    mvaddnstr(termH - 1, 0,
              "Flechas:mover  Enter/Espacio:jugar  Tab/[ ]:cambiar tablero  r/R:reset  q:menu",
              termW - 1);

    refresh();

    const int ch = getch();
    if (ch == ERR) continue;

    if (ch == KEY_RESIZE) continue;
    if (ch == 'q' || ch == 'Q') return;

    if (ch == 'R') {
      for (int i = 0; i < static_cast<int>(boards.size()); ++i) resetBoard(i);
      if (settings_.numPlayers == 2) ensureAutoTurn();
      continue;
    }

    if (ch == 'r') {
      resetBoard(activeBoard);
      if (settings_.numPlayers == 2) ensureAutoTurn();
      continue;
    }

    if (settings_.numPlayers == 0) continue; // sin control manual

    if (ch == '\t' || ch == ']') {
      activeBoard = (activeBoard + 1) % static_cast<int>(boards.size());
      cursorIdx = 0;
      continue;
    }
    if (ch == KEY_BTAB || ch == '[') {
      activeBoard = (activeBoard - 1 + static_cast<int>(boards.size())) % static_cast<int>(boards.size());
      cursorIdx = 0;
      continue;
    }

    // movimiento dentro del tablero activo
    int row, col;
    indexToCell(cursorIdx, row, col);
    if (ch == KEY_UP) row = (row + 2) % 3;
    if (ch == KEY_DOWN) row = (row + 1) % 3;
    if (ch == KEY_LEFT) col = (col + 2) % 3;
    if (ch == KEY_RIGHT) col = (col + 1) % 3;
    cursorIdx = cellToIndex(row, col);

    // jugar
    if (ch == 10 || ch == KEY_ENTER || ch == ' ') {
      auto &b = boards[activeBoard];
      if (b.outcome() != Board::Outcome::InProgress) {
        beep();
        continue;
      }

      if (settings_.numPlayers == 1) {
        if (!b.placeAt(cursorIdx)) beep();
      } else if (settings_.numPlayers == 2) {
        if (b.turn() != 'O') {
          beep();
          continue;
        }
        if (!b.placeAt(cursorIdx)) {
          beep();
          continue;
        }
        // Respuesta inmediata de X si toca.
        if (b.outcome() == Board::Outcome::InProgress && b.turn() == 'X') {
          b.randomMove(rng);
        }
      }
    }
  }
}
