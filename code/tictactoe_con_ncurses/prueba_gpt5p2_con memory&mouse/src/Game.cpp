#include "Game.h"

#include <algorithm>
#include <chrono>

namespace {
int nowMs() {
  using namespace std::chrono;
  return static_cast<int>(duration_cast<milliseconds>(steady_clock::now().time_since_epoch()).count());
}

int maybeColor(const UI& ui, UI::ColorPair p) {
  return ui.colorsEnabled() ? COLOR_PAIR(p) : 0;
}

const char* resultLabel(Board::Result r) {
  switch (r) {
    case Board::Result::InProgress:
      return "Jugando";
    case Board::Result::XWins:
      return "Gana X";
    case Board::Result::OWins:
      return "Gana O";
    case Board::Result::Draw:
      return "Empate";
  }
  return "";
}

bool inWinningLine(const Board& b, int r, int c) {
  auto wl = b.winningLine();
  if (!wl) return false;
  for (const auto& [rr, cc] : *wl) {
    if (rr == r && cc == c) return true;
  }
  return false;
}

void drawSeparator(int y, int x) {
  mvaddstr(y, x, "+---+---+---+");
}

void drawCellLine(int y, int x) {
  mvaddstr(y, x, "|   |   |   |");
}
}

void Game::clampSettingsToScreen(Settings& settings) {
  ui_.refreshSize();
  const Layout l = ui_.calcLayout();
  settings.clampToMaxBoards(l.maxBoards);
}

void Game::ensureBoards(const Settings& settings) {
  if (static_cast<int>(boards_.size()) == settings.boards) return;

  boards_.assign(static_cast<size_t>(settings.boards), Board{});
  cursorR_.assign(static_cast<size_t>(settings.boards), 1);
  cursorC_.assign(static_cast<size_t>(settings.boards), 1);
  selectedBoard_ = 0;
}

void Game::resetSelected() {
  if (boards_.empty()) return;
  boards_[static_cast<size_t>(selectedBoard_)].reset();
}

void Game::resetAll() {
  for (auto& b : boards_) b.reset();
}

void Game::nextBoard(int delta) {
  if (boards_.empty()) return;
  const int n = static_cast<int>(boards_.size());
  selectedBoard_ = (selectedBoard_ + delta) % n;
  if (selectedBoard_ < 0) selectedBoard_ += n;
}

void Game::maybeRunBots(const Settings& settings) {
  if (boards_.empty()) return;

  const int t = nowMs();

  // Modo 0: ticks periódicos para dar tiempo a ver cómo se rellenan.
  if (settings.players == PlayersMode::Zero) {
    if (t - lastAutoTickMs_ < 120) return;
    lastAutoTickMs_ = t;

    for (auto& b : boards_) {
      if (!b.isFinished()) b.makeRandomMove(rng_);
    }
    return;
  }

  // Modo 2: X automático siempre que toque.
  if (settings.players == PlayersMode::Two) {
    for (auto& b : boards_) {
      if (!b.isFinished() && b.currentTurn() == 'X') {
        b.makeRandomMove(rng_);
      }
    }
  }
}

void Game::draw(const Settings& settings) {
  ui_.refreshSize();
  ui_.clear();

  const Layout l = ui_.calcLayout();
  if (ui_.termH() < 12 || ui_.termW() < 20) {
    ui_.drawCentered(2, "Terminal demasiado pequeña.", A_BOLD | maybeColor(ui_, UI::CP_WARN));
    ui_.drawCentered(4, "Redimensiona la ventana o pulsa q para volver.");
    refresh();
    return;
  }

  auto modeLabel = [&]() {
    switch (settings.players) {
      case PlayersMode::Zero:
        return "0 jugadores (auto)";
      case PlayersMode::One:
        return "1 jugador (manual X/O)";
      case PlayersMode::Two:
        return "2 jugadores (tú=O, X auto)";
    }
    return "";
  };

  ui_.drawCentered(0, std::string("Partida — ") + modeLabel(), A_BOLD | maybeColor(ui_, UI::CP_HILITE));

  const int shownBoards = std::min(settings.boards, l.maxBoards);

  for (int i = 0; i < shownBoards; ++i) {
    const int gridCol = i % l.cols;
    const int gridRow = i / l.cols;

    const int x0 = l.marginX + gridCol * (l.boardW + l.padX);
    const int y0 = l.marginY + 2 + gridRow * (l.boardH + l.padY);

    const Board& b = boards_[static_cast<size_t>(i)];
    const bool isSel = (i == selectedBoard_);

    int titleAttrs = A_BOLD | (isSel ? maybeColor(ui_, UI::CP_HILITE) : 0);

    std::string title = "Tablero " + std::to_string(i + 1) + "  [" + resultLabel(b.result()) + "]";
    if (b.result() == Board::Result::InProgress) {
      title += "  Turno: ";
      title.push_back(b.currentTurn());
    }
    mvaddnstr(y0 + 0, x0, title.c_str(), l.boardW);
    if (titleAttrs) {
      mvchgat(y0 + 0, x0, l.boardW, titleAttrs, 0, nullptr);
    }

    std::string stats = "X:" + std::to_string(b.xWins()) + "  O:" + std::to_string(b.oWins()) + "  D:" + std::to_string(b.draws());
    mvaddnstr(y0 + 1, x0, stats.c_str(), l.boardW);

    const int gy = y0 + 2;
    for (int r = 0; r < 3; ++r) {
      drawSeparator(gy + r * 2, x0);
      drawCellLine(gy + r * 2 + 1, x0);

      for (int c = 0; c < 3; ++c) {
        const bool isCursor = isSel && (cursorR_[static_cast<size_t>(i)] == r) && (cursorC_[static_cast<size_t>(i)] == c);
        const bool isWin = inWinningLine(b, r, c);

        int attrs = 0;
        if (isWin) attrs |= A_BOLD | maybeColor(ui_, UI::CP_WIN);
        if (isCursor) attrs |= A_REVERSE | maybeColor(ui_, UI::CP_CURSOR);

        const int cellX = x0 + 1 + c * 4;
        const char mark = b.cell(r, c);

        if (attrs) attron(attrs);
        mvaddch(gy + r * 2 + 1, cellX + 0, ' ');
        mvaddch(gy + r * 2 + 1, cellX + 1, mark);
        mvaddch(gy + r * 2 + 1, cellX + 2, ' ');
        if (attrs) attroff(attrs);
      }
    }
    drawSeparator(gy + 6, x0);

    if (isSel) {
      mvaddch(y0 + 0, x0 - 1, '>');
      mvaddch(y0 + l.boardH - 1, x0 - 1, '>');
      mvaddch(y0 + 0, x0 + l.boardW, '<');
      mvaddch(y0 + l.boardH - 1, x0 + l.boardW, '<');
    }
  }

  const int footerY = ui_.termH() - 2;
  mvaddnstr(footerY + 0, 1,
            "Controles: Flechas mover | Enter jugar | Tab/[ ] cambiar tablero | r reset tablero | a reset todos | q/Esc volver",
            ui_.termW() - 2);

  if (settings.boards > l.maxBoards) {
    const std::string warn = "Aviso: " + std::to_string(settings.boards) + " tableros configurados, pero solo caben " +
                             std::to_string(l.maxBoards) + " simultáneamente. (Se limita en pantalla)";
    mvaddnstr(footerY + 1, 1, warn.c_str(), ui_.termW() - 2);
    mvchgat(footerY + 1, 1, ui_.termW() - 2, maybeColor(ui_, UI::CP_WARN), 0, nullptr);
  } else {
    mvaddnstr(footerY + 1, 1, "Tip: cada tablero es independiente (turno, victoria, empate y reinicios).", ui_.termW() - 2);
  }

  refresh();
}

void Game::run(Settings settings) {
  clampSettingsToScreen(settings);
  ensureBoards(settings);

  nodelay(stdscr, TRUE); // permite animación en modo 0 y bots sin bloquear.
  lastAutoTickMs_ = nowMs();

  while (true) {
    clampSettingsToScreen(settings);

    // Si el usuario cambió tamaño y ahora caben menos, ajustamos "lógico" también.
    const Layout l = ui_.calcLayout();
    if (settings.boards > l.maxBoards) settings.boards = l.maxBoards;
    ensureBoards(settings);

    maybeRunBots(settings);
    draw(settings);

    const int ch = getch();
    if (ch == ERR) {
      napms(10);
      continue;
    }

    if (ch == KEY_RESIZE) {
      ui_.refreshSize();
      continue;
    }

    if (ch == 'q' || ch == 'Q' || ch == 27) {
      break;
    }

    if (ch == '\t' || ch == ']') {
      nextBoard(+1);
      continue;
    }
    if (ch == '[') {
      nextBoard(-1);
      continue;
    }

    if (ch == 'r' || ch == 'R') {
      resetSelected();
      continue;
    }
    if (ch == 'a' || ch == 'A') {
      resetAll();
      continue;
    }

    if (boards_.empty()) continue;

    Board& b = boards_[static_cast<size_t>(selectedBoard_)];
    int& cr = cursorR_[static_cast<size_t>(selectedBoard_)];
    int& cc = cursorC_[static_cast<size_t>(selectedBoard_)];

    if (ch == KEY_UP) cr = std::max(0, cr - 1);
    else if (ch == KEY_DOWN) cr = std::min(2, cr + 1);
    else if (ch == KEY_LEFT) cc = std::max(0, cc - 1);
    else if (ch == KEY_RIGHT) cc = std::min(2, cc + 1);
    else if (ch == 10 || ch == KEY_ENTER) {
      if (settings.players == PlayersMode::One) {
        b.makeMove(cr, cc);
      } else if (settings.players == PlayersMode::Two) {
        // El jugador solo pone O; X es automática.
        if (b.currentTurn() == 'O') b.makeMove(cr, cc);
      } else {
        // 0 jugadores: no hay jugada manual.
      }
    }

    napms(10);
  }

  nodelay(stdscr, FALSE);
}
