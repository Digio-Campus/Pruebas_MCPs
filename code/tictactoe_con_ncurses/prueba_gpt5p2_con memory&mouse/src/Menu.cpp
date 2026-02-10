#include "Menu.h"

#include <vector>

namespace {
int maybeColor(const UI& ui, UI::ColorPair p) {
  return ui.colorsEnabled() ? COLOR_PAIR(p) : 0;
}
}

Menu::Action Menu::main(Settings& settings) {
  (void)settings;

  const std::vector<std::string> items = {"Jugar", "Ajustes", "Ayuda", "Salir"};
  int sel = 0;

  while (true) {
    ui_.refreshSize();
    ui_.clear();

    ui_.drawCentered(2, "Tres en Raya (ncurses)", A_BOLD | maybeColor(ui_, UI::CP_HILITE));
    ui_.drawCentered(4, "Menú principal", A_BOLD);

    const int startY = 6;
    for (int i = 0; i < static_cast<int>(items.size()); ++i) {
      const bool selected = (i == sel);
      int attrs = selected ? (A_REVERSE | maybeColor(ui_, UI::CP_CURSOR)) : maybeColor(ui_, UI::CP_NORMAL);
      ui_.drawCentered(startY + i, items[i], attrs);
    }

    ui_.drawCentered(ui_.termH() - 2, "Flechas: navegar | Enter: seleccionar | Esc: salir", maybeColor(ui_, UI::CP_NORMAL));

    refresh();

    const int ch = getch();
    if (ch == KEY_UP) {
      sel = (sel - 1 + static_cast<int>(items.size())) % static_cast<int>(items.size());
    } else if (ch == KEY_DOWN) {
      sel = (sel + 1) % static_cast<int>(items.size());
    } else if (ch == 10 || ch == KEY_ENTER) {
      if (sel == 0) return Action::Play;
      if (sel == 1) {
        this->settings(settings);
      } else if (sel == 2) {
        help();
      } else if (sel == 3) {
        return Action::Exit;
      }
    } else if (ch == 27 || ch == 'q' || ch == 'Q') {
      return Action::Exit;
    }
  }
}

void Menu::settings(Settings& settings) {
  int sel = 0; // 0=players, 1=boards

  while (true) {
    ui_.refreshSize();
    const Layout l = ui_.calcLayout();
    settings.clampToMaxBoards(l.maxBoards);

    ui_.clear();
    ui_.drawCentered(2, "Ajustes", A_BOLD | maybeColor(ui_, UI::CP_HILITE));

    const std::string playersLine = "Número de jugadores:  " + Settings::playersLabel(settings.players);
    const std::string boardsLine = "Número de tableros:   " + std::to_string(settings.boards) +
                                   "   (máx visible ahora: " + std::to_string(l.maxBoards) + ")";

    const int startY = 6;
    ui_.drawCentered(startY + 0, playersLine, (sel == 0) ? (A_REVERSE | maybeColor(ui_, UI::CP_CURSOR)) : 0);
    ui_.drawCentered(startY + 2, boardsLine, (sel == 1) ? (A_REVERSE | maybeColor(ui_, UI::CP_CURSOR)) : 0);

    ui_.drawCentered(startY + 5, "Flechas ↑↓: seleccionar | ←→: cambiar | Enter/Esc: volver", maybeColor(ui_, UI::CP_NORMAL));

    refresh();

    const int ch = getch();
    if (ch == KEY_UP) {
      sel = (sel - 1 + 2) % 2;
    } else if (ch == KEY_DOWN) {
      sel = (sel + 1) % 2;
    } else if (ch == KEY_LEFT) {
      if (sel == 0) {
        int v = static_cast<int>(settings.players);
        v = (v - 1 + 3) % 3;
        settings.players = static_cast<PlayersMode>(v);
      } else {
        settings.boards = std::max(1, settings.boards - 1);
      }
    } else if (ch == KEY_RIGHT) {
      if (sel == 0) {
        int v = static_cast<int>(settings.players);
        v = (v + 1) % 3;
        settings.players = static_cast<PlayersMode>(v);
      } else {
        settings.boards = std::min(l.maxBoards, settings.boards + 1);
      }
    } else if (ch == 10 || ch == KEY_ENTER || ch == 27) {
      return;
    }
  }
}

void Menu::help() {
  ui_.refreshSize();
  ui_.clear();

  ui_.drawCentered(2, "Ayuda", A_BOLD | maybeColor(ui_, UI::CP_HILITE));

  int y = 5;
  mvaddstr(y++, 2, "Controles en partida:");
  mvaddstr(y++, 4, "- Flechas: mover cursor por casillas (en el tablero seleccionado)");
  mvaddstr(y++, 4, "- Enter: colocar ficha (si el turno del tablero lo permite)");
  mvaddstr(y++, 4, "- Tab o ]/[ : cambiar de tablero");
  mvaddstr(y++, 4, "- r: reiniciar tablero seleccionado (estadísticas se mantienen)");
  mvaddstr(y++, 4, "- a: reiniciar todos los tableros (estadísticas se mantienen)");
  mvaddstr(y++, 4, "- q o Esc: volver al menú");
  y++;
  mvaddstr(y++, 2, "Modos:");
  mvaddstr(y++, 4, "- 0 jugadores: X y O se colocan automáticamente de forma aleatoria");
  mvaddstr(y++, 4, "- 1 jugador: controlas X y O manualmente (turnos alternos por tablero)");
  mvaddstr(y++, 4, "- 2 jugadores: tú juegas O y X se genera automáticamente (aleatoria)");
  y++;
  mvaddstr(y++, 2, "Reglas:");
  mvaddstr(y++, 4, "- Cada tablero es independiente y empieza con X");
  mvaddstr(y++, 4, "- Alternancia estricta por tablero: X -> O -> X -> O ...");
  mvaddstr(y++, 4, "- Se detecta victoria (3 en línea) o empate automáticamente");

  ui_.drawCentered(ui_.termH() - 2, "Pulsa cualquier tecla para volver", maybeColor(ui_, UI::CP_NORMAL));
  refresh();
  getch();
}
