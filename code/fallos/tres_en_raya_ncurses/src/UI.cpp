#include "UI.h"

#include <ncurses.h>

namespace {
void centerPrint(int y, const std::string &s, int attr = 0) {
  int h, w;
  getmaxyx(stdscr, h, w);
  (void)h;
  const int x = (w - static_cast<int>(s.size())) / 2;
  if (attr) attron(attr);
  mvaddnstr(y, std::max(0, x), s.c_str(), w - 1);
  if (attr) attroff(attr);
}
} // namespace

int UI::runMenu(const std::string &title, const std::vector<std::string> &items) {
  keypad(stdscr, TRUE);
  nodelay(stdscr, FALSE);

  int selected = 0;
  while (true) {
    clear();
    int h, w;
    getmaxyx(stdscr, h, w);

    centerPrint(1, title, A_BOLD);
    mvhline(2, 0, ACS_HLINE, w);

    const int startY = 4;
    for (int i = 0; i < static_cast<int>(items.size()); ++i) {
      const bool hl = (i == selected);
      if (hl) attron(A_REVERSE);
      centerPrint(startY + i, items[i]);
      if (hl) attroff(A_REVERSE);
    }

    centerPrint(h - 2, "Flechas: navegar | Enter: seleccionar | Esc: volver");
    refresh();

    const int ch = getch();
    if (ch == KEY_UP) {
      selected = (selected - 1 + static_cast<int>(items.size())) % static_cast<int>(items.size());
    } else if (ch == KEY_DOWN) {
      selected = (selected + 1) % static_cast<int>(items.size());
    } else if (ch == 10 || ch == KEY_ENTER) {
      return selected;
    } else if (ch == 27) {
      return -1;
    } else if (ch == KEY_RESIZE) {
      // se redibuja en el siguiente loop
    }
  }
}

void UI::showMessageBox(const std::string &title, const std::vector<std::string> &lines) {
  keypad(stdscr, TRUE);
  nodelay(stdscr, FALSE);

  while (true) {
    clear();
    int h, w;
    getmaxyx(stdscr, h, w);

    centerPrint(1, title, A_BOLD);
    mvhline(2, 0, ACS_HLINE, w);

    int y = 4;
    for (const auto &ln : lines) {
      centerPrint(y++, ln);
    }

    centerPrint(h - 2, "Pulsa cualquier tecla para continuar");
    refresh();

    const int ch = getch();
    if (ch == KEY_RESIZE) continue;
    break;
  }
}
