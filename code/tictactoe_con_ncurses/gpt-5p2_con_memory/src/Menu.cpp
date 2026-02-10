#include "Menu.h"

#include <ncurses.h>

Menu::Choice Menu::mainMenu(const UI& ui) const {
  int selected = 0;

  while (true) {
    ui.drawMainMenu(selected);

    int ch = getch();
    if (ch == KEY_UP) selected = (selected + 3) % 4;
    else if (ch == KEY_DOWN) selected = (selected + 1) % 4;
    else if (ch == 10 || ch == KEY_ENTER) {
      switch (selected) {
        case 0: return Choice::Play;
        case 1: return Choice::Settings;
        case 2: return Choice::Help;
        default: return Choice::Exit;
      }
    } else if (ch == 27 || ch == 'q' || ch == 'Q') {
      return Choice::Exit;
    } else if (ch == KEY_RESIZE) {
      // redibuja
    }
  }
}

void Menu::help(const UI& ui) const {
  ui.drawHelp();
  getch();
}

void Menu::settings(const UI& ui, Settings& s) const {
  s.clamp();
  int selected = 0;

  while (true) {
    int maxFit = ui.maxBoardsFit();
    if (s.boardsCount > maxFit) s.boardsCount = maxFit;
    ui.drawSettings(s, selected, maxFit);

    int ch = getch();
    if (ch == KEY_UP) selected = (selected + 2) % 3;
    else if (ch == KEY_DOWN) selected = (selected + 1) % 3;
    else if (ch == KEY_LEFT || ch == KEY_RIGHT) {
      int dir = (ch == KEY_RIGHT) ? 1 : -1;

      if (selected == 0) {
        int pm = static_cast<int>(s.playersMode) + dir;
        if (pm < 0) pm = 2;
        if (pm > 2) pm = 0;
        s.playersMode = static_cast<PlayersMode>(pm);
      } else if (selected == 1) {
        s.boardsCount += dir;
        s.clamp();
      }

    } else if (ch == 10 || ch == KEY_ENTER) {
      if (selected == 2) {
        s.clamp();
        return;
      }
    } else if (ch == 27 || ch == 'q' || ch == 'Q') {
      s.clamp();
      return;
    } else if (ch == KEY_RESIZE) {
      // redibuja
    }
  }
}
