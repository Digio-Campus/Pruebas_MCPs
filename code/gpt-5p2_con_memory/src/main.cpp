#include <exception>
#include <iostream>

#include "Game.h"
#include "Menu.h"
#include "NcursesSession.h"
#include "Settings.h"
#include "UI.h"

int main() {
  try {
    NcursesSession session;
    UI ui;
    Menu menu;
    Settings settings;

    while (true) {
      switch (menu.mainMenu(ui)) {
        case Menu::Choice::Play: {
          Game game(settings);
          game.run(ui);
          break;
        }
        case Menu::Choice::Settings:
          menu.settings(ui, settings);
          break;
        case Menu::Choice::Help:
          menu.help(ui);
          break;
        case Menu::Choice::Exit:
          return 0;
      }
    }

  } catch (const std::exception& e) {
    // Si ncurses no pudo inicializar, mostramos error por stderr.
    std::cerr << "Fatal: " << e.what() << "\n";
    return 1;
  }
}
