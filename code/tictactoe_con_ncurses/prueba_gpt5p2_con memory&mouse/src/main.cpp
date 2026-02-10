#include "Game.h"
#include "Menu.h"
#include "Settings.h"
#include "UI.h"

int main() {
  UI ui;
  Settings settings;

  Menu menu(ui);
  Game game(ui);

  while (true) {
    const auto action = menu.main(settings);
    if (action == Menu::Action::Exit) break;

    // Ajuste final por tamaño actual.
    settings.clampToMaxBoards(ui.calcLayout().maxBoards);

    game.run(settings);
  }

  return 0;
}
