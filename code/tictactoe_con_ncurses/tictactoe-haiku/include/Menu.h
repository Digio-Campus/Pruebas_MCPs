#ifndef MENU_H
#define MENU_H

#include <vector>
#include <string>
#include "UI.h"
#include "Settings.h"

namespace ttt {

enum class MenuState { Main, Settings, Help, Playing, Exit };

class Menu {
public:
    Menu(UI& ui, Settings& settings);
    
    MenuState showMainMenu();
    MenuState showSettingsMenu();
    MenuState showHelpMenu();

private:
    UI& ui_;
    Settings& settings_;
};

}  // namespace ttt

#endif  // MENU_H
