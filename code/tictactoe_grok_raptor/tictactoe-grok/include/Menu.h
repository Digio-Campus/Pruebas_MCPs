#ifndef MENU_H
#define MENU_H

#include "UI.h"
#include "Settings.h"

namespace ttt {

class Menu {
public:
    enum class MenuResult {
        Play,
        Settings,
        Help,
        Exit,
        Back
    };

    Menu(UI& ui, Settings& settings);

    MenuResult showMainMenu();
    void showSettingsMenu();
    void showHelp();

private:
    UI& ui_;
    Settings& settings_;
};

} // namespace ttt

#endif // MENU_H