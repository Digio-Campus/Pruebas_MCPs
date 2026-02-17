#ifndef MENU_H
#define MENU_H

#include "UI.h"
#include "Settings.h"

class Menu {
private:
    UI& ui;
    Settings& settings;

public:
    Menu(UI& ui, Settings& settings);
    int showMainMenu();
    void showSettingsMenu();
};

#endif // MENU_H