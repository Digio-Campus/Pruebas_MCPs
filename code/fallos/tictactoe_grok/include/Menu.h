#ifndef MENU_H
#define MENU_H

#include <vector>
#include <string>

class UI;
class Settings;

class Menu {
private:
    UI& ui;
    Settings& settings;

    int navigateMenu(const std::vector<std::string>& options, int initialSelection = 0);

public:
    Menu(UI& uiRef, Settings& settingsRef);
    ~Menu() = default;

    // Menu screens
    int showMainMenu();
    void showSettingsMenu();
    void showHelp();
};

#endif // MENU_H