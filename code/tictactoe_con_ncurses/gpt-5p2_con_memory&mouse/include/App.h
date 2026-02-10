#pragma once

#include "Menu.h"
#include "Settings.h"

class UI;

class App {
public:
    App();
    void run();

private:
    enum class State {
        MainMenu,
        Settings,
        Help,
        Game,
        Exit,
    };

    void runMainMenu_();
    void runSettings_();
    void runHelp_();
    void runGame_();

    State m_state{State::MainMenu};
    Settings m_settings;
    Menu m_menu;
    UI* m_ui; // creado en run() para asegurar endwin al salir
};
