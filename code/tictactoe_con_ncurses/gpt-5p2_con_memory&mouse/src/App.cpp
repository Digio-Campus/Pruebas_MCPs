#include "App.h"

#include "Game.h"
#include "UI.h"

#include <ncurses.h>

App::App()
    : m_menu({"Jugar", "Ajustes", "Ayuda", "Salir"}),
      m_ui(nullptr) {}

void App::run() {
    UI ui;
    m_ui = &ui;

    while (m_state != State::Exit) {
        switch (m_state) {
            case State::MainMenu: runMainMenu_(); break;
            case State::Settings: runSettings_(); break;
            case State::Help: runHelp_(); break;
            case State::Game: runGame_(); break;
            case State::Exit: break;
        }
    }

    m_ui = nullptr;
}

void App::runMainMenu_() {
    m_ui->setTimeoutMs(-1);

    while (m_state == State::MainMenu) {
        m_ui->updateSize();
        m_menu.layout(m_ui->termW(), m_ui->termH());

        m_ui->clear();
        m_ui->drawMainMenu(m_menu);
        m_ui->refresh();

        int ch = m_ui->getInput();
        if (ch == KEY_RESIZE) continue;

        if (ch == KEY_UP) m_menu.moveUp();
        else if (ch == KEY_DOWN) m_menu.moveDown();
        else if (ch == '\n' || ch == KEY_ENTER) {
            int sel = m_menu.selected();
            if (sel == 0) m_state = State::Game;
            else if (sel == 1) m_state = State::Settings;
            else if (sel == 2) m_state = State::Help;
            else if (sel == 3) m_state = State::Exit;
        } else if (ch == KEY_MOUSE) {
            MEVENT ev{};
            if (getmouse(&ev) == OK) {
                if (ev.bstate & (BUTTON1_CLICKED | BUTTON1_PRESSED | BUTTON1_RELEASED)) {
                    int idx = m_menu.hitTest(ev.x, ev.y);
                    if (idx >= 0) {
                        m_menu.setSelected(idx);
                        if (idx == 0) m_state = State::Game;
                        else if (idx == 1) m_state = State::Settings;
                        else if (idx == 2) m_state = State::Help;
                        else if (idx == 3) m_state = State::Exit;
                    }
                }
            }
        }
    }
}

void App::runSettings_() {
    m_ui->setTimeoutMs(-1);

    int focus = 0; // 0 jugadores, 1 tableros, 2 volver

    while (m_state == State::Settings) {
        m_ui->updateSize();

        m_ui->clear();
        UI::SettingsHitAreas areas = m_ui->drawSettings(m_settings, focus);
        m_ui->refresh();

        int ch = m_ui->getInput();
        if (ch == KEY_RESIZE) continue;

        auto decPlayers = [&]() { m_settings.setNumPlayers(m_settings.numPlayers() - 1); };
        auto incPlayers = [&]() { m_settings.setNumPlayers(m_settings.numPlayers() + 1); };
        auto decBoards  = [&]() { m_settings.setNumBoards(m_settings.numBoards() - 1); };
        auto incBoards  = [&]() { m_settings.setNumBoards(m_settings.numBoards() + 1); };

        if (ch == 'q' || ch == 'Q' || ch == 27 /*ESC*/) {
            m_state = State::MainMenu;
        } else if (ch == KEY_UP) {
            focus = (focus + 2) % 3;
        } else if (ch == KEY_DOWN) {
            focus = (focus + 1) % 3;
        } else if (ch == KEY_LEFT) {
            if (focus == 0) decPlayers();
            else if (focus == 1) decBoards();
        } else if (ch == KEY_RIGHT) {
            if (focus == 0) incPlayers();
            else if (focus == 1) incBoards();
        } else if (ch == '\n' || ch == KEY_ENTER) {
            if (focus == 2) m_state = State::MainMenu;
        } else if (ch == KEY_MOUSE) {
            MEVENT ev{};
            if (getmouse(&ev) == OK) {
                if (ev.bstate & (BUTTON1_CLICKED | BUTTON1_PRESSED | BUTTON1_RELEASED)) {
                    if (areas.playersMinus.contains(ev.x, ev.y)) decPlayers();
                    else if (areas.playersPlus.contains(ev.x, ev.y)) incPlayers();
                    else if (areas.boardsMinus.contains(ev.x, ev.y)) decBoards();
                    else if (areas.boardsPlus.contains(ev.x, ev.y)) incBoards();
                    else if (areas.back.contains(ev.x, ev.y)) m_state = State::MainMenu;
                }
            }
        }
    }
}

void App::runHelp_() {
    m_ui->setTimeoutMs(-1);

    while (m_state == State::Help) {
        m_ui->updateSize();
        m_ui->clear();
        m_ui->drawHelp();
        m_ui->refresh();

        int ch = m_ui->getInput();
        if (ch == KEY_RESIZE) continue;
        if (ch == KEY_MOUSE) {
            // cualquier clic vuelve
            MEVENT ev{};
            if (getmouse(&ev) == OK) {
                if (ev.bstate & (BUTTON1_CLICKED | BUTTON1_PRESSED | BUTTON1_RELEASED)) {
                    m_state = State::MainMenu;
                }
            }
        } else {
            m_state = State::MainMenu;
        }
    }
}

void App::runGame_() {
    Game game(m_settings);
    game.run(*m_ui, m_settings);
    m_state = State::MainMenu;
}
