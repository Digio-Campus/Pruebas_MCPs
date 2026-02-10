#pragma once

#include <string>
#include <vector>
#include "Rect.h"
#include "Board.h"
#include "Menu.h"
#include "Settings.h"

// Encapsula inicialización/dibujo con ncurses.
class UI {
public:
    struct SettingsHitAreas {
        Rect playersMinus, playersPlus;
        Rect boardsMinus, boardsPlus;
        Rect back;
    };

    UI();
    ~UI();

    UI(const UI&) = delete;
    UI& operator=(const UI&) = delete;

    int termW() const { return m_termW; }
    int termH() const { return m_termH; }

    void updateSize();

    void setTimeoutMs(int ms);
    int getInput();

    void clear();
    void refresh();

    void drawFrameTitle(const std::string& title);
    void drawCenteredText(int y, const std::string& text, int colorPair = 0, bool bold = false);

    void drawMainMenu(const Menu& menu);
    SettingsHitAreas drawSettings(const Settings& settings, int focusedRow);
    void drawHelp();

    // Calcula layout de tableros (rect por tablero) para dibujar/hit-testing.
    std::vector<Rect> computeBoardRects(int numBoards) const;

    void drawGame(const std::vector<Board>& boards,
                  const std::vector<Rect>& boardRects,
                  int selectedBoard,
                  int cursorRow,
                  int cursorCol,
                  const Settings& settings,
                  const std::string& statusLine);

    void drawTooSmall(const std::string& msg);

private:
    void initColors_();
    void drawRectBorder_(const Rect& r, int colorPair, bool thick);

    int m_termW{0};
    int m_termH{0};
    bool m_colors{false};
};
