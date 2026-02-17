#include "../include/Menu.h"
#include "../include/UI.h"
#include <ncurses.h>

using namespace ttt;

int Menu::run(const std::vector<std::string>& items)
{
    UI ui;
    int sel = 0;
    while (true) {
        ui.drawMenu(items, sel);
        int ch = getch();
        if (ch == KEY_UP) { sel = (sel - 1 + (int)items.size()) % (int)items.size(); }
        else if (ch == KEY_DOWN) { sel = (sel + 1) % (int)items.size(); }
        else if (ch == KEY_MOUSE) {
            MEVENT m; if (getmouse(&m) == OK) {
                // map y to item roughly
                int itemIdx = (m.y - 3);
                if (itemIdx >= 0 && itemIdx < (int)items.size()) return itemIdx;
            }
        }
        else if (ch == '\n' || ch == KEY_ENTER) return sel;
        else if (ch == 'q' || ch == 'Q') return -1;
    }
}
