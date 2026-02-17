#ifndef TICTACTOE_MENU_H
#define TICTACTOE_MENU_H

#include <vector>
#include <string>

class Menu {
public:
    Menu();
    const std::vector<std::string>& options() const;
    void up();
    void down();
    int current() const;
private:
    std::vector<std::string> m_opts;
    int m_cur;
};

#endif // TICTACTOE_MENU_H
