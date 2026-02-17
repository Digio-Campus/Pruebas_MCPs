#include "../include/Menu.h"

Menu::Menu() : m_cur(0) {
    m_opts.push_back("Jugar");
    m_opts.push_back("Ajustes");
    m_opts.push_back("Ayuda");
    m_opts.push_back("Salir");
}

const std::vector<std::string>& Menu::options() const { return m_opts; }

void Menu::up() { if (m_cur > 0) --m_cur; else m_cur = (int)m_opts.size()-1; }
void Menu::down() { if (m_cur + 1 < (int)m_opts.size()) ++m_cur; else m_cur = 0; }
int Menu::current() const { return m_cur; }
