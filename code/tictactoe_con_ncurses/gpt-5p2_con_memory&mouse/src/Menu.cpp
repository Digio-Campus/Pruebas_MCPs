#include "Menu.h"

Menu::Menu(std::vector<std::string> items)
    : m_items(std::move(items)) {
    if (m_items.empty()) m_items.push_back("(vacío)");
    m_itemRects.resize(m_items.size());
}

void Menu::setSelected(int idx) {
    if (idx < 0) idx = 0;
    if (idx >= (int)m_items.size()) idx = (int)m_items.size() - 1;
    m_selected = idx;
}

void Menu::moveUp() {
    setSelected(m_selected - 1);
}

void Menu::moveDown() {
    setSelected(m_selected + 1);
}

void Menu::layout(int termW, int termH) {
    int maxLen = 0;
    for (auto& it : m_items) if ((int)it.size() > maxLen) maxLen = (int)it.size();

    int w = maxLen + 10;
    int h = (int)m_items.size() * 2 + 4;
    int x = (termW - w) / 2;
    int y = (termH - h) / 2;

    for (int i = 0; i < (int)m_items.size(); ++i) {
        // Rect para el "botón" del item.
        m_itemRects[i] = Rect{ x + 2, y + 2 + i * 2, w - 4, 1 };
    }
}

int Menu::hitTest(int x, int y) const {
    for (int i = 0; i < (int)m_itemRects.size(); ++i) {
        if (m_itemRects[i].contains(x, y)) return i;
    }
    return -1;
}
