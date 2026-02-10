#pragma once

#include <string>
#include <vector>
#include "Rect.h"

class Menu {
public:
    explicit Menu(std::vector<std::string> items);

    const std::vector<std::string>& items() const { return m_items; }

    int selected() const { return m_selected; }
    void setSelected(int idx);

    void moveUp();
    void moveDown();

    // Calcula rectángulos clicables (uno por item) para ratón.
    void layout(int termW, int termH);
    const std::vector<Rect>& itemRects() const { return m_itemRects; }

    int hitTest(int x, int y) const;

private:
    std::vector<std::string> m_items;
    int m_selected{0};
    std::vector<Rect> m_itemRects;
};
