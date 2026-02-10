#pragma once

// Rectángulo simple para layout y hit-testing (ratón).
struct Rect {
    int x{0};
    int y{0};
    int w{0};
    int h{0};

    bool contains(int px, int py) const {
        return px >= x && px < (x + w) && py >= y && py < (y + h);
    }

    Rect inset(int dx, int dy) const {
        Rect r = *this;
        r.x += dx;
        r.y += dy;
        r.w -= 2 * dx;
        r.h -= 2 * dy;
        if (r.w < 0) r.w = 0;
        if (r.h < 0) r.h = 0;
        return r;
    }
};
