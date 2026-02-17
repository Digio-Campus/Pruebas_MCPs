#include "../include/UI.h"
#include <cassert>
#include <iostream>

using namespace ttt;

int main() {
    GridLayout gl;
    gl.startX = 10; gl.startY = 5; gl.cellW = 4; gl.cellH = 2; gl.rows = 3; gl.cols = 3;
    // center of top-left cell
    auto c0 = getCellFromCoord(11, 6, gl);
    assert(c0 && c0->first == 0 && c0->second == 0);
    // center of bottom-right cell
    auto c1 = getCellFromCoord(10 + 4*2 + 1, 5 + 2*2 + 1, gl);
    assert(c1 && c1->first == 2 && c1->second == 2);
    // outside
    auto c2 = getCellFromCoord(0,0,gl);
    assert(!c2);
    // edges
    auto c3 = getCellFromCoord(10 + 4 - 1, 5 + 2 - 1, gl);
    assert(c3 && c3->first == 0 && c3->second == 0);
    std::cout << "unit_mouse_logic: OK\n";
    return 0;
}
