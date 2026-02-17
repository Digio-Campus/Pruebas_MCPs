#include <cassert>
#include <iostream>

// función simple que replica el mapeo de celda usado en UI
int getCellFromCoord(int relX, int relY, int cellW, int cellH) {
    int col = relX / cellW;
    int row = relY / cellH;
    if (row < 0) row = 0; if (row > 2) row = 2;
    if (col < 0) col = 0; if (col > 2) col = 2;
    return row*3 + col;
}

int main() {
    // cell size 4x2 (example)
    int cw = 4, ch = 2;
    assert(getCellFromCoord(0,0,cw,ch) == 0);     // esquina sup-izq
    assert(getCellFromCoord(3,1,cw,ch) == 0);     // dentro celda 0
    assert(getCellFromCoord(4,0,cw,ch) == 1);     // columna 1
    assert(getCellFromCoord(11,5,cw,ch) == 8);    // ultima celda
    assert(getCellFromCoord(-1,-1,cw,ch) == 0);   // fuera: clamp
    assert(getCellFromCoord(100,100,cw,ch) == 8); // fuera: clamp
    std::cout << "All mouse logic unit tests passed.\n";
    return 0;
}
