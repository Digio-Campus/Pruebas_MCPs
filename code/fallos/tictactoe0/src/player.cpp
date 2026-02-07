#include "../include/player.h"
#include <cstdlib>
#include <ctime>
#include <vector>

Player::Player(char sym, PlayerType t) : symbol(sym), type(t) {
    static bool seeded = false;
    if (!seeded) {
        srand(time(nullptr));
        seeded = true;
    }
}

char Player::getSymbol() const {
    return symbol;
}

PlayerType Player::getType() const {
    return type;
}

std::pair<int, int> Player::getAutoMove(const Board& board) {
    // Recopilar todas las posiciones vacías
    std::vector<std::pair<int, int>> empty_cells;
    
    for (int i = 0; i < 3; i++) {
        for (int j = 0; j < 3; j++) {
            if (board.getCell(i, j) == ' ') {
                empty_cells.push_back({i, j});
            }
        }
    }
    
    // Si hay celdas vacías, seleccionar una al azar
    if (!empty_cells.empty()) {
        int idx = rand() % empty_cells.size();
        return empty_cells[idx];
    }
    
    return {-1, -1};
}
