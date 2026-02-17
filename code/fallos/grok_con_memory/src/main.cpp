#include "UI.h"
#include "Game.h"
#include <cstdlib>
#include <ctime>

int main() {
    srand(time(NULL)); // Para movimientos aleatorios
    Game game(1, 1); // Default: 1 jugador, 1 tablero
    UI ui(&game);
    ui.init();
    ui.run();
    return 0;
}