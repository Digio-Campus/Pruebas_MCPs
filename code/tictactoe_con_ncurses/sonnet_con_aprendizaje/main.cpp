#include "UI.h"
#include "GameManager.h"
#include <cstdlib>
#include <ctime>

int main() {
    // Inicializar generador de números aleatorios
    srand(time(nullptr));
    
    // Crear el gestor del juego
    GameManager gameManager;
    
    // Crear la interfaz de usuario
    UI ui(gameManager);
    
    // Ejecutar el programa
    ui.run();
    
    return 0;
}
