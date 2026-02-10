#include <iostream>
#include <vector>
#include <limits>

class TresEnRaya {
private:
    std::vector<std::vector<char>> tablero;
    char jugadorActual;
    
    void inicializarTablero() {
        tablero = std::vector<std::vector<char>>(3, std::vector<char>(3, ' '));
    }
    
    void mostrarTablero() {
        std::cout << "\n  0   1   2\n";
        for (int i = 0; i < 3; i++) {
            std::cout << i << " ";
            for (int j = 0; j < 3; j++) {
                std::cout << tablero[i][j];
                if (j < 2) std::cout << " | ";
            }
            std::cout << "\n";
            if (i < 2) std::cout << "  ---------\n";
        }
        std::cout << "\n";
    }
    
    bool movimientoValido(int fila, int col) {
        return fila >= 0 && fila < 3 && col >= 0 && col < 3 && tablero[fila][col] == ' ';
    }
    
    bool verificarGanador(char jugador) {
        // Verificar filas y columnas
        for (int i = 0; i < 3; i++) {
            if ((tablero[i][0] == jugador && tablero[i][1] == jugador && tablero[i][2] == jugador) ||
                (tablero[0][i] == jugador && tablero[1][i] == jugador && tablero[2][i] == jugador)) {
                return true;
            }
        }
        
        // Verificar diagonales
        if ((tablero[0][0] == jugador && tablero[1][1] == jugador && tablero[2][2] == jugador) ||
            (tablero[0][2] == jugador && tablero[1][1] == jugador && tablero[2][0] == jugador)) {
            return true;
        }
        
        return false;
    }
    
    bool tableroLleno() {
        for (int i = 0; i < 3; i++) {
            for (int j = 0; j < 3; j++) {
                if (tablero[i][j] == ' ') return false;
            }
        }
        return true;
    }
    
    void cambiarJugador() {
        jugadorActual = (jugadorActual == 'X') ? 'O' : 'X';
    }
    
public:
    TresEnRaya() : jugadorActual('X') {
        inicializarTablero();
    }
    
    void jugar() {
        int fila, col;
        bool juegoActivo = true;
        
        std::cout << "=== TRES EN RAYA ===\n";
        std::cout << "Jugador X vs Jugador O\n";
        
        while (juegoActivo) {
            mostrarTablero();
            std::cout << "Turno del jugador " << jugadorActual << "\n";
            std::cout << "Ingrese fila (0-2): ";
            
            while (!(std::cin >> fila)) {
                std::cin.clear();
                std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
                std::cout << "Entrada invalida. Ingrese fila (0-2): ";
            }
            
            std::cout << "Ingrese columna (0-2): ";
            while (!(std::cin >> col)) {
                std::cin.clear();
                std::cin.ignore(std::numeric_limits<std::streamsize>::max(), '\n');
                std::cout << "Entrada invalida. Ingrese columna (0-2): ";
            }
            
            if (!movimientoValido(fila, col)) {
                std::cout << "Movimiento invalido. Intente de nuevo.\n";
                continue;
            }
            
            tablero[fila][col] = jugadorActual;
            
            if (verificarGanador(jugadorActual)) {
                mostrarTablero();
                std::cout << "¡Jugador " << jugadorActual << " gana!\n";
                juegoActivo = false;
            } else if (tableroLleno()) {
                mostrarTablero();
                std::cout << "¡Empate!\n";
                juegoActivo = false;
            } else {
                cambiarJugador();
            }
        }
        
        std::cout << "\n¿Jugar de nuevo? (s/n): ";
        char respuesta;
        std::cin >> respuesta;
        if (respuesta == 's' || respuesta == 'S') {
            inicializarTablero();
            jugadorActual = 'X';
            jugar();
        }
    }
};

int main() {
    TresEnRaya juego;
    juego.jugar();
    return 0;
}
