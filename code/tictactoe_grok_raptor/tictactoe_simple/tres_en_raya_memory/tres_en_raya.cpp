#include <iostream>
#include <vector>
#include <limits>

using namespace std;

class TresEnRaya {
private:
    vector<vector<char>> tablero;
    char jugadorActual;

    void inicializarTablero() {
        tablero = vector<vector<char>>(3, vector<char>(3, ' '));
        jugadorActual = 'X';
    }

    void mostrarTablero() {
        cout << "\n  0   1   2\n";
        for (int i = 0; i < 3; i++) {
            cout << i << " ";
            for (int j = 0; j < 3; j++) {
                cout << tablero[i][j];
                if (j < 2) cout << " | ";
            }
            cout << "\n";
            if (i < 2) cout << "  -----------\n";
        }
        cout << "\n";
    }

    bool movimientoValido(int fila, int columna) {
        return fila >= 0 && fila < 3 && columna >= 0 && columna < 3 && tablero[fila][columna] == ' ';
    }

    bool verificarGanador() {
        // Verificar filas y columnas en un solo bucle
        for (int i = 0; i < 3; i++) {
            if (tablero[i][0] == jugadorActual && tablero[i][1] == jugadorActual && tablero[i][2] == jugadorActual)
                return true;
            if (tablero[0][i] == jugadorActual && tablero[1][i] == jugadorActual && tablero[2][i] == jugadorActual)
                return true;
        }

        // Verificar diagonales
        if (tablero[0][0] == jugadorActual && tablero[1][1] == jugadorActual && tablero[2][2] == jugadorActual)
            return true;
        if (tablero[0][2] == jugadorActual && tablero[1][1] == jugadorActual && tablero[2][0] == jugadorActual)
            return true;

        return false;
    }

    bool tableroLleno() {
        for (int i = 0; i < 3; i++) {
            for (int j = 0; j < 3; j++) {
                if (tablero[i][j] == ' ')
                    return false;
            }
        }
        return true;
    }

    void cambiarJugador() {
        jugadorActual = (jugadorActual == 'X') ? 'O' : 'X';
    }

    void realizarMovimiento() {
        int fila, columna;
        bool movimientoExitoso = false;

        while (!movimientoExitoso) {
            cout << "Jugador " << jugadorActual << ", ingresa fila (0-2): ";
            if (!(cin >> fila)) {
                cin.clear();
                cin.ignore(numeric_limits<streamsize>::max(), '\n');
                cout << "Entrada inválida. Por favor ingresa un número.\n";
                continue;
            }

            cout << "Jugador " << jugadorActual << ", ingresa columna (0-2): ";
            if (!(cin >> columna)) {
                cin.clear();
                cin.ignore(numeric_limits<streamsize>::max(), '\n');
                cout << "Entrada inválida. Por favor ingresa un número.\n";
                continue;
            }

            if (movimientoValido(fila, columna)) {
                tablero[fila][columna] = jugadorActual;
                movimientoExitoso = true;
            } else {
                cout << "Movimiento inválido. Intenta de nuevo.\n";
            }
        }
    }

public:
    void jugar() {
        inicializarTablero();
        cout << "¡Bienvenido al Tres en Raya!\n";

        while (true) {
            mostrarTablero();
            realizarMovimiento();

            if (verificarGanador()) {
                mostrarTablero();
                cout << "¡Jugador " << jugadorActual << " ha ganado!\n";
                break;
            }

            if (tableroLleno()) {
                mostrarTablero();
                cout << "¡Es un empate!\n";
                break;
            }

            cambiarJugador();
        }

        char respuesta;
        cout << "¿Quieres jugar de nuevo? (s/n): ";
        cin >> respuesta;
        if (respuesta == 's' || respuesta == 'S') {
            jugar();
        }
    }
};

int main() {
    TresEnRaya juego;
    juego.jugar();
    return 0;
}
