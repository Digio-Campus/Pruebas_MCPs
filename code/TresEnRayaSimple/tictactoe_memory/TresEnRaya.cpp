#include <iostream>
#include <vector>
#include <limits>
using namespace std;

class TresEnRaya {
    vector<vector<char>> tablero;
    char jugadorActual;

    void renderizarTablero() {
        cout << "\n  0 1 2" << endl;
        for (int i = 0; i < 3; ++i) {
            cout << i << " ";
            for (int j = 0; j < 3; ++j) {
                cout << tablero[i][j];
                if (j < 2) cout << "|";
            }
            cout << endl;
            if (i < 2) cout << "  -+-+-" << endl;
        }
    }

    bool movimientoValido(int fila, int col) {
        return fila >= 0 && fila < 3 && col >= 0 && col < 3 && tablero[fila][col] == ' ';
    }

    bool hayGanador() {
        for (int i = 0; i < 3; ++i) {
            if (tablero[i][0] != ' ' && tablero[i][0] == tablero[i][1] && tablero[i][1] == tablero[i][2]) return true;
            if (tablero[0][i] != ' ' && tablero[0][i] == tablero[1][i] && tablero[1][i] == tablero[2][i]) return true;
        }
        if (tablero[0][0] != ' ' && tablero[0][0] == tablero[1][1] && tablero[1][1] == tablero[2][2]) return true;
        if (tablero[0][2] != ' ' && tablero[0][2] == tablero[1][1] && tablero[1][1] == tablero[2][0]) return true;
        return false;
    }

    bool tableroLleno() {
        for (auto &fila : tablero)
            for (char c : fila)
                if (c == ' ') return false;
        return true;
    }

    void cambiarJugador() {
        jugadorActual = (jugadorActual == 'X') ? 'O' : 'X';
    }

    void limpiarEntrada() {
        cin.clear();
        cin.ignore(numeric_limits<streamsize>::max(), '\n');
    }

public:
    TresEnRaya() : tablero(3, vector<char>(3, ' ')), jugadorActual('X') {}

    void jugar() {
        while (true) {
            tablero = vector<vector<char>>(3, vector<char>(3, ' '));
            jugadorActual = 'X';
            bool terminado = false;
            while (!terminado) {
                renderizarTablero();
                int fila, col;
                cout << "Turno de " << jugadorActual << ". Ingresa fila y columna (0-2): ";
                if (!(cin >> fila >> col)) {
                    cout << "Entrada inválida. Intenta de nuevo.\n";
                    limpiarEntrada();
                    continue;
                }
                if (!movimientoValido(fila, col)) {
                    cout << "Movimiento inválido. Intenta de nuevo.\n";
                    continue;
                }
                tablero[fila][col] = jugadorActual;
                if (hayGanador()) {
                    renderizarTablero();
                    cout << "¡Gana " << jugadorActual << "!\n";
                    terminado = true;
                } else if (tableroLleno()) {
                    renderizarTablero();
                    cout << "Empate.\n";
                    terminado = true;
                } else {
                    cambiarJugador();
                }
            }
            cout << "¿Jugar de nuevo? (s/n): ";
            char resp;
            cin >> resp;
            if (resp != 's' && resp != 'S') break;
        }
    }
};

int main() {
    TresEnRaya juego;
    juego.jugar();
    return 0;
}
