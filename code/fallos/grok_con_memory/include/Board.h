#ifndef BOARD_H
#define BOARD_H

#include <vector>
#include <string>

// Clase que representa un tablero individual de Tres en Raya
class Board {
private:
    std::vector<std::vector<char>> grid; // 3x3 grid, ' ' empty, 'X', 'O'
    char currentPlayer; // 'X' or 'O'
    int winsX, winsO, draws; // Estadísticas

public:
    Board();
    void reset(); // Reinicia el tablero
    bool makeMove(int row, int col); // Hace un movimiento si es válido
    char checkWinner(); // Devuelve 'X', 'O', 'D' (draw), o ' ' (ongoing)
    void autoMove(); // Movimiento automático aleatorio
    char getCell(int row, int col) const;
    char getCurrentPlayer() const { return currentPlayer; }
    void switchPlayer();
    int getWinsX() const { return winsX; }
    int getWinsO() const { return winsO; }
    int getDraws() const { return draws; }
    void incrementWinsX() { winsX++; }
    void incrementWinsO() { winsO++; }
    void incrementDraws() { draws++; }
};

#endif // BOARD_H