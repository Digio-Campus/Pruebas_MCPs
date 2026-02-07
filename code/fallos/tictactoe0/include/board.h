#ifndef BOARD_H
#define BOARD_H

#include <vector>

// Representa un tablero de Tictactoe 3x3
class Board {
private:
    std::vector<std::vector<char>> grid; // Matriz 3x3
    char winner;                          // 'X', 'O', 'D' (empate), o ' '
    int moves_count;                      // Contador de movimientos

public:
    Board();
    
    // Realiza un movimiento en la posición (row, col) con el símbolo dado
    bool makeMove(int row, int col, char symbol);
    
    // Obtiene el símbolo en una posición
    char getCell(int row, int col) const;
    
    // Verifica si hay un ganador y actualiza el estado
    bool checkWin();
    
    // Verifica si el tablero está lleno (empate)
    bool checkDraw();
    
    // Obtiene el ganador actual
    char getWinner() const;
    
    // Verifica si el juego ha terminado
    bool isGameOver() const;
    
    // Reinicia el tablero
    void reset();
    
    // Obtiene el número de movimientos realizados
    int getMovesCount() const;
};

#endif
