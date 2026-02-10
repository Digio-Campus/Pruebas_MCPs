#ifndef BOARD_H
#define BOARD_H

#include <vector>

// Representa el estado de una celda del tablero
enum class CellState {
    EMPTY,
    X,
    O
};

// Representa el estado del juego
enum class GameState {
    PLAYING,
    X_WINS,
    O_WINS,
    DRAW
};

// Clase que representa un tablero de Tictactoe 3x3
class Board {
private:
    std::vector<std::vector<CellState>> cells;
    GameState state;
    CellState currentTurn;
    int movesCount;

public:
    Board();
    
    // Reinicia el tablero
    void reset();
    
    // Realiza un movimiento en la posición especificada
    bool makeMove(int row, int col);
    
    // Realiza un movimiento automático aleatorio
    bool makeRandomMove();
    
    // Obtiene el estado de una celda
    CellState getCell(int row, int col) const;
    
    // Obtiene el estado del juego
    GameState getState() const;
    
    // Obtiene el turno actual
    CellState getCurrentTurn() const;
    
    // Verifica si el tablero está lleno
    bool isFull() const;
    
    // Verifica si hay un ganador
    void checkWinner();
    
private:
    // Cambia el turno
    void switchTurn();
};

#endif
