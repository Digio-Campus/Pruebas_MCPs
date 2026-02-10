#ifndef BOARD_H
#define BOARD_H

#include <vector>
#include <cstring>

// Enumeración para los estados de las casillas
enum CellState {
    EMPTY = 0,
    PLAYER_X = 1,
    PLAYER_O = 2
};

// Estructura para representar una casilla
struct Cell {
    CellState state;
    int row;
    int col;
};

// Clase para gestionar un tablero individual
class Board {
private:
    CellState grid[3][3];
    CellState currentTurn;      // Jugador actual (X o O)
    CellState winner;           // Ganador (EMPTY si no hay, X, O)
    bool isFull;               // ¿Está el tablero lleno?
    int moveCount;             // Número de movimientos realizados
    int xWins;                 // Victorias de X
    int oWins;                 // Victorias de O
    int draws;                 // Empates
    
public:
    Board();
    ~Board();
    
    // Métodos de juego
    bool makeMove(int row, int col);
    void reset();
    CellState checkWinner();
    bool isGameOver();
    bool isCellEmpty(int row, int col) const;
    
    // Getters
    CellState getCell(int row, int col) const;
    CellState getCurrentTurn() const;
    CellState getWinner() const;
    bool getIsFull() const;
    int getMoveCount() const;
    int getXWins() const;
    int getOWins() const;
    int getDraws() const;
    
    // Setters
    void setCurrentTurn(CellState turn);
    void incrementXWins();
    void incrementOWins();
    void incrementDraws();
    
    // Utilidades
    void print() const;
    std::vector<std::pair<int, int>> getAvailableMoves() const;
};

#endif // BOARD_H
