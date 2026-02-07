#ifndef BOARD_H
#define BOARD_H

#include <vector>
#include <string>

// Enumeración para los estados de las casillas
enum CellState {
    EMPTY = 0,
    X = 1,
    O = 2
};

// Enumeración para el resultado del juego
enum GameResult {
    ONGOING,
    X_WINS,
    O_WINS,
    DRAW
};

// Clase que representa un tablero individual de TicTacToe
class Board {
private:
    std::vector<std::vector<CellState>> grid;  // Tablero 3x3
    CellState currentTurn;                      // Turno actual (X u O)
    GameResult result;                          // Resultado del juego
    int movesCount;                             // Número de movimientos realizados
    int boardId;                                // ID único del tablero
    
    // Estadísticas individuales
    int xWins;
    int oWins;
    int draws;
    
    // Verifica si hay victoria
    bool checkWin(CellState player);
    
    // Verifica si hay empate
    bool checkDraw();

public:
    Board(int id);
    
    // Gestión del tablero
    bool makeMove(int row, int col);            // Realiza un movimiento
    bool makeAutoMove();                        // Movimiento automático aleatorio
    void reset();                               // Reinicia el tablero
    
    // Getters
    CellState getCell(int row, int col) const;
    CellState getCurrentTurn() const;
    GameResult getResult() const;
    int getMovesCount() const;
    int getBoardId() const;
    int getXWins() const;
    int getOWins() const;
    int getDraws() const;
    
    // Validación
    bool isValidMove(int row, int col) const;
    bool isGameOver() const;
    
    // Utilidades
    std::string getCellSymbol(int row, int col) const;
    std::string getCurrentTurnSymbol() const;
};

#endif
