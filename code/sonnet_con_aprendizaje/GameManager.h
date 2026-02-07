#ifndef GAMEMANAGER_H
#define GAMEMANAGER_H

#include "Board.h"
#include <vector>

// Modos de juego
enum class GameMode {
    ZERO_PLAYERS,   // Todos los tableros automáticos
    ONE_PLAYER,     // Un jugador controla X y O manualmente
    TWO_PLAYERS     // Jugador controla O, X automática
};

// Estructura para estadísticas de un tablero
struct BoardStats {
    int xWins;
    int oWins;
    int draws;
    int gamesPlayed;
    
    BoardStats() : xWins(0), oWins(0), draws(0), gamesPlayed(0) {}
};

// Gestor del juego que maneja múltiples tableros
class GameManager {
private:
    std::vector<Board> boards;
    std::vector<BoardStats> stats;
    GameMode mode;
    int numBoards;
    int selectedBoard;
    int selectedRow;
    int selectedCol;

public:
    GameManager();
    
    // Configura el número de tableros
    void setNumBoards(int num);
    
    // Configura el modo de juego
    void setGameMode(GameMode m);
    
    // Obtiene el número de tableros
    int getNumBoards() const;
    
    // Obtiene el modo de juego
    GameMode getGameMode() const;
    
    // Obtiene un tablero específico
    const Board& getBoard(int index) const;
    
    // Obtiene las estadísticas de un tablero
    const BoardStats& getStats(int index) const;
    
    // Realiza un movimiento en el tablero seleccionado
    bool makeMove();
    
    // Actualiza todos los tableros automáticos
    void updateAutoBoards();
    
    // Selecciona un tablero
    void selectBoard(int index);
    
    // Selecciona una celda dentro del tablero
    void selectCell(int row, int col);
    
    // Obtiene el índice del tablero seleccionado
    int getSelectedBoard() const;
    
    // Obtiene la fila seleccionada
    int getSelectedRow() const;
    
    // Obtiene la columna seleccionada
    int getSelectedCol() const;
    
    // Mueve la selección
    void moveSelection(int dr, int dc);
    
    // Reinicia un tablero específico
    void resetBoard(int index);
    
    // Reinicia todos los tableros
    void resetAll();
    
private:
    // Actualiza estadísticas después de un juego
    void updateStats(int boardIndex);
};

#endif
