#ifndef GAME_H
#define GAME_H

#include "board.h"
#include <vector>
#include <memory>

// Enumeración para los modos de juego
enum GameMode {
    MODE_AUTO = 0,      // 0 jugadores - Automático
    MODE_MANUAL = 1,    // 1 jugador - Manual
    MODE_AI = 2         // 2 jugadores - Con IA
};

// Clase para gestionar el juego
class Game {
private:
    std::vector<Board*> boards;
    int numBoards;
    GameMode gameMode;
    bool isRunning;
    
public:
    Game();
    ~Game();
    
    // Inicialización
    void initGame(int numBoards, GameMode mode);
    void reset();
    
    // Gestión de tableros
    Board* getBoard(int index);
    int getNumBoards() const;
    
    // Movimientos
    bool makeMove(int boardIndex, int row, int col);
    bool makeAIMove(int boardIndex);
    int getAIMove(int boardIndex);
    
    // Utilidades
    void resetBoard(int boardIndex);
    GameMode getGameMode() const;
    bool getIsRunning() const;
    void setIsRunning(bool running);
    
    // Estadísticas
    int getTotalXWins() const;
    int getTotalOWins() const;
    int getTotalDraws() const;
};

#endif // GAME_H
