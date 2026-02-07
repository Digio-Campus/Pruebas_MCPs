#ifndef GAME_H
#define GAME_H

#include "board.h"
#include "player.h"
#include <vector>
#include <memory>

// Configuración del juego
struct GameConfig {
    int num_players;    // 0, 1, o 2 jugadores humanos
    int num_boards;     // Número de tableros simultáneos
    
    GameConfig() : num_players(1), num_boards(1) {}
};

// Gestiona la lógica del juego con múltiples tableros
class Game {
private:
    std::vector<Board> boards;
    std::unique_ptr<Player> playerX;
    std::unique_ptr<Player> playerO;
    GameConfig config;
    int current_player;  // 0 para X, 1 para O
    int score_x;
    int score_o;
    int draws;
    bool running;

public:
    Game();
    
    // Configura el juego con los parámetros dados
    void configure(const GameConfig& cfg);
    
    // Inicia una nueva sesión de juego
    void start();
    
    // Procesa un movimiento en un tablero específico
    bool processMove(int board_idx, int row, int col);
    
    // Procesa movimientos automáticos si es necesario
    void processAutoMoves();
    
    // Obtiene el número de tableros
    int getNumBoards() const;
    
    // Obtiene un tablero específico
    const Board& getBoard(int idx) const;
    
    // Obtiene el jugador actual
    char getCurrentPlayer() const;
    
    // Obtiene las puntuaciones
    void getScores(int& x, int& o, int& d) const;
    
    // Verifica si el juego está en ejecución
    bool isRunning() const;
    
    // Detiene el juego
    void stop();
    
    // Obtiene la configuración actual
    const GameConfig& getConfig() const;
    
    // Actualiza el estado del juego (reinicia tableros terminados en modo auto)
    void update();
};

#endif
