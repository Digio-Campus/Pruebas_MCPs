#ifndef PLAYER_H
#define PLAYER_H

#include "board.h"
#include <utility>

// Tipos de jugador
enum PlayerType {
    HUMAN,
    AUTO
};

// Representa un jugador
class Player {
private:
    char symbol;        // 'X' o 'O'
    PlayerType type;    // Humano o automático

public:
    Player(char sym, PlayerType t);
    
    // Obtiene el símbolo del jugador
    char getSymbol() const;
    
    // Obtiene el tipo de jugador
    PlayerType getType() const;
    
    // Genera un movimiento automático aleatorio
    std::pair<int, int> getAutoMove(const Board& board);
};

#endif
