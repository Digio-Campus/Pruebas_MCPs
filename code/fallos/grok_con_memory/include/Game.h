#ifndef GAME_H
#define GAME_H

#include "Board.h"
#include <vector>

// Clase que maneja el juego general con múltiples tableros
class Game {
private:
    std::vector<Board> boards;
    int numPlayers; // 0, 1, 2
    int numBoards; // Número de tableros
    int currentBoard; // Tablero actual seleccionado

public:
    Game(int players = 1, int boards = 1);
    void setSettings(int players, int boards);
    void playTurn(int boardIdx, int row, int col); // Juega en un tablero específico
    void autoPlayAll(); // Para modo 0 jugadores
    void resetBoard(int idx);
    Board& getBoard(int idx) { return boards[idx]; }
    int getNumBoards() const { return numBoards; }
    int getNumPlayers() const { return numPlayers; }
    int getCurrentBoard() const { return currentBoard; }
    void setCurrentBoard(int idx) { if (idx >= 0 && idx < numBoards) currentBoard = idx; }
};

#endif // GAME_H