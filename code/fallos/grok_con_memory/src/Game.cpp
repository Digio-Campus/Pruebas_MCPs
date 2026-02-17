#include "Game.h"

// Constructor
Game::Game(int players, int boards) : numPlayers(players), numBoards(boards), currentBoard(0) {
    this->boards.resize(numBoards);
}

// Configura ajustes
void Game::setSettings(int players, int boards) {
    numPlayers = players;
    numBoards = boards;
    this->boards.resize(numBoards);
    currentBoard = 0;
}

// Juega un turno en un tablero
void Game::playTurn(int boardIdx, int row, int col) {
    if (boardIdx < 0 || boardIdx >= numBoards) return;
    Board& board = boards[boardIdx];
    if (numPlayers == 0) {
        // Modo auto
        board.autoMove();
    } else if (numPlayers == 1) {
        // Manual para ambos
        board.makeMove(row, col);
    } else if (numPlayers == 2) {
        // Jugador O, auto X
        if (board.getCurrentPlayer() == 'O') {
            board.makeMove(row, col);
        } else {
            board.autoMove();
        }
    }
    // Verificar ganador
    char winner = board.checkWinner();
    if (winner == 'X') board.incrementWinsX();
    else if (winner == 'O') board.incrementWinsO();
    else if (winner == 'D') board.incrementDraws();
}

// Auto play para todos los tableros en modo 0
void Game::autoPlayAll() {
    for (auto& board : boards) {
        if (board.checkWinner() == ' ') {
            board.autoMove();
            char winner = board.checkWinner();
            if (winner == 'X') board.incrementWinsX();
            else if (winner == 'O') board.incrementWinsO();
            else if (winner == 'D') board.incrementDraws();
        }
    }
}

// Reinicia un tablero
void Game::resetBoard(int idx) {
    if (idx >= 0 && idx < numBoards) {
        boards[idx].reset();
    }
}