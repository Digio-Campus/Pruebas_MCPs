#include "game.h"
#include <cstdlib>
#include <ctime>
#include <algorithm>

// Constructor
Game::Game() : numBoards(0), gameMode(MODE_AUTO), isRunning(false) {
    srand(time(nullptr));
}

// Destructor
Game::~Game() {
    for (auto board : boards) {
        delete board;
    }
    boards.clear();
}

// Inicializa el juego
void Game::initGame(int numBoards, GameMode mode) {
    // Limpiar tableros antiguos
    for (auto board : boards) {
        delete board;
    }
    boards.clear();
    
    // Crear nuevos tableros
    this->numBoards = numBoards;
    this->gameMode = mode;
    
    for (int i = 0; i < numBoards; i++) {
        boards.push_back(new Board());
    }
    
    isRunning = true;
}

// Resetea el juego
void Game::reset() {
    for (auto board : boards) {
        if (board) {
            board->reset();
        }
    }
}

// Obtiene un tablero
Board* Game::getBoard(int index) {
    if (index < 0 || index >= (int)boards.size()) {
        return nullptr;
    }
    return boards[index];
}

// Obtiene el número de tableros
int Game::getNumBoards() const {
    return numBoards;
}

// Realiza un movimiento
bool Game::makeMove(int boardIndex, int row, int col) {
    if (boardIndex < 0 || boardIndex >= (int)boards.size()) {
        return false;
    }
    
    Board* board = boards[boardIndex];
    if (!board) return false;
    
    // Si el juego ya terminó en este tablero, no hacer nada
    if (board->isGameOver()) {
        return false;
    }
    
    // Intenta hacer el movimiento
    if (!board->makeMove(row, col)) {
        return false;
    }
    
    // Si es modo automático, no hacer movimiento automático aquí
    // (Se maneja en el bucle principal)
    
    // Si es modo IA y es turno de O (jugador), hacer movimiento de X (IA)
    if (gameMode == MODE_AI && board->getCurrentTurn() == PLAYER_X && !board->isGameOver()) {
        makeAIMove(boardIndex);
    }
    
    return true;
}

// Realiza un movimiento de IA
bool Game::makeAIMove(int boardIndex) {
    if (boardIndex < 0 || boardIndex >= (int)boards.size()) {
        return false;
    }
    
    Board* board = boards[boardIndex];
    if (!board || board->isGameOver()) {
        return false;
    }
    
    int move = getAIMove(boardIndex);
    if (move < 0) return false;
    
    int row = move / 3;
    int col = move % 3;
    
    return board->makeMove(row, col);
}

// Obtiene un movimiento de IA (estrategia simple)
int Game::getAIMove(int boardIndex) {
    if (boardIndex < 0 || boardIndex >= (int)boards.size()) {
        return -1;
    }
    
    Board* board = boards[boardIndex];
    if (!board) return -1;
    
    std::vector<std::pair<int, int>> available = board->getAvailableMoves();
    
    if (available.empty()) {
        return -1;
    }
    
    // Estrategia: 
    // 1. Intentar ganar
    // 2. Bloquear al jugador
    // 3. Tomar el centro
    // 4. Tomar una esquina
    // 5. Tomar un lado aleatorio
    
    // Para simplificar, usar movimiento aleatorio
    int randomIndex = rand() % available.size();
    return available[randomIndex].first * 3 + available[randomIndex].second;
}

// Resetea un tablero específico
void Game::resetBoard(int boardIndex) {
    if (boardIndex >= 0 && boardIndex < (int)boards.size()) {
        boards[boardIndex]->reset();
    }
}

// Getters
GameMode Game::getGameMode() const {
    return gameMode;
}

bool Game::getIsRunning() const {
    return isRunning;
}

void Game::setIsRunning(bool running) {
    isRunning = running;
}

// Estadísticas totales
int Game::getTotalXWins() const {
    int total = 0;
    for (auto board : boards) {
        if (board) total += board->getXWins();
    }
    return total;
}

int Game::getTotalOWins() const {
    int total = 0;
    for (auto board : boards) {
        if (board) total += board->getOWins();
    }
    return total;
}

int Game::getTotalDraws() const {
    int total = 0;
    for (auto board : boards) {
        if (board) total += board->getDraws();
    }
    return total;
}
