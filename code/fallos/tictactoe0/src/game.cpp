#include "../include/game.h"
#include <thread>
#include <chrono>

Game::Game() : current_player(0), score_x(0), score_o(0), draws(0), running(false) {
    config.num_players = 1;
    config.num_boards = 1;
}

void Game::configure(const GameConfig& cfg) {
    config = cfg;
    
    // Configurar jugadores según el número de jugadores humanos
    if (config.num_players == 0) {
        playerX = std::make_unique<Player>('X', AUTO);
        playerO = std::make_unique<Player>('O', AUTO);
    } else if (config.num_players == 1) {
        playerX = std::make_unique<Player>('X', HUMAN);
        playerO = std::make_unique<Player>('O', AUTO);
    } else { // 2 jugadores
        playerX = std::make_unique<Player>('X', AUTO);
        playerO = std::make_unique<Player>('O', HUMAN);
    }
}

void Game::start() {
    boards.clear();
    boards.resize(config.num_boards);
    current_player = 0;
    running = true;
}

bool Game::processMove(int board_idx, int row, int col) {
    if (board_idx < 0 || board_idx >= static_cast<int>(boards.size())) return false;
    if (boards[board_idx].isGameOver()) return false;
    
    char symbol = (current_player == 0) ? 'X' : 'O';
    
    if (boards[board_idx].makeMove(row, col, symbol)) {
        // Verificar si hay ganador
        if (boards[board_idx].getWinner() == 'X') {
            score_x++;
        } else if (boards[board_idx].getWinner() == 'O') {
            score_o++;
        } else if (boards[board_idx].getWinner() == 'D') {
            draws++;
        }
        
        // Cambiar turno
        current_player = 1 - current_player;
        return true;
    }
    
    return false;
}

void Game::processAutoMoves() {
    // Procesar movimientos automáticos
    Player* current = (current_player == 0) ? playerX.get() : playerO.get();
    
    if (current->getType() == AUTO) {
        // Realizar movimientos en todos los tableros no terminados
        for (size_t i = 0; i < boards.size(); i++) {
            if (!boards[i].isGameOver()) {
                auto move = current->getAutoMove(boards[i]);
                if (move.first != -1) {
                    processMove(i, move.first, move.second);
                }
            }
        }
    }
}

void Game::update() {
    // En modo automático (0 jugadores), reiniciar tableros terminados
    if (config.num_players == 0) {
        bool all_finished = true;
        for (auto& board : boards) {
            if (!board.isGameOver()) {
                all_finished = false;
                break;
            }
        }
        
        if (all_finished) {
            std::this_thread::sleep_for(std::chrono::milliseconds(500));
            for (auto& board : boards) {
                board.reset();
            }
        }
    }
}

int Game::getNumBoards() const {
    return boards.size();
}

const Board& Game::getBoard(int idx) const {
    return boards[idx];
}

char Game::getCurrentPlayer() const {
    return (current_player == 0) ? 'X' : 'O';
}

void Game::getScores(int& x, int& o, int& d) const {
    x = score_x;
    o = score_o;
    d = draws;
}

bool Game::isRunning() const {
    return running;
}

void Game::stop() {
    running = false;
}

const GameConfig& Game::getConfig() const {
    return config;
}
