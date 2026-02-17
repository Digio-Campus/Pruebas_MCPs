#include "../include/Game.h"
#include <chrono>
#include <thread>
#include <stdexcept>

namespace ttt {

Game::Game(int numBoards, Mode mode) : mode_(mode), rng_(std::random_device{}()) {
    setNumBoards(std::max(1, numBoards));
}

void Game::setNumBoards(int n) {
    if (n < 1) n = 1;
    boards_.clear();
    stats_.clear();
    lastRecordedResult_.clear();
    for (int i = 0; i < n; ++i) {
        boards_.emplace_back(i);
        stats_.emplace_back();
        lastRecordedResult_.push_back(Result::Ongoing);
    }
}

void Game::resetAll() {
    for (size_t i = 0; i < boards_.size(); ++i) {
        boards_[i].reset();
        lastRecordedResult_[i] = Result::Ongoing;
        stats_[i] = BoardStats();
    }
}

void Game::resetBoard(int idx) {
    if (idx < 0 || idx >= (int)boards_.size()) return;
    boards_[idx].reset();
    lastRecordedResult_[idx] = Result::Ongoing;
    stats_[idx] = BoardStats();
}

int Game::boardCount() const { return (int)boards_.size(); }
Board& Game::getBoard(int idx) { return boards_.at(idx); }
const Board& Game::getBoard(int idx) const { return boards_.at(idx); }

void Game::setMode(Mode m) { mode_ = m; }
Mode Game::mode() const { return mode_; }
int Game::numBoards() const { return (int)boards_.size(); }

const std::vector<BoardStats>& Game::stats() const { return stats_; }

bool Game::updateAutoPlayStep(int delayMs) {
    bool moved = false;
    if (mode_ == Mode::Auto) {
        for (size_t i = 0; i < boards_.size(); ++i) {
            Board &b = boards_[i];
            if (b.result() == Result::Ongoing) {
                auto avail = b.availableMoves();
                if (!avail.empty()) {
                    std::uniform_int_distribution<int> dist(0, (int)avail.size() - 1);
                    auto mv = avail[dist(rng_)];
                    b.makeMove(mv.first, mv.second);
                    moved = true;
                    maybeRecordResult((int)i);
                }
            }
        }
    } else if (mode_ == Mode::PlayerO_AutoX) {
        // generar X automáticas en los tableros cuyo turno sea X
        for (size_t i = 0; i < boards_.size(); ++i) {
            Board &b = boards_[i];
            if (b.result() == Result::Ongoing && b.currentTurn() == Cell::X) {
                auto avail = b.availableMoves();
                if (!avail.empty()) {
                    std::uniform_int_distribution<int> dist(0, (int)avail.size() - 1);
                    auto mv = avail[dist(rng_)];
                    b.makeMove(mv.first, mv.second);
                    moved = true;
                    maybeRecordResult((int)i);
                }
            }
        }
    }
    if (moved && delayMs > 0) std::this_thread::sleep_for(std::chrono::milliseconds(delayMs));
    return moved;
}

void Game::playerMove(int boardIdx, int row, int col) {
    if (boardIdx < 0 || boardIdx >= (int)boards_.size()) return;
    Board &b = boards_[boardIdx];
    if (b.result() != Result::Ongoing) return;

    if (mode_ == Mode::Auto) return; // sin interacción humana en modo auto

    if (mode_ == Mode::ManualBoth) {
        if (b.makeMove(row, col)) maybeRecordResult(boardIdx);
        return;
    }

    if (mode_ == Mode::PlayerO_AutoX) {
        // si es turno de X, generar X automático antes de permitir la jugada humana
        if (b.currentTurn() == Cell::X) {
            updateAutoPlayStep(0);
            if (b.result() != Result::Ongoing) return;
        }
        // ahora debe ser turno de O (jugador)
        if (b.currentTurn() == Cell::O) {
            if (!b.makeMove(row, col)) return;
            maybeRecordResult(boardIdx);
            // después de O, generar X automáticamente en el mismo tablero (si sigue activo)
            if (b.result() == Result::Ongoing && b.currentTurn() == Cell::X) {
                auto avail = b.availableMoves();
                if (!avail.empty()) {
                    std::uniform_int_distribution<int> dist(0, (int)avail.size() - 1);
                    auto mv = avail[dist(rng_)];
                    b.makeMove(mv.first, mv.second);
                    maybeRecordResult(boardIdx);
                }
            }
        }
    }
}

void Game::maybeRecordResult(int idx) {
    if (idx < 0 || idx >= (int)boards_.size()) return;
    const Board &b = boards_[idx];
    Result r = b.result();
    if (r == lastRecordedResult_[idx]) return; // ya registrado
    if (r == Result::X_Win) stats_[idx].xWins++;
    else if (r == Result::O_Win) stats_[idx].oWins++;
    else if (r == Result::Draw) stats_[idx].draws++;
    lastRecordedResult_[idx] = r;
}

} // namespace ttt
