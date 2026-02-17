#include "../include/Board.h"
#include <cassert>
#include <iostream>

int main() {
    std::cout << "Running Board tests..." << std::endl;

    ttt::Board board;

    // Test initial state
    assert(board.getCurrentTurn() == ttt::Cell::X);
    assert(board.getResult() == ttt::Result::Ongoing);
    assert(board.getCell(0, 0) == ttt::Cell::Empty);

    // Test valid move
    assert(board.makeMove(0, 0) == true);
    assert(board.getCell(0, 0) == ttt::Cell::X);
    assert(board.getCurrentTurn() == ttt::Cell::O);

    // Test invalid move
    assert(board.makeMove(0, 0) == false); // Already occupied

    // Test win
    board.makeMove(1, 1); // O
    board.makeMove(0, 1); // X
    board.makeMove(2, 2); // O
    board.makeMove(0, 2); // X wins
    assert(board.getResult() == ttt::Result::X_Win);

    // Test stats
    auto stats = board.getStats();
    assert(stats.xWins == 1);

    // Test reset
    board.reset();
    assert(board.getResult() == ttt::Result::Ongoing);
    assert(board.getCell(0, 0) == ttt::Cell::Empty);
    assert(board.getCurrentTurn() == ttt::Cell::X);

    std::cout << "All tests passed!" << std::endl;
    return 0;
}