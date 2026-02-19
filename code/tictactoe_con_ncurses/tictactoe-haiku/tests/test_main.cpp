#include <cassert>
#include <iostream>
#include "../include/Board.h"
#include "../include/Settings.h"

using namespace ttt;

void testBoardCreation() {
    Board board;
    assert(board.getCell(0, 0) == Cell::Empty);
    assert(board.getCurrentTurn() == Cell::X);
    assert(board.getResult() == Result::Ongoing);
    std::cout << "✓ testBoardCreation passed\n";
}

void testBoardMoveX() {
    Board board;
    assert(board.makeMove(0, 0, Cell::X));
    assert(board.getCell(0, 0) == Cell::X);
    assert(board.getCurrentTurn() == Cell::O);
    std::cout << "✓ testBoardMoveX passed\n";
}

void testBoardMoveInvalidCell() {
    Board board;
    assert(board.makeMove(0, 0, Cell::X));
    assert(!board.makeMove(0, 0, Cell::O));  // Already occupied
    std::cout << "✓ testBoardMoveInvalidCell passed\n";
}

void testBoardWinRow() {
    Board board;
    assert(board.makeMove(0, 0, Cell::X));
    assert(board.makeMove(1, 0, Cell::O));
    assert(board.makeMove(0, 1, Cell::X));
    assert(board.makeMove(1, 1, Cell::O));
    assert(board.makeMove(0, 2, Cell::X));
    assert(board.getResult() == Result::X_Win);
    std::cout << "✓ testBoardWinRow passed\n";
}

void testBoardWinDiagonal() {
    Board board;
    assert(board.makeMove(0, 0, Cell::X));
    assert(board.makeMove(0, 1, Cell::O));
    assert(board.makeMove(1, 1, Cell::X));
    assert(board.makeMove(0, 2, Cell::O));
    assert(board.makeMove(2, 2, Cell::X));
    assert(board.getResult() == Result::X_Win);
    std::cout << "✓ testBoardWinDiagonal passed\n";
}

void testBoardDraw() {
    Board board;
    board.makeMove(0, 0, Cell::X);  // X
    board.makeMove(0, 1, Cell::O);  // O
    board.makeMove(0, 2, Cell::X);  // X
    board.makeMove(1, 1, Cell::O);  // O
    board.makeMove(1, 0, Cell::X);  // X
    board.makeMove(2, 2, Cell::O);  // O
    board.makeMove(1, 2, Cell::X);  // X
    board.makeMove(2, 0, Cell::O);  // O
    board.makeMove(2, 1, Cell::X);  // X
    assert(board.getResult() == Result::Draw);
    std::cout << "✓ testBoardDraw passed\n";
}

void testBoardReset() {
    Board board;
    board.makeMove(0, 0, Cell::X);
    board.reset();
    assert(board.getCell(0, 0) == Cell::Empty);
    assert(board.getCurrentTurn() == Cell::X);
    assert(board.getResult() == Result::Ongoing);
    std::cout << "✓ testBoardReset passed\n";
}

void testBoardAvailableMoves() {
    Board board;
    board.makeMove(0, 0, Cell::X);
    auto moves = board.availableMoves();
    assert(moves.size() == 8);
    std::cout << "✓ testBoardAvailableMoves passed\n";
}

void testBoardStats() {
    Board board;
    board.makeMove(0, 0, Cell::X);
    board.makeMove(0, 1, Cell::O);
    board.makeMove(0, 2, Cell::X);
    board.makeMove(1, 1, Cell::O);
    board.makeMove(1, 0, Cell::X);
    board.makeMove(1, 2, Cell::O);
    board.makeMove(2, 0, Cell::X);
    // X wins on diagonal (0,0), (1,1), (2,0)
    
    auto stats = board.getStats();
    assert(stats.xWins == 1);  // X has won
    assert(stats.oWins == 0);
    assert(stats.draws == 0);
    std::cout << "✓ testBoardStats passed\n";
}

void testSettingsCreation() {
    Settings settings;
    assert(settings.getNumPlayers() == 2);
    assert(settings.getNumBoards() == 1);
    assert(settings.isValid());
    std::cout << "✓ testSettingsCreation passed\n";
}

void testSettingsSetPlayers() {
    Settings settings;
    settings.setNumPlayers(0);
    assert(settings.getNumPlayers() == 0);
    settings.setNumPlayers(1);
    assert(settings.getNumPlayers() == 1);
    settings.setNumPlayers(2);
    assert(settings.getNumPlayers() == 2);
    std::cout << "✓ testSettingsSetPlayers passed\n";
}

void testSettingsSetBoards() {
    Settings settings;
    for (int i = 1; i <= 9; ++i) {
        settings.setNumBoards(i);
        assert(settings.getNumBoards() == i);
    }
    std::cout << "✓ testSettingsSetBoards passed\n";
}

void testSettingsValidation() {
    Settings settings;
    settings.setNumPlayers(3);  // Invalid
    assert(settings.getNumPlayers() == 2);  // Should not change
    settings.setNumBoards(10);  // Invalid
    assert(settings.getNumBoards() == 1);  // Should not change
    std::cout << "✓ testSettingsValidation passed\n";
}

int main() {
    std::cout << "Running unit tests...\n\n";
    
    testBoardCreation();
    testBoardMoveX();
    testBoardMoveInvalidCell();
    testBoardWinRow();
    testBoardWinDiagonal();
    testBoardDraw();
    testBoardReset();
    testBoardAvailableMoves();
    testBoardStats();
    testSettingsCreation();
    testSettingsSetPlayers();
    testSettingsSetBoards();
    testSettingsValidation();
    
    std::cout << "\n✓ All tests passed!\n";
    return 0;
}
