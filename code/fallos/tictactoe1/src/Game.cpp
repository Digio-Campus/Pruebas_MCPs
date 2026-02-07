#include "../include/Game.h"
#include <unistd.h>  // Para usleep

Game::Game(UI* userInterface, Settings* gameSettings) 
    : ui(userInterface), settings(gameSettings), 
      selectedBoard(0), cursorRow(1), cursorCol(1), running(false) {
}

Game::~Game() {
    clearBoards();
}

void Game::createBoards() {
    clearBoards();
    int numBoards = settings->getNumBoards();
    for (int i = 0; i < numBoards; i++) {
        boards.push_back(new Board(i + 1));
    }
}

void Game::clearBoards() {
    for (auto board : boards) {
        delete board;
    }
    boards.clear();
}

void Game::run() {
    createBoards();
    running = true;
    selectedBoard = 0;
    cursorRow = 1;
    cursorCol = 1;
    
    int numPlayers = settings->getNumPlayers();
    
    // Modo 0 jugadores: automático completo
    if (numPlayers == 0) {
        handleMode0();
        return;
    }
    
    // Modos 1 y 2 jugadores: interactivo
    while (running) {
        // Dibujar tableros
        ui->drawBoards(boards, selectedBoard, cursorRow, cursorCol, *settings);
        
        // Obtener input
        int ch = ui->getInput();
        
        // Manejar input
        if (ch == KEY_MOUSE) {
            int y, x, button;
            if (ui->getMouseEvent(y, x, button)) {
                handleMouseInput(y, x, button);
            }
        } else {
            handleInput(ch);
        }
    }
}

void Game::handleMode0() {
    // Modo automático: llenar todos los tableros aleatoriamente
    ui->drawBoards(boards, selectedBoard, cursorRow, cursorCol, *settings);
    usleep(500000);  // Esperar 0.5 segundos antes de comenzar
    
    bool allFinished = false;
    
    while (!allFinished && running) {
        allFinished = true;
        
        // Realizar un movimiento en cada tablero no terminado
        for (auto board : boards) {
            if (!board->isGameOver()) {
                board->makeAutoMove();
                allFinished = false;
            }
        }
        
        // Actualizar display
        ui->drawBoards(boards, selectedBoard, cursorRow, cursorCol, *settings);
        usleep(300000);  // Pausa de 0.3 segundos entre movimientos
        
        // Verificar si el usuario quiere salir
        nodelay(stdscr, TRUE);  // No bloquear en getch()
        int ch = ui->getInput();
        nodelay(stdscr, FALSE);
        
        if (ch == 'q' || ch == 'Q') {
            running = false;
        }
    }
    
    // Esperar a que el usuario presione una tecla
    if (running) {
        ui->showCenteredMessage("Todos los tableros terminados. Presiona cualquier tecla...", 
                               LINES - 3);
        ui->refresh();
        ui->waitForKey();
    }
}

void Game::handleInput(int ch) {
    switch (ch) {
        case KEY_UP:
            if (cursorRow > 0) cursorRow--;
            break;
            
        case KEY_DOWN:
            if (cursorRow < 2) cursorRow++;
            break;
            
        case KEY_LEFT:
            if (cursorCol > 0) cursorCol--;
            break;
            
        case KEY_RIGHT:
            if (cursorCol < 2) cursorCol++;
            break;
            
        case 10:  // Enter
        case KEY_ENTER:
            processMove();
            break;
            
        case 9:   // Tab
        case KEY_STAB:
            switchBoard(1);
            break;
            
        case 'r':
        case 'R':
            resetCurrentBoard();
            break;
            
        case 'q':
        case 'Q':
            stop();
            break;
    }
}

void Game::handleMouseInput(int y, int x, int button) {
    // TODO: Implementar conversión de coordenadas de ratón a tablero/casilla
    // Por simplicidad, esta funcionalidad está parcialmente implementada
}

void Game::processMove() {
    if (selectedBoard < 0 || selectedBoard >= (int)boards.size()) {
        return;
    }
    
    Board* board = boards[selectedBoard];
    
    if (board->isGameOver()) {
        return;
    }
    
    int numPlayers = settings->getNumPlayers();
    
    // Modo 1 jugador: manual completo
    if (numPlayers == 1) {
        handleMode1();
    }
    // Modo 2 jugadores: jugador vs auto
    else if (numPlayers == 2) {
        handleMode2();
    }
}

void Game::handleMode1() {
    // El jugador controla tanto X como O manualmente
    Board* board = boards[selectedBoard];
    
    // Realizar el movimiento manual
    if (board->makeMove(cursorRow, cursorCol)) {
        // Movimiento exitoso
        // En modo 1, el jugador debe hacer el siguiente movimiento manualmente
        // No hay acción automática
    }
}

void Game::handleMode2() {
    // El jugador controla O, X se genera automáticamente
    Board* board = boards[selectedBoard];
    
    CellState currentTurn = board->getCurrentTurn();
    
    if (currentTurn == O) {
        // Es el turno del jugador (O)
        if (board->makeMove(cursorRow, cursorCol)) {
            // Movimiento exitoso, ahora debe jugar X automáticamente
            if (!board->isGameOver()) {
                // Pausa breve antes de la jugada automática
                ui->drawBoards(boards, selectedBoard, cursorRow, cursorCol, *settings);
                usleep(300000);  // 0.3 segundos
                
                // Realizar movimiento automático de X
                board->makeAutoMove();
            }
        }
    } else {
        // Es turno de X (automático), el jugador no puede mover aún
        // Mostrar mensaje o simplemente no hacer nada
    }
}

void Game::switchBoard(int direction) {
    selectedBoard = (selectedBoard + direction + boards.size()) % boards.size();
    cursorRow = 1;
    cursorCol = 1;
}

void Game::resetCurrentBoard() {
    if (selectedBoard >= 0 && selectedBoard < (int)boards.size()) {
        boards[selectedBoard]->reset();
    }
}

void Game::stop() {
    running = false;
}
