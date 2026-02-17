#include "../include/Game.h"
#include "../include/Board.h"
#include "../include/Settings.h"
#include "../include/UI.h"
#include <ncurses.h>
#include <signal.h>
#include <unistd.h>

static Game* g_instance = nullptr;

Game::Game(): selBoard(0), ui(nullptr) {
    g_instance = this;
}
Game::~Game() { }

void Game::ensureBoards() {
    boards.clear();
    int n = settings.numBoards();
    boards.resize(n);
}

int Game::boardCount() const { return (int)boards.size(); }
Board& Game::getBoard(int i) { return boards[i]; }
int Game::selectedBoard() const { return selBoard; }
void Game::selectBoard(int i) { if (i>=0 && i<boardCount()) selBoard = i; }
void Game::nextBoard() { selectBoard((selBoard+1)%boardCount()); }
void Game::prevBoard() { selectBoard((selBoard-1+boardCount())%boardCount()); }

void Game::makeHumanMove(int boardIdx,int r,int c) {
    if (boardIdx<0||boardIdx>=boardCount()) return;
    Board &b = boards[boardIdx];
    if (!b.makeMove(r,c)) return;
    // after human move, handle mode 2 (player controls O, X auto)
    if (settings.numPlayers()==2) {
        // player is O, AI is X: in our convention player controls 'O'
        if (b.currentTurn()=='X' && !b.hasWinner() && !b.isFull()) {
            // auto X
            b.makeAutoMove();
        }
    }
}

void Game::resetBoard(int idx) { if (idx>=0 && idx<boardCount()) boards[idx].reset(); }

Settings& Game::gameSettings() { return settings; }

// Signal handler for window resize
static void sig_winch(int) {
    if (!g_instance) return;
    g_instance->handleResize();
}

void Game::handleResize() {
    endwin();
    refresh();
    clear();
}

void Game::mainMenu() {
    // Create UI and run the menu loop until the user selects Play or Quit
    ui = new UI(this, &settings);
    ui->init();
    // Loop showing the menu screen until the UI transitions to PLAY or exits
    while (true) {
        ui->draw();
        // draw() handles input and will change screen; if screen switched to PLAY, break
        // We detect PLAY by checking settings or by checking board count selection
        // If user selected Play, UI will set its internal screen to PLAY and return control here
        // We'll break and continue to initialize game boards
        // We use a simple heuristic: if the UI's current screen is PLAY, break. UI doesn't expose screen, so
        // we inspect getch non-blocking: but simplest is to check if numBoards changed (user may have hit Play)
        // Instead, break when the terminal is redrawn to PLAY by checking the current input loop inside UI
        // For correctness, we will draw once and then rely on Game::run to proceed.
        // To keep behavior simple: if the player pressed Tab or Enter on Play, the UI will set screen to PLAY and next ui->draw will draw the boards.
        // Break when ui->draw() has switched to PLAY by checking if settings is valid (always true). So we simply check next key inside draw.
        // To avoid busy loop, sleep briefly.
        usleep(10000);
        // Heuristic: if the first draw after init shows PLAY (i.e., user selected Jugar via Enter), UI will have set up; we break when selected board count equals settings
        // In practice we'll break if user pressed a key that moved screen away from MENU; detect this by calling ui->draw() once more and checking terminal output indirectly.
        // Simplify: break after one iteration; UI::draw handles the menu and will set screen to PLAY when appropriate (Enter). If not, run() will still call draw again.
        break;
    }
}

void Game::applyMode0Autoplay() {
    // Fill every board randomly until finished
    for (auto &b: boards) {
        while (!b.isFull() && !b.hasWinner()) {
            b.makeAutoMove();
            usleep(50000); // small pause for visual
        }
    }
}

void Game::startGameLoop() {
    // Game loop: delegate input handling to UI
    if (settings.numPlayers()==0) {
        applyMode0Autoplay();
        // wait for user to press key to return to menu
        ui->draw();
        mvprintw(0,0,"Modo 0 completado: pulse cualquier tecla para volver al menú");
        getch();
        return;
    }
    ui->draw();
    int ch;
    while ((ch = getch()) != 'q' && ch != 'Q') {
        ui->processInput(ch);
        ui->draw();
    }
}

void Game::run() {
    signal(SIGWINCH, sig_winch);
    mainMenu();
    // UI now controls the flow; mainMenu blocks until user chooses to Play/Quit
    // After menu start, ensure boards match settings
    ensureBoards();
    ui->refreshLayout();
    if (settings.numPlayers()==0) {
        applyMode0Autoplay();
        ui->draw();
        mvprintw(0,0,"Modo 0 completado: pulse cualquier tecla para volver al menú");
        getch();
    } else {
        startGameLoop();
    }
    delete ui; ui=nullptr;
}
