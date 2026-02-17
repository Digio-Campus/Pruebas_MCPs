#ifndef GAME_H
#define GAME_H

#include <vector>
#include "Board.h"
#include "Settings.h"
#include "UI.h"

/**
 * @class Game
 * @brief Controlador principal del juego
 *
 * Gestiona el flujo del juego, tableros y modos de juego.
 */
class Game {
private:
    std::vector<Board> boards;  ///< Vector de tableros
    Settings& settings;         ///< Configuración del juego
    UI& ui;                     ///< Interfaz de usuario
    int selectedBoard;          ///< Tablero actualmente seleccionado
    int cursorRow, cursorCol;   ///< Posición del cursor

public:
    /**
     * @brief Constructor
     * @param settings Referencia a configuración
     * @param ui Referencia a interfaz
     */
    Game(Settings& settings, UI& ui);

    /**
     * @brief Inicializa los tableros según la configuración
     */
    void initializeBoards();

    /**
     * @brief Ejecuta el modo de juego seleccionado
     */
    void play();

private:
    /**
     * @brief Modo 0 jugadores: movimientos automáticos
     */
    void playMode0();

    /**
     * @brief Modo 1 jugador: control manual de X y O
     */
    void playMode1();

    /**
     * @brief Modo 2 jugadores: jugador controla O, X automática
     */
    void playMode2();

    /**
     * @brief Procesa entrada del usuario en modo manual
     * @return true si se debe continuar, false para salir
     */
    bool processInput();

    /**
     * @brief Maneja clic del ratón
     * @param mouseY Coordenada Y
     * @param mouseX Coordenada X
     */
    void handleMouseClick(int mouseY, int mouseX);

    /**
     * @brief Verifica y maneja fin de partida en un tablero
     * @param boardIndex Índice del tablero
     */
    void checkGameEnd(int boardIndex);
};

#endif // GAME_H