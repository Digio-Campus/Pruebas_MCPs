#ifndef UI_H
#define UI_H

#include <vector>
#include "Board.h"
#include "Settings.h"

/**
 * @class UI
 * @brief Gestiona la interfaz de usuario con ncurses
 *
 * Maneja el dibujo de menús, tableros, estadísticas y entrada del usuario.
 */
class UI {
private:
    int maxY, maxX;  ///< Dimensiones de la terminal

public:
    /**
     * @brief Constructor
     */
    UI();

    /**
     * @brief Destructor
     */
    ~UI();

    /**
     * @brief Inicializa ncurses
     */
    void init();

    /**
     * @brief Finaliza ncurses
     */
    void cleanup();

    /**
     * @brief Actualiza las dimensiones de la terminal
     */
    void updateDimensions();

    /**
     * @brief Dibuja el menú principal
     * @param selectedItem Elemento seleccionado (0-3)
     */
    void drawMenu(int selectedItem);

    /**
     * @brief Dibuja el menú de ajustes
     * @param settings Configuración actual
     * @param selectedItem Elemento seleccionado
     */
    void drawSettingsMenu(const Settings& settings, int selectedItem);

    /**
     * @brief Dibuja la pantalla de ayuda
     */
    void drawHelp();

    /**
     * @brief Dibuja todos los tableros
     * @param boards Vector de tableros
     * @param selectedBoard Tablero seleccionado
     * @param cursorRow Fila del cursor
     * @param cursorCol Columna del cursor
     */
    void drawBoards(const std::vector<Board>& boards, int selectedBoard, int cursorRow, int cursorCol);

    /**
     * @brief Dibuja las estadísticas de un tablero
     * @param board Tablero
     * @param boardIndex Índice del tablero
     * @param y Posición Y
     * @param x Posición X
     */
    void drawStats(const Board& board, int boardIndex, int y, int x);

    /**
     * @brief Obtiene entrada del usuario
     * @return Código de tecla presionada
     */
    int getInput();

    /**
     * @brief Verifica si se puede usar el ratón
     * @return true si está disponible, false en caso contrario
     */
    bool hasMouse();

    /**
     * @brief Mapea coordenadas de clic a celda del tablero
     * @param mouseY Coordenada Y del clic
     * @param mouseX Coordenada X del clic
     * @param boardIndex Índice del tablero clickeado
     * @param cellRow Fila de la celda
     * @param cellCol Columna de la celda
     * @param boards Vector de tableros
     * @return true si el clic fue en una celda válida
     */
    bool mapClickToCell(int mouseY, int mouseX, int& boardIndex, int& cellRow, int& cellCol, const std::vector<Board>& boards);
};

#endif // UI_H