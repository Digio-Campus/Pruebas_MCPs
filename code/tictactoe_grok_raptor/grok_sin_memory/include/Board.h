#ifndef BOARD_H
#define BOARD_H

#include <vector>

/**
 * @class Board
 * @brief Representa un tablero individual de Tres en Raya
 *
 * Gestiona el estado del tablero, turnos, movimientos y detección de victoria/empate.
 */
class Board {
private:
    std::vector<std::vector<char>> grid;  ///< Matriz 3x3 del tablero
    char currentPlayer;                   ///< Jugador actual ('X' o 'O')
    int xWins;                           ///< Contador de victorias de X
    int oWins;                           ///< Contador de victorias de O
    int draws;                           ///< Contador de empates

public:
    /**
     * @brief Constructor por defecto
     */
    Board();

    /**
     * @brief Reinicia el tablero para una nueva partida
     */
    void reset();

    /**
     * @brief Realiza un movimiento en la posición especificada
     * @param row Fila (0-2)
     * @param col Columna (0-2)
     * @return true si el movimiento fue válido, false en caso contrario
     */
    bool makeMove(int row, int col);

    /**
     * @brief Realiza un movimiento automático aleatorio
     * @return true si se realizó el movimiento, false si el tablero está lleno
     */
    bool makeAutoMove();

    /**
     * @brief Verifica si hay un ganador
     * @return 'X', 'O' si hay ganador, ' ' si no hay
     */
    char checkWin() const;

    /**
     * @brief Verifica si el tablero está en empate
     * @return true si hay empate, false en caso contrario
     */
    bool checkDraw() const;

    /**
     * @brief Obtiene el estado de una celda
     * @param row Fila
     * @param col Columna
     * @return Carácter en la celda ('X', 'O', o ' ')
     */
    char getCell(int row, int col) const;

    /**
     * @brief Obtiene el jugador actual
     * @return 'X' o 'O'
     */
    char getCurrentPlayer() const;

    /**
     * @brief Obtiene las estadísticas de victorias
     * @param xWins Referencia para almacenar victorias de X
     * @param oWins Referencia para almacenar victorias de O
     * @param draws Referencia para almacenar empates
     */
    void getStats(int& xWins, int& oWins, int& draws) const;

    /**
     * @brief Incrementa el contador de victorias según el ganador
     * @param winner 'X' o 'O'
     */
    void incrementWin(char winner);
};

#endif // BOARD_H