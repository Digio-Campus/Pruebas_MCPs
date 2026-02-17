#ifndef SETTINGS_H
#define SETTINGS_H

/**
 * @class Settings
 * @brief Gestiona la configuración del juego Tres en Raya
 *
 * Permite configurar el número de jugadores y el número de tableros.
 */
class Settings {
private:
    int numPlayers;     ///< Número de jugadores (0, 1, 2)
    int numBoards;      ///< Número de tableros simultáneos (1-9)

public:
    /**
     * @brief Constructor por defecto
     */
    Settings();

    /**
     * @brief Establece el número de jugadores
     * @param players Número de jugadores (0-2)
     */
    void setNumPlayers(int players);

    /**
     * @brief Establece el número de tableros
     * @param boards Número de tableros (1-9)
     */
    void setNumBoards(int boards);

    /**
     * @brief Obtiene el número de jugadores
     * @return Número de jugadores
     */
    int getNumPlayers() const;

    /**
     * @brief Obtiene el número de tableros
     * @return Número de tableros
     */
    int getNumBoards() const;

    /**
     * @brief Valida si la configuración es correcta
     * @return true si es válida, false en caso contrario
     */
    bool isValid() const;
};

#endif // SETTINGS_H