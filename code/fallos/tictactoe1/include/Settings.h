#ifndef SETTINGS_H
#define SETTINGS_H

// Clase para gestionar la configuración del juego
class Settings {
private:
    int numPlayers;     // 0: todos automáticos, 1: un jugador manual, 2: jugador vs auto
    int numBoards;      // Número de tableros simultáneos (1-9)

public:
    Settings();
    
    // Getters
    int getNumPlayers() const;
    int getNumBoards() const;
    
    // Setters
    void setNumPlayers(int players);
    void setNumBoards(int boards);
    
    // Validación
    bool isValid() const;
};

#endif
