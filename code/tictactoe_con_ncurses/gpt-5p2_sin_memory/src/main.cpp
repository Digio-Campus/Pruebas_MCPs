#include "Settings.h"
#include "Ui.h"

#include <exception>
#include <iostream>

int main(int argc, char **argv) {
  // Pequeña ayuda por CLI para entornos no interactivos.
  if (argc > 1) {
    std::string arg = argv[1];
    if (arg == "--help" || arg == "-h") {
      std::cout << "Tres en Raya (ncurses)\n"
                   "Uso: "
                << argv[0] << "\n\n"
                << "Compila con: make -C code\n"
                << "Ejecuta con: ./code/bin/tres_en_raya\n";
      return 0;
    }
  }

  try {
    Settings settings;
    settings.clamp();

    Ui ui(settings);
    return ui.run();
  } catch (const std::exception &e) {
    std::cerr << "Error: " << e.what() << "\n";
    return 1;
  } catch (...) {
    std::cerr << "Error desconocido\n";
    return 1;
  }
}
