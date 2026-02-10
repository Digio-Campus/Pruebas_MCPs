#pragma once

#include <stdexcept>

#include <ncurses.h>

// RAII para inicializar/terminar ncurses de forma segura.
class NcursesSession {
public:
  NcursesSession();
  ~NcursesSession();

  NcursesSession(const NcursesSession&) = delete;
  NcursesSession& operator=(const NcursesSession&) = delete;
};
