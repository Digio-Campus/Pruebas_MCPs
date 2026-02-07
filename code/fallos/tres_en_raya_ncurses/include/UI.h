#pragma once

#include <string>
#include <vector>

struct Rect {
  int y = 0;
  int x = 0;
  int h = 0;
  int w = 0;
};

class UI {
public:
  // Menú simple navegable con flechas + Enter.
  // Devuelve el índice seleccionado (0..n-1). Si el usuario cancela (Esc), devuelve -1.
  static int runMenu(const std::string &title, const std::vector<std::string> &items);

  static void showMessageBox(const std::string &title, const std::vector<std::string> &lines);
};
