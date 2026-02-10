#pragma once

#include "Settings.h"
#include "UI.h"

class Menu {
public:
  enum class Action { Play, Exit };

  explicit Menu(UI& ui) : ui_(ui) {}

  Action main(Settings& settings);
  void settings(Settings& settings);
  void help();

private:
  UI& ui_;
};
