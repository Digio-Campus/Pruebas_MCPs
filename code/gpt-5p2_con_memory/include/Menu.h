#pragma once

#include "Settings.h"
#include "UI.h"

class Menu {
public:
  enum class Choice { Play, Settings, Help, Exit };

  Choice mainMenu(const UI& ui) const;
  void help(const UI& ui) const;
  void settings(const UI& ui, Settings& s) const;
};
