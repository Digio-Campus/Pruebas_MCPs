#pragma once

#include "Settings.h"

class App {
public:
  int run();

private:
  void screenMainMenu();
  void screenSettings();
  void screenHelp();
  void screenGame();

  Settings settings_{};
  bool shouldExit_ = false;
};
