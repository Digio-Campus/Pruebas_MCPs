#include "App.h"

#include <clocale>
#include <ncurses.h>

namespace {
class CursesSession {
public:
  CursesSession() {
    setlocale(LC_ALL, "");
    win_ = initscr();
    if (!win_) {
      ok_ = false;
      return;
    }
    ok_ = true;

    cbreak();
    noecho();
    keypad(stdscr, TRUE);
    curs_set(0);

    if (has_colors()) {
      start_color();
      use_default_colors();
    }
  }

  ~CursesSession() {
    if (ok_) endwin();
  }

  bool ok() const { return ok_; }

private:
  WINDOW *win_ = nullptr;
  bool ok_ = false;
};
} // namespace

int main() {
  CursesSession session;
  if (!session.ok()) return 1;

  App app;
  return app.run();
}
