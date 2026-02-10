#include "UI.h"

#include <algorithm>

UI::UI() {
  initscr();
  cbreak();
  noecho();
  keypad(stdscr, TRUE);
  curs_set(0);

  refreshSize();

  colorsEnabled_ = (has_colors() == TRUE);
  if (colorsEnabled_) {
    start_color();
    use_default_colors();

    init_pair(CP_NORMAL, COLOR_WHITE, -1);
    init_pair(CP_HILITE, COLOR_CYAN, -1);
    init_pair(CP_CURSOR, COLOR_BLACK, COLOR_CYAN);
    init_pair(CP_WIN, COLOR_GREEN, -1);
    init_pair(CP_WARN, COLOR_YELLOW, -1);
  }
}

UI::~UI() {
  endwin();
}

void UI::refreshSize() {
  getmaxyx(stdscr, termH_, termW_);
}

Layout UI::calcLayout() const {
  Layout l;

  const int availW = std::max(0, termW_ - 2 * l.marginX);
  const int availH = std::max(0, termH_ - 2 * l.marginY - l.footerH);

  // cols*boardW + (cols-1)*padX <= availW  => cols <= (availW+padX)/(boardW+padX)
  l.cols = std::max(1, (availW + l.padX) / (l.boardW + l.padX));

  // rows*boardH + (rows-1)*padY <= availH
  l.rows = std::max(1, (availH + l.padY) / (l.boardH + l.padY));

  l.maxBoards = std::max(1, l.cols * l.rows);
  return l;
}

void UI::clear() {
  ::erase();
}

void UI::drawCentered(int y, const std::string& text, int attrs) const {
  int x = std::max(0, (termW_ - static_cast<int>(text.size())) / 2);
  if (attrs) attron(attrs);
  mvaddnstr(y, x, text.c_str(), termW_ - x);
  if (attrs) attroff(attrs);
}
