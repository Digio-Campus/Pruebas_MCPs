#include "UI.h"

#include <algorithm>
#include <sstream>

#include <ncurses.h>

UI::UI() {
    initscr();
    cbreak();
    noecho();
    keypad(stdscr, TRUE);
    curs_set(0);

    // Ratón.
    mousemask(ALL_MOUSE_EVENTS | REPORT_MOUSE_POSITION, nullptr);

    // Colores.
    if (has_colors()) {
        start_color();
#ifdef NCURSES_VERSION
        use_default_colors();
#endif
        m_colors = true;
        initColors_();
    }

    updateSize();
}

UI::~UI() {
    endwin();
}

void UI::initColors_() {
    // 1: título
    init_pair(1, COLOR_CYAN, -1);
    // 2: selección
    init_pair(2, COLOR_BLACK, COLOR_YELLOW);
    // 3: bordes
    init_pair(3, COLOR_WHITE, -1);
    // 4: X
    init_pair(4, COLOR_RED, -1);
    // 5: O
    init_pair(5, COLOR_GREEN, -1);
    // 6: avisos/estado
    init_pair(6, COLOR_YELLOW, -1);
}

void UI::updateSize() {
    getmaxyx(stdscr, m_termH, m_termW);
}

void UI::setTimeoutMs(int ms) {
    timeout(ms);
}

int UI::getInput() {
    return getch();
}

void UI::clear() {
    ::erase();
}

void UI::refresh() {
    ::refresh();
}

void UI::drawFrameTitle(const std::string& title) {
    int y = 0;
    if (m_colors) attron(COLOR_PAIR(1) | A_BOLD);
    mvhline(y, 0, ' ', m_termW);
    mvaddnstr(y, std::max(0, (m_termW - (int)title.size()) / 2), title.c_str(), m_termW);
    if (m_colors) attroff(COLOR_PAIR(1) | A_BOLD);
}

void UI::drawCenteredText(int y, const std::string& text, int colorPair, bool bold) {
    int x = std::max(0, (m_termW - (int)text.size()) / 2);
    if (m_colors && colorPair != 0) attron(COLOR_PAIR(colorPair));
    if (bold) attron(A_BOLD);
    mvaddnstr(y, x, text.c_str(), m_termW);
    if (bold) attroff(A_BOLD);
    if (m_colors && colorPair != 0) attroff(COLOR_PAIR(colorPair));
}

void UI::drawRectBorder_(const Rect& r, int colorPair, bool thick) {
    if (r.w <= 1 || r.h <= 1) return;

    if (m_colors && colorPair != 0) attron(COLOR_PAIR(colorPair));
    if (thick) attron(A_BOLD);

    // Top / bottom
    mvhline(r.y, r.x, 0, 0); // no-op en algunos ncurses
    mvaddch(r.y, r.x, '+');
    mvhline(r.y, r.x + 1, '-', r.w - 2);
    mvaddch(r.y, r.x + r.w - 1, '+');

    mvaddch(r.y + r.h - 1, r.x, '+');
    mvhline(r.y + r.h - 1, r.x + 1, '-', r.w - 2);
    mvaddch(r.y + r.h - 1, r.x + r.w - 1, '+');

    // Sides
    for (int yy = r.y + 1; yy < r.y + r.h - 1; ++yy) {
        mvaddch(yy, r.x, '|');
        mvaddch(yy, r.x + r.w - 1, '|');
    }

    if (thick) attroff(A_BOLD);
    if (m_colors && colorPair != 0) attroff(COLOR_PAIR(colorPair));
}

void UI::drawMainMenu(const Menu& menu) {
    drawFrameTitle("Tres en Raya (ncurses)");

    const auto& rects = menu.itemRects();
    if (rects.empty()) return;

    int minX = rects[0].x;
    int minY = rects[0].y;
    int maxX = rects[0].x + rects[0].w;
    int maxY = rects[0].y + rects[0].h;
    for (auto& r : rects) {
        minX = std::min(minX, r.x);
        minY = std::min(minY, r.y);
        maxX = std::max(maxX, r.x + r.w);
        maxY = std::max(maxY, r.y + r.h);
    }

    Rect frame{ minX - 2, minY - 2, (maxX - minX) + 4, (maxY - minY) + 4 };
    drawRectBorder_(frame, 3, false);

    for (int i = 0; i < (int)menu.items().size(); ++i) {
        const auto& item = menu.items()[i];
        const Rect& r = rects[i];

        bool sel = (i == menu.selected());
        if (sel) {
            if (m_colors) attron(COLOR_PAIR(2));
            else attron(A_REVERSE);
        }

        std::string label = "  " + item + "  ";
        int x = r.x + std::max(0, (r.w - (int)label.size()) / 2);
        mvaddnstr(r.y, x, label.c_str(), r.w);

        if (sel) {
            if (m_colors) attroff(COLOR_PAIR(2));
            else attroff(A_REVERSE);
        }
    }

    drawCenteredText(frame.y + frame.h + 1, "Ratón: clic para seleccionar | Teclado: \u2191\u2193 + Enter", 6, false);
}

UI::SettingsHitAreas UI::drawSettings(const Settings& settings, int focusedRow) {
    drawFrameTitle("Ajustes");

    SettingsHitAreas a{};

    int y0 = 4;
    int x0 = std::max(2, (m_termW - 50) / 2);

    auto drawRow = [&](int row, const std::string& name, const std::string& value, Rect& minusR, Rect& plusR) {
        int y = y0 + row * 3;

        bool focused = (focusedRow == row);
        if (focused) attron(A_BOLD);
        mvaddnstr(y, x0, name.c_str(), m_termW - x0);
        if (focused) attroff(A_BOLD);

        std::string minus = "[ - ]";
        std::string plus  = "[ + ]";

        int vx = x0 + 22;
        minusR = Rect{ vx, y, (int)minus.size(), 1 };
        plusR  = Rect{ vx + 18, y, (int)plus.size(), 1 };

        mvaddnstr(y, minusR.x, minus.c_str(), minusR.w);
        mvaddnstr(y, plusR.x, plus.c_str(), plusR.w);

        std::string val = "[ " + value + " ]";
        mvaddnstr(y, vx + 6, val.c_str(), (int)val.size());
    };

    std::string playersValue = std::to_string(settings.numPlayers());
    std::string boardsValue = std::to_string(settings.numBoards());

    drawRow(0, "Jugadores (0/1/2)", playersValue, a.playersMinus, a.playersPlus);
    drawRow(1, "Tableros (1-9)", boardsValue, a.boardsMinus, a.boardsPlus);

    int backY = y0 + 2 * 3;
    std::string back = "[ Volver ]";
    a.back = Rect{ x0 + 22, backY, (int)back.size(), 1 };

    bool focused = (focusedRow == 2);
    if (focused) {
        if (m_colors) attron(COLOR_PAIR(2));
        else attron(A_REVERSE);
    }
    mvaddnstr(a.back.y, a.back.x, back.c_str(), a.back.w);
    if (focused) {
        if (m_colors) attroff(COLOR_PAIR(2));
        else attroff(A_REVERSE);
    }

    drawCenteredText(m_termH - 2, "Flechas \u2191\u2193 para elegir | \u2190\u2192 para cambiar | Ratón: clic en [ - ] / [ + ]", 6, false);
    return a;
}

void UI::drawHelp() {
    drawFrameTitle("Ayuda");

    int y = 3;
    mvaddstr(y++, 2, "CONTROLES (en partida):");
    mvaddstr(y++, 4, "- Flechas: mover el cursor por la celda activa");
    mvaddstr(y++, 4, "- Enter / Espacio: colocar ficha en la celda seleccionada");
    mvaddstr(y++, 4, "- Tab: cambiar de tablero");
    mvaddstr(y++, 4, "- R: reiniciar SOLO el tablero activo (estadisticas se mantienen)");
    mvaddstr(y++, 4, "- Q: volver al menu");

    y++;
    mvaddstr(y++, 2, "RATON:");
    mvaddstr(y++, 4, "- Menu/Ajustes: clic para seleccionar" );
    mvaddstr(y++, 4, "- Partida: clic en un tablero para activarlo; clic en una celda para jugar" );

    y++;
    mvaddstr(y++, 2, "MODOS (Ajustes -> Jugadores):");
    mvaddstr(y++, 4, "0 jugadores: X y O se juegan automaticamente de forma aleatoria (todos los tableros)." );
    mvaddstr(y++, 4, "1 jugador: control manual de X y O; cada tablero alterna X->O estrictamente." );
    mvaddstr(y++, 4, "2 jugadores: controlas O; X se genera automaticamente cuando toque jugar X." );

    y++;
    mvaddstr(y++, 2, "REGLAS:");
    mvaddstr(y++, 4, "- Cada tablero es independiente: turno, victorias y empates separados." );
    mvaddstr(y++, 4, "- En cada tablero la alternancia es estricta: X siempre antes que O." );

    drawCenteredText(m_termH - 2, "Pulsa cualquier tecla o haz clic para volver", 6, true);
}

std::vector<Rect> UI::computeBoardRects(int numBoards) const {
    std::vector<Rect> out;
    if (numBoards <= 0) return out;

    const int top = 2;
    const int bottom = 2;
    int availH = std::max(1, m_termH - top - bottom);

    const int minBW = 14;
    const int minBH = 9;

    int bestCols = 1;
    int maxTry = std::min(3, numBoards);

    for (int cols = maxTry; cols >= 1; --cols) {
        int rows = (numBoards + cols - 1) / cols;
        int cellW = m_termW / cols;
        int cellH = availH / rows;
        if (cellW >= minBW && cellH >= minBH) { bestCols = cols; break; }
    }

    int cols = bestCols;
    int rows = (numBoards + cols - 1) / cols;
    int cellW = std::max(1, m_termW / cols);
    int cellH = std::max(1, availH / rows);

    out.reserve(numBoards);
    for (int i = 0; i < numBoards; ++i) {
        int r = i / cols;
        int c = i % cols;
        Rect rect{ c * cellW, top + r * cellH, cellW, cellH };
        rect = rect.inset(1, 0);
        // Asegurar un mínimo visible (aunque el terminal sea pequeño).
        rect.w = std::max(rect.w, 10);
        rect.h = std::max(rect.h, 6);
        out.push_back(rect);
    }
    return out;
}

static const char* playersModeName(int n) {
    switch (n) {
        case 0: return "0 jugadores (auto)";
        case 1: return "1 jugador (manual X+O)";
        case 2: return "2 jugadores (O manual, X auto)";
        default: return "?";
    }
}

void UI::drawGame(const std::vector<Board>& boards,
                  const std::vector<Rect>& boardRects,
                  int selectedBoard,
                  int cursorRow,
                  int cursorCol,
                  const Settings& settings,
                  const std::string& statusLine) {
    std::ostringstream oss;
    oss << "Partida - " << playersModeName(settings.numPlayers())
        << " | Tableros: " << settings.numBoards();
    drawFrameTitle(oss.str());

    if ((int)boards.size() != (int)boardRects.size()) return;

    for (int i = 0; i < (int)boards.size(); ++i) {
        const Rect& r = boardRects[i];
        bool sel = (i == selectedBoard);
        drawRectBorder_(r, 3, sel);

        // Encabezado del tablero + stats.
        if (m_colors) attron(COLOR_PAIR(6));
        mvprintw(r.y, r.x + 2, "B#%d X:%d O:%d D:%d", i + 1, boards[i].xWins(), boards[i].oWins(), boards[i].draws());
        if (m_colors) attroff(COLOR_PAIR(6));

        Rect inner = r.inset(1, 1);
        if (inner.w < 6 || inner.h < 3) continue;

        int cw = std::max(1, inner.w / 3);
        int ch = std::max(1, inner.h / 3);

        // Separadores.
        for (int c = 1; c < 3; ++c) {
            int x = inner.x + c * cw;
            for (int y = inner.y; y < inner.y + inner.h; ++y) mvaddch(y, x, '|');
        }
        for (int rr = 1; rr < 3; ++rr) {
            int y = inner.y + rr * ch;
            for (int x = inner.x; x < inner.x + inner.w; ++x) mvaddch(y, x, '-');
        }

        // Marcas.
        for (int rr = 0; rr < 3; ++rr) {
            for (int cc = 0; cc < 3; ++cc) {
                char m = boards[i].cell(rr, cc);
                int cx = inner.x + cc * cw + cw / 2;
                int cy = inner.y + rr * ch + ch / 2;

                bool cursor = sel && rr == cursorRow && cc == cursorCol;
                if (cursor) {
                    if (m_colors) attron(COLOR_PAIR(2));
                    else attron(A_REVERSE);
                }

                if (m == 'X' && m_colors) attron(COLOR_PAIR(4) | A_BOLD);
                if (m == 'O' && m_colors) attron(COLOR_PAIR(5) | A_BOLD);

                mvaddch(cy, cx, (m == ' ') ? '.' : m);

                if (m == 'X' && m_colors) attroff(COLOR_PAIR(4) | A_BOLD);
                if (m == 'O' && m_colors) attroff(COLOR_PAIR(5) | A_BOLD);

                if (cursor) {
                    if (m_colors) attroff(COLOR_PAIR(2));
                    else attroff(A_REVERSE);
                }
            }
        }

        // Estado del tablero.
        std::string st;
        switch (boards[i].result()) {
            case BoardResult::InProgress:
                st = std::string("Turno: ") + boards[i].nextTurn();
                break;
            case BoardResult::XWin: st = "X GANA (R para reiniciar)"; break;
            case BoardResult::OWin: st = "O GANA (R para reiniciar)"; break;
            case BoardResult::Draw: st = "EMPATE (R para reiniciar)"; break;
        }
        if (m_colors) attron(COLOR_PAIR(6));
        mvaddnstr(r.y + r.h - 1, r.x + 2, st.c_str(), r.w - 4);
        if (m_colors) attroff(COLOR_PAIR(6));
    }

    if (m_colors) attron(COLOR_PAIR(6));
    mvhline(m_termH - 1, 0, ' ', m_termW);
    mvaddnstr(m_termH - 1, 1, statusLine.c_str(), m_termW - 2);
    if (m_colors) attroff(COLOR_PAIR(6));
}

void UI::drawTooSmall(const std::string& msg) {
    drawFrameTitle("Terminal demasiado pequeño");
    drawCenteredText(m_termH / 2, msg, 6, true);
    drawCenteredText(m_termH / 2 + 2, "Redimensiona la ventana o reduce el numero de tableros.", 6, false);
}
