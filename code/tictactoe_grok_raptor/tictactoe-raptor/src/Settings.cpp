#include "../include/Settings.h"
#include <algorithm>

using namespace ttt;

Settings::Settings()
    : numPlayers_(1) // OBLIGATORIO: valores por defecto razonables
    , numBoards_(1)
{
}

void Settings::setNumPlayers(int p)
{
    numPlayers_ = std::clamp(p, 0, 2);
}

void Settings::setNumBoards(int b)
{
    numBoards_ = std::clamp(b, 1, 9);
}

bool Settings::isValid() const
{
    return (numPlayers_ >= 0 && numPlayers_ <= 2 && numBoards_ >= 1 && numBoards_ <= 9);
}
