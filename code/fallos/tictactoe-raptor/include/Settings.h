#pragma once

struct Settings {
    // 0 = fully automatic, 1 = one human (controls X and O), 2 = human controls O only
    int numPlayers = 2;
    // number of boards displayed simultaneously (1,2,4,6,9 supported)
    int numBoards = 1;

    bool isValid() const {
        return (numPlayers >= 0 && numPlayers <= 2) &&
               (numBoards == 1 || numBoards == 2 || numBoards == 4 || numBoards == 6 || numBoards == 9);
    }
};
