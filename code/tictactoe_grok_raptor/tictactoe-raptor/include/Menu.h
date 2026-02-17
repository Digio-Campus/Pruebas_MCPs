#pragma once

#include <vector>
#include <string>

namespace ttt {

class Menu {
public:
    Menu() = default;
    // Returns index of selected item or -1 on cancel
    int run(const std::vector<std::string>& items);
};

} // namespace ttt
