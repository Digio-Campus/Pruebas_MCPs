#include <iostream>
#include <vector>
using namespace std;

void printBoard(const vector<vector<char>>& board) {
    cout << "\n";
    for (int i = 0; i < 3; ++i) {
        for (int j = 0; j < 3; ++j) {
            cout << board[i][j];
            if (j < 2) cout << " | ";
        }
        cout << "\n";
        if (i < 2) cout << "---------\n";
    }
    cout << "\n";
}

bool checkWin(const vector<vector<char>>& board, char player) {
    for (int i = 0; i < 3; ++i)
        if (board[i][0] == player && board[i][1] == player && board[i][2] == player)
            return true;
    for (int j = 0; j < 3; ++j)
        if (board[0][j] == player && board[1][j] == player && board[2][j] == player)
            return true;
    if (board[0][0] == player && board[1][1] == player && board[2][2] == player)
        return true;
    if (board[0][2] == player && board[1][1] == player && board[2][0] == player)
        return true;
    return false;
}

bool checkDraw(const vector<vector<char>>& board) {
    for (int i = 0; i < 3; ++i)
        for (int j = 0; j < 3; ++j)
            if (board[i][j] == ' ')
                return false;
    return true;
}

int main() {
    vector<vector<char>> board(3, vector<char>(3, ' '));
    char currentPlayer = 'X';
    int row, col;
    while (true) {
        printBoard(board);
        cout << "Turno de " << currentPlayer << ". Ingresa fila y columna (1-3): ";
        cin >> row >> col;
        --row; --col;
        if (row < 0 || row > 2 || col < 0 || col > 2 || board[row][col] != ' ') {
            cout << "Movimiento inválido. Intenta de nuevo.\n";
            continue;
        }
        board[row][col] = currentPlayer;
        if (checkWin(board, currentPlayer)) {
            printBoard(board);
            cout << "¡Gana " << currentPlayer << "!\n";
            break;
        }
        if (checkDraw(board)) {
            printBoard(board);
            cout << "¡Empate!\n";
            break;
        }
        currentPlayer = (currentPlayer == 'X') ? 'O' : 'X';
    }
    return 0;
}
