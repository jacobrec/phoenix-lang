#include <stdio.h>

extern int phoenix_start();

void print_val(int v) {
    if ((v & 0b11) == 0b00) {
        printf("%d", v >> 2);
    } else if ((v & 0b11111111) == 0b00001111) {
        printf("'%c'", (v >> 8) & 0xFF);
    } else if ((v & 0b11111111) == 0b00101111) {
        printf("[]");
    } else if ((v &  0b1111111) ==  0b0011111) {
        printf("%s", (1 & (v >> 7)) ? "true" : "false");
    }
}

int main() {
    int v = phoenix_start();
    print_val(v);
    puts("");
    return 0;
}
