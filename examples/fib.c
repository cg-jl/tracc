int main() {
    int a = 0;
    int b = 1;

    int i = 0;
    while (i < 10) {
        int temp = a;
        a = a + b;
        b = temp;
        i += 1;
    }

    return a;
}
