int main() {
    int a = 0;
    int b = 1;

    for (int i = 0; i < 10; i += 1) {
        int temp = a;
        a = a + b;
        b = temp;
        i += 1;
    }

    return a;
}
