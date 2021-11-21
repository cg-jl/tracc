int main() {
  int a = 1;
  int b = a + 2;
  a || (b = 5);
  return b - a;
}
