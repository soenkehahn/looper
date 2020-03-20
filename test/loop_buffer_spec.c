#include <stdio.h>
#include <stdlib.h>
#include "../src/loop_buffer.c"

int result = 0;

void test(char* string) {
  fprintf(stderr, "      %s\n", string);
}

void assertEqual(float a, float b) {
  if (a == b) {
    return;
  } else {
    fprintf(stderr, "        assertEqual: %f == %f\n", a, b);
    result++;
  }
}

int main() {
  {
    test("create_empty_looper creates an empty audio buffer");
    struct looper* looper = create_empty_looper();
    assertEqual(looper->length, 0);
    free(looper);
  }

  {
    test("create_empty_looper returns a silent sample");
    struct looper* looper = create_empty_looper();
    assertEqual(get_next_sample(looper), 0);
    free(looper);
  }

  {
    test("create_empty_looper returns silent samples on subsequent calls");
    struct looper* looper = create_empty_looper();
    get_next_sample(looper);
    assertEqual(get_next_sample(looper), 0);
  }

  {
    test("set_buffer allows to set a buffer to a constant");
    struct looper* looper = create_empty_looper();
    float array[1] = {42.0};
    set_buffer(looper, array, 1);
    get_next_sample(looper);
    assertEqual(get_next_sample(looper), 42);
    free(looper);
  }

  {
    test("set_buffer allows to set a buffer to a sequence");
    struct looper* looper = create_empty_looper();
    float sequence[3] = {1, 2, 3};
    set_buffer(looper, sequence, 3);
    assertEqual(get_next_sample(looper), 1);
    assertEqual(get_next_sample(looper), 2);
    assertEqual(get_next_sample(looper), 3);
    free(looper);
  }

  {
    test("set_buffer loops sequences");
    struct looper* looper = create_empty_looper();
    float sequence[3] = {1, 2, 3};
    set_buffer(looper, sequence, 3);
    get_next_sample(looper);
    get_next_sample(looper);
    get_next_sample(looper);
    assertEqual(get_next_sample(looper), 1);
    assertEqual(get_next_sample(looper), 2);
    assertEqual(get_next_sample(looper), 3);
    free(looper);
  }

  {
    test("set_buffer loops constants");
    struct looper* looper = create_empty_looper();
    float array[1] = {42};
    set_buffer(looper, array, 1);
    get_next_sample(looper);
    assertEqual(get_next_sample(looper), 42);
    free(looper);
  }

  {
    test("set_buffer waits for the current sequence to finish before switching");
    struct looper* looper = create_empty_looper();
    float a[3] = {1, 2, 3};
    set_buffer(looper, a, 3);
    assertEqual(get_next_sample(looper), 1);
    float b[3] = {4, 5, 6};
    set_buffer(looper, b, 3);
    assertEqual(get_next_sample(looper), 2);
    assertEqual(get_next_sample(looper), 3);
    assertEqual(get_next_sample(looper), 4);
    assertEqual(get_next_sample(looper), 5);
    assertEqual(get_next_sample(looper), 6);
    free(looper);
  }

  {
    test("set_buffer handles two calls to set_buffer gracefully");
    struct looper* looper = create_empty_looper();
    float x[3] = {1, 2, 3};
    set_buffer(looper, x, 3);
    float y[3] = {4, 5, 6};
    set_buffer(looper, y, 3);
    assertEqual(get_next_sample(looper), 4);
    assertEqual(get_next_sample(looper), 5);
    assertEqual(get_next_sample(looper), 6);
    free(looper);
  }

  return result;
}
