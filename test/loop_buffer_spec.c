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
    test("create_empty_loopnaut creates an empty audio buffer");
    struct loopnaut* loopnaut = create_empty_loopnaut();
    assertEqual(loopnaut->length, 0);
    free(loopnaut);
  }

  {
    test("create_empty_loopnaut returns a silent sample");
    struct loopnaut* loopnaut = create_empty_loopnaut();
    assertEqual(get_next_sample(loopnaut), 0);
    free(loopnaut);
  }

  {
    test("create_empty_loopnaut returns silent samples on subsequent calls");
    struct loopnaut* loopnaut = create_empty_loopnaut();
    get_next_sample(loopnaut);
    assertEqual(get_next_sample(loopnaut), 0);
  }

  {
    test("set_buffer allows to set a buffer to a constant");
    struct loopnaut* loopnaut = create_empty_loopnaut();
    float array[1] = {42.0};
    set_buffer(loopnaut, array, 1);
    get_next_sample(loopnaut);
    assertEqual(get_next_sample(loopnaut), 42);
    free(loopnaut);
  }

  {
    test("set_buffer allows to set a buffer to a sequence");
    struct loopnaut* loopnaut = create_empty_loopnaut();
    float sequence[3] = {1, 2, 3};
    set_buffer(loopnaut, sequence, 3);
    assertEqual(get_next_sample(loopnaut), 1);
    assertEqual(get_next_sample(loopnaut), 2);
    assertEqual(get_next_sample(loopnaut), 3);
    free(loopnaut);
  }

  {
    test("set_buffer loops sequences");
    struct loopnaut* loopnaut = create_empty_loopnaut();
    float sequence[3] = {1, 2, 3};
    set_buffer(loopnaut, sequence, 3);
    get_next_sample(loopnaut);
    get_next_sample(loopnaut);
    get_next_sample(loopnaut);
    assertEqual(get_next_sample(loopnaut), 1);
    assertEqual(get_next_sample(loopnaut), 2);
    assertEqual(get_next_sample(loopnaut), 3);
    free(loopnaut);
  }

  {
    test("set_buffer loops constants");
    struct loopnaut* loopnaut = create_empty_loopnaut();
    float array[1] = {42};
    set_buffer(loopnaut, array, 1);
    get_next_sample(loopnaut);
    assertEqual(get_next_sample(loopnaut), 42);
    free(loopnaut);
  }

  {
    test("set_buffer waits for the current sequence to finish before switching");
    struct loopnaut* loopnaut = create_empty_loopnaut();
    float a[3] = {1, 2, 3};
    set_buffer(loopnaut, a, 3);
    assertEqual(get_next_sample(loopnaut), 1);
    float b[3] = {4, 5, 6};
    set_buffer(loopnaut, b, 3);
    assertEqual(get_next_sample(loopnaut), 2);
    assertEqual(get_next_sample(loopnaut), 3);
    assertEqual(get_next_sample(loopnaut), 4);
    assertEqual(get_next_sample(loopnaut), 5);
    assertEqual(get_next_sample(loopnaut), 6);
    free(loopnaut);
  }

  {
    test("set_buffer handles two calls to set_buffer gracefully");
    struct loopnaut* loopnaut = create_empty_loopnaut();
    float x[3] = {1, 2, 3};
    set_buffer(loopnaut, x, 3);
    float y[3] = {4, 5, 6};
    set_buffer(loopnaut, y, 3);
    assertEqual(get_next_sample(loopnaut), 4);
    assertEqual(get_next_sample(loopnaut), 5);
    assertEqual(get_next_sample(loopnaut), 6);
    free(loopnaut);
  }

  return result;
}
