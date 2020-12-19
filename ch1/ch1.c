#include <stdio.h>

typedef enum { false, true } bool;

float square(float x) {
     return x * x;
}

float my_abs(float x) {
     return x < 0 ? -x : x;
}

float average(float x, float y) {
     return (x + y) / 2;
}

bool good_enough(float guess, float x) {
     return my_abs(square(guess) - x) < 0.001 ? true : false;
}

float improve(float guess, float x) {
     return average(guess, x / guess);
}

float sqrt_iter(float guess, float x) {
     if (good_enough(guess, x))
          return guess;
     return sqrt_iter(improve(guess, x), x);
}

float my_sqrt(float x) {
     return sqrt_iter(1.0, x);
}

int main(int argc, char** argv) {
     printf("sqrt(2): %f\n", my_sqrt(2));
     printf("sqrt(3): %f\n", my_sqrt(3));
     printf("sqrt(4): %f\n", my_sqrt(4));
     return 0;
}
