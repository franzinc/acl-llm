#include <immintrin.h>
/* compile with
   gcc -shared -o simdot.so -O3 -msse2 simdot.c
*/
float simdot(const float *a, const float *b, int size) {
  __m128 sum = _mm_setzero_ps(); // Initialize to zero
  int i;

  for (i = 0; i <= size - 4; i += 4) {
    __m128 vec_a = _mm_loadu_ps(&a[i]);
    __m128 vec_b = _mm_loadu_ps(&b[i]);
    __m128 mul = _mm_mul_ps(vec_a, vec_b);
    sum = _mm_add_ps(sum, mul);
  }

  float result[4];
  _mm_storeu_ps(result, sum);

  float dot = result[0] + result[1] + result[2] + result[3];

  // Process any remaining elements
  for (; i < size; i++) {
    dot += a[i] * b[i];
  }

  return dot;
}
