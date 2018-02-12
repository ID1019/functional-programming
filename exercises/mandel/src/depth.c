#include "erl_nif.h"
#include "stdio.h"

int test(double real, double imag, int max) {

  long double zr = 0.0;
  long double zi = 0.0;

  long double r = real;
  long double i = imag;

  int depth = 0;

  while (depth < max) {
    long double zr2 = zr * zr;
    long double zi2 = zi * zi;
    long double a2 = zr2 + zi2;
    if (a2 < 4.0) {
      depth++;
      zi = 2 * zr * zi + i;
      zr = zr2 - zi2 + r;
    } else {
      return depth;
    }
  }
  return 0;
}

static ERL_NIF_TERM test_nif(ErlNifEnv *env, int argc,
                             const ERL_NIF_TERM argv[]) {
  int ret;
  int max;
  double real, imag;

  if (!enif_get_double(env, argv[0], &real)) {
    return enif_make_badarg(env);
  }

  if (!enif_get_double(env, argv[1], &imag)) {
    return enif_make_badarg(env);
  }

  if (!enif_get_int(env, argv[2], &max)) {
    return enif_make_badarg(env);
  }
  ret = test(real, imag, max);
  return enif_make_int(env, ret);
}

static ErlNifFunc nif_funcs[] = {
    {"test", 3, test_nif},
};

ERL_NIF_INIT(Elixir.Depth, nif_funcs, NULL, NULL, NULL, NULL)
