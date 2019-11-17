/*
 *  libigl_nif.c --
 *
 *     Erlang nif for accessing libigl functionality.
 *
 *  Copyright (c) 2019 Dan Gudmundsson
 *
 *  See the file "license.terms" for information on usage and redistribution
 *  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
 *
 */

#ifdef __WIN32__
#define WIN32
//#define WIN32_LEAN_AND_MEAN
//#include <windows.h>
#endif
#include <string.h>

class nif_error
{
public:
  nif_error(const char * What, const char * Func, const char * File, int Line) :
    what(What),  func(Func), file(File), line(Line) { } ;
  const char * what = NULL;
  const char * func = NULL;
  const char * file = NULL;
  int line = 0;
};

#define eigen_assert(x) \
 do { \
   if(!Eigen::internal::copy_bool(x))                          \
     throw nif_error(#x, __FUNCTION__, __FILE__, __LINE__);    \
 } while(false)

#define IGL_NO_OPENGL true
#define IGL_NO_ANTTWEAKBAR true

#include <Eigen/Core>
#include <igl/MappingEnergyType.h>
#include <igl/lscm.h>
#include <igl/slim.h>
#include <igl/harmonic.h>
#include <igl/flipped_triangles.h>

#define make_error(TYPE, Desc)                           \
  { fprintf(stderr, "LIBIGL:%d error %s\r\n", __LINE__, Desc);       \
    fflush(stderr); \
    return enif_raise_exception(env, \
            enif_make_tuple2(env, TYPE, enif_make_string(env, Desc, ERL_NIF_LATIN1))); \
  }

extern "C" {
#include "erl_nif.h"

  ERL_NIF_TERM atom_ok;
  ERL_NIF_TERM atom_true;
  ERL_NIF_TERM atom_false;
  ERL_NIF_TERM atom_badarg;
  ERL_NIF_TERM atom_error;

  ErlNifResourceType* igl_mem = NULL;
  ERL_NIF_TERM lscm_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
  ERL_NIF_TERM slim_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

}  // extern c

bool fetch_listT2d(ErlNifEnv* env, ERL_NIF_TERM list, Eigen::MatrixXd & Out)
{
  unsigned int len;
  int n;
  ERL_NIF_TERM hd, tail;
  const ERL_NIF_TERM *tuple;
  if(!enif_get_list_length(env, list, &len) || len == 0)
    return false;
  Out.resize(len, 2);
  enif_get_list_cell(env, list, &hd, &tail);
  for(unsigned int i=0; i<len; i++) {
    if(!enif_get_tuple(env, hd, &n, &tuple) || n != 2) return false;
    if(!enif_get_double(env, tuple[0], &Out(i,0))) return false;
    if(!enif_get_double(env, tuple[1], &Out(i,1))) return false;
    enif_get_list_cell(env, tail, &hd, &tail);
  }
  return true;
}

bool fetch_listT3d(ErlNifEnv* env, ERL_NIF_TERM list, Eigen::MatrixXd & Out)
{
  unsigned int len;
  int n;
  ERL_NIF_TERM hd, tail;
  const ERL_NIF_TERM *tuple;

  if(!enif_get_list_length(env, list, &len) || len == 0)
    return false;
  Out.resize(len, 3);
  enif_get_list_cell(env, list, &hd, &tail);
  for(unsigned int i=0; i<len; i++) {
    if(!enif_get_tuple(env, hd, &n, &tuple) || n != 3) return false;
    if(!enif_get_double(env, tuple[0], &Out(i,0))) return false;
    if(!enif_get_double(env, tuple[1], &Out(i,1))) return false;
    if(!enif_get_double(env, tuple[2], &Out(i,2))) return false;
    enif_get_list_cell(env, tail, &hd, &tail);
  }
  return true;
}

bool fetch_listi(ErlNifEnv* env, ERL_NIF_TERM list, Eigen::VectorXi & Out)
{
  unsigned int len;
  ERL_NIF_TERM hd, tail;

  if(!enif_get_list_length(env, list, &len) || len == 0)
    return false;
  Out.resize(len);
  enif_get_list_cell(env, list, &hd, &tail);
  for(unsigned int i=0; i<len; i++) {
    if(!enif_get_int(env, hd, &Out(i))) return false;
    enif_get_list_cell(env, tail, &hd, &tail);
  }
  return true;
}

bool fetch_listL3i(ErlNifEnv* env, ERL_NIF_TERM list, Eigen::MatrixXi & Out)
{
  unsigned int len, n;
  ERL_NIF_TERM hd, tail, ihd, itail;

  if(!enif_get_list_length(env, list, &len) || len == 0)
    return false;
  Out.resize(len, 3);
  enif_get_list_cell(env, list, &hd, &tail);
  for(int i=0; i< (int) len; i++) {
    if(!enif_get_list_length(env, hd, &n) || n != 3) return false;
    if(!enif_get_list_cell(env, hd, &ihd, &itail))  return false;
    if(!enif_get_int(env, ihd, &Out(i,0))) return false;
    if(!enif_get_list_cell(env, itail, &ihd, &itail))  return false;
    if(!enif_get_int(env, ihd, &Out(i,1))) return false;
    if(!enif_get_list_cell(env, itail, &ihd, &itail))  return false;
    if(!enif_get_int(env, ihd, &Out(i,2))) return false;
    enif_get_list_cell(env, tail, &hd, &tail);
  }
  return true;
}

ERL_NIF_TERM make_listT2d(ErlNifEnv *env, Eigen::MatrixXd & In)
{
  ERL_NIF_TERM head,tail;
  tail = enif_make_list(env, 0);
  for(int i = (int) In.rows()-1; i >= 0; i--) {
    head = enif_make_tuple2(env, enif_make_double(env, In(i,0)), enif_make_double(env, In(i,1)));
    tail = enif_make_list_cell(env, head, tail);
  }
  return tail;
}

ERL_NIF_TERM lscm_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  Eigen::MatrixXd Vs;
  Eigen::MatrixXi Fs;
  Eigen::VectorXi bndIndx;
  Eigen::MatrixXd bndPos;

  Eigen::MatrixXd UV;

  if(!fetch_listT3d(env, argv[0], Vs)) make_error(atom_badarg, "Vs");
  if(!fetch_listL3i(env, argv[1], Fs)) make_error(atom_badarg, "Fs");
  if(!fetch_listi(env, argv[2], bndIndx)) make_error(atom_badarg, "bndIndx");
  if(!fetch_listT2d(env, argv[3], bndPos)) make_error(atom_badarg, "bndPos");

  try {
    if(!igl::lscm(Vs,Fs,bndIndx,bndPos,UV)) return atom_false;
  } catch (nif_error& err) {
    std::cout << "Exception: " << err.what << "\r\n";
    make_error(atom_error, err.what);
  };
  // std::cout << "UV:" << UV.rows() << "x" << UV.cols() << "\r\n";
  return make_listT2d(env, UV);
}

ERL_NIF_TERM harmonic_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  Eigen::MatrixXd Vs;
  Eigen::MatrixXi Fs;
  Eigen::VectorXi bndIndx;
  Eigen::MatrixXd bndPos;

  Eigen::MatrixXd UV;

  if(!fetch_listT3d(env, argv[0], Vs)) make_error(atom_badarg, "Vs");
  if(!fetch_listL3i(env, argv[1], Fs)) make_error(atom_badarg, "Fs");
  if(!fetch_listi(env, argv[2], bndIndx)) make_error(atom_badarg, "borderIndx");
  if(!fetch_listT2d(env, argv[3], bndPos)) make_error(atom_badarg, "borderPos");

  if(bndIndx.rows() != bndPos.rows())
    make_error(atom_badarg, "border args should be of same length");

  if(bndIndx.rows() >= Vs.rows()) {
    // We are done all Vs mapped in BorderPos
    // remap them to the correct order
    UV.resize(Vs.rows(), 2);
    for(int i=0; i < Vs.rows(); i++) {
      int indx = bndIndx(i);
      UV(indx,0) = bndPos(i,0);
      UV(indx,1) = bndPos(i,1);
    }
    return make_listT2d(env, UV);
  }

  try {
    if(!igl::harmonic(Vs,Fs,bndIndx,bndPos,1,UV)) make_error(atom_error, "calc1 failed");
    if(igl::flipped_triangles(UV,Fs).size() != 0) {
      if(!igl::harmonic(Fs,bndIndx,bndPos,1,UV)) // use uniform laplacian
        make_error(atom_error, "calc2 failed");
    }
  } catch (nif_error& err) {
    std::cout << "Exception: " << err.what << "\r\n";
    make_error(atom_error, err.what);
  };
  // std::cout << "UV:" << UV.rows() << "x" << UV.cols() << "\r\n";
  return make_listT2d(env, UV);
}

ERL_NIF_TERM slim_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  Eigen::MatrixXd Vs;
  Eigen::MatrixXi Fs;
  Eigen::MatrixXd UV;
  Eigen::VectorXi b; Eigen::MatrixXd bc;
  char eTypeStr[64];
  double eps;
  int iter=0;

  if(!fetch_listT3d(env, argv[0], Vs)) make_error(atom_badarg, "Vs");
  // std::cout << "Vs:" << Vs.rows() << "x" << Vs.cols() << "\r\n";
  if(!fetch_listL3i(env, argv[1], Fs)) make_error(atom_badarg, "Fs");
  // std::cout << "Fs:" << Fs.rows() << "x" << Fs.cols() << "\r\n";
  if(!fetch_listT2d(env, argv[2], UV)) make_error(atom_badarg, "UV");
  // std::cout << "UV:" << UV.rows() << "x" << UV.cols() << "\r\n";
  if(Vs.rows() != UV.rows()) make_error(atom_badarg, "UV size is not equal to Vs size");

  if(!enif_get_atom(env, argv[3], eTypeStr, 64, ERL_NIF_LATIN1))
    make_error(atom_badarg, "Type should be an atom");
  if(!enif_get_double(env, argv[4], &eps)) make_error(atom_badarg, "eps");

  igl::SLIMData sData;

  if(!strcmp("arap", eTypeStr)) {
    sData.slim_energy = igl::MappingEnergyType::ARAP;
  } else if(!strcmp("log_arap", eTypeStr)) {
    sData.slim_energy = igl::MappingEnergyType::LOG_ARAP;
  } else if(!strcmp("symmetric_dirichlet", eTypeStr)) {
    sData.slim_energy = igl::MappingEnergyType::SYMMETRIC_DIRICHLET;
  } else if(!strcmp("conformal", eTypeStr)) {
    sData.slim_energy = igl::MappingEnergyType::CONFORMAL;
  } else if(!strcmp("exp_conformal", eTypeStr)) {
    sData.slim_energy = igl::MappingEnergyType::EXP_CONFORMAL;
  } else if(!strcmp("exp_symmetric_dirichlet", eTypeStr)) {
    sData.slim_energy = igl::MappingEnergyType::EXP_SYMMETRIC_DIRICHLET;
  } else make_error(atom_badarg, "Non supported Type");

  try {
    igl::slim_precompute(Vs, Fs, UV, sData, sData.slim_energy, b, bc, 0);
    // std::cout << "Input Energy: " << sData.energy  << "\r\n";
    double old_energy = sData.energy;
    int step = 5;
    slim_solve(sData, step);
    iter += step;
    while((old_energy - sData.energy) > eps && iter < 100) {
      old_energy = sData.energy;
      iter += step;
      slim_solve(sData, step);
    }
    // std::cout << "Stop Energy: " << iter << " " << sData.energy  << "\r\n";
  } catch (nif_error& err) {
    std::cout << "Exception: " << err.what << "\r\n";
    make_error(atom_error, err.what);
  };
  return make_listT2d(env, sData.V_o);
}

extern "C" {
static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    atom_ok     = enif_make_atom(env, "ok");
    atom_true   = enif_make_atom(env, "true");
    atom_false  = enif_make_atom(env, "false");
    atom_badarg = enif_make_atom(env, "badarg");
    atom_error  = enif_make_atom(env, "error");
    
    igl_mem = enif_open_resource_type(env, NULL, "igl_mem", NULL, ERL_NIF_RT_CREATE, NULL);
    return 0;
}

static ErlNifFunc nif_funcs[] =
  {
    {"lscm", 4, lscm_nif, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"harmonic", 4, harmonic_nif, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"slim", 5, slim_nif, ERL_NIF_DIRTY_JOB_CPU_BOUND},
  };

ERL_NIF_INIT(libigl,nif_funcs,load,NULL,NULL,NULL)
}  // extern c
