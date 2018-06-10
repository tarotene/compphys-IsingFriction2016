#!/usr/bin/env bash

ifort -mkl src/MTC_analyze_2d_eq.o lib/mod_proc.o lib/mod_global.o -o bin/MTC_analyze_2d_eq
ifort -mkl src/MTC_Ising_2d_eq.o lib/mod_proc.o lib/mod_global.o -o bin/MTC_Ising_2d_eq