#!/usr/bin/env bash

for beta in $(cat ../../list_beta.dat); do
	cd "beta${beta}"
	rip eb_sweep/*
	rip ee_sweep/*
	rip en_step/*
	rip m_step/*
	rip mb_sweep/*
	rip me_sweep/*
	rip p_sweep/*
	rip sp_sweep/*
	cd ../
done