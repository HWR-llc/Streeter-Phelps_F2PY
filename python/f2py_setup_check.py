# -*- coding: utf-8 -*-
"""
===============================================================================
Hodge.WaterResources, LLC
Project Number: AD006
Project Name: FORTmod
Developed by: Matt Hodge
Type: Script

Created on Wed Nov 7 11:25:00 2018
Last Updated: 11/09/2018

Purpose: 
This script checks the environment setup for Python to make sure
that f2py will run correctly. If the setup is correct, then the script will
run f2py to create the Python dynamic link library file: str_phps.pyd/so/??.
Script works with Windows 10, Ubuntu LTS 16.04, and ??
HWR Disclaimer:
This script was created by Hodge.WaterResources, LLC (HWR). HWR makes no warranty,
expressed or implied, as to its usefulness or correctness.
===============================================================================
"""
# import necessary modules
import pdb
import os
import sys
import subprocess

# check system
if sys.platform == 'win32':
    mingw = False
    paths = os.environ['PATH']
    if r'mingw64\bin' not in paths:
        print '--Mingw was not found in System PATH--'
        print '--Check setup and re-run--'
    else:
        print '--Mingw64 found in System PATH--'
        if os.environ['CONDA_DEFAULT_ENV'] != 'fm_win':
            print '--fm_win not active Conda environment--'
            print '--Check setup and re-run--'
        else:
            print '--fm_win is active Conda environment--'
            if r'envs\fm_win' not in paths:
                print '--active environment has not been added to System PATH--'
                print '--Check setup and re-run--'
            else:
                print '--fm_win environment is in System PATH--'
                os.chdir(r'..\fortran')
                fort_file = os.getcwd() + '\\str_phps_mod.f95'
                os.chdir(r'..\python')
                try:
                    # if all checks out, run f2py
                    subprocess.call('f2py -c --compiler=mingw32 --fcompiler=gnu95 -m str_phps ' + fort_file, shell=True)
                except Exception as e:
                    print '--f2py failed, review error--'
                    print(e)
                else:
                    print '--f2py successfully created file: str_phps.pyd--'
                    print '--try a Python script--'
elif sys.platform == 'linux' or sys.platform == 'linux2':
    gfortran_check = subprocess.call('gfortran --version', shell=True)
    paths = os.environ['PATH']
    if gfortran_check != 0:
        print '--gfortran was not found in installed packages--'
        print '--Check setup and re-run--'
    else:
        print '--gfortran found in installed packages--'
        if os.environ['CONDA_DEFAULT_ENV'] != 'fm_linux':
            print '--fm_linux not active Conda environment--'
            print '--Check setup and re-run--'
        else:
            print '--fm_linux is active Conda environment--'
            if r'envs/fm_linux' not in paths:
                print '--active environment has not been added to System PATH--'
                print '--Check setup and re-run--'
            else:
                print '--fm_linux environment is in System PATH--'
                os.chdir('../fortran')
                fort_file = os.getcwd() + '/str_phps_mod.f95'
                os.chdir('../python')
                try:
                    # if all checks out, run f2py
                    subprocess.call('f2py -c -m str_phps ' + fort_file, shell=True)
                except Exception as e:
                    print '--f2py failed, review error--'
                    print(e)
                else:
                    print '--f2py successfully created file: str_phps.pyd--'
                    print '--try a Python script--'
elif sys.platform == 'darwin':
    gfortran_check = subprocess.call('gfortran --version', shell=True)
    paths = os.environ['PATH']
    if gfortran_check != 0:
        print '--gfortran was not found in installed packages--'
        print '--Check setup and re-run--'
    else:
        print '--gfortran found in installed packages--'
        if os.environ['CONDA_DEFAULT_ENV'] != 'fm_mac':
            print '--fm_mac not active Conda environment--'
            print '--Check setup and re-run--'
        else:
            print '--fm_mac is active Conda environment--'
            if r'envs/fm_mac' not in paths:
                print '--active environment has not been added to System PATH--'
                print '--Check setup and re-run--'
            else:
                print '--fm_mac environment is in System PATH--'
                os.chdir('../fortran')
                fort_file = os.getcwd() + '/str_phps_mod.f95'
                os.chdir('../python')
                try:
                    # if all checks out, run f2py
                    subprocess.call('f2py -c -m str_phps ' + fort_file, shell=True)
                except Exception as e:
                    print '--f2py failed, review error--'
                    print(e)
                else:
                    print '--f2py successfully created file: str_phps.pyd--'
                    print '--try a Python script--'
else:
    print 'os not recognized, check setup and re-run'