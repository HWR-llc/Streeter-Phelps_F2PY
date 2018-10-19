# -*- coding: utf-8 -*-
"""
===============================================================================
Hodge.WaterResources, LLC
Project Number: AD006
Project Name: FORTmod
Developed by: Matt Hodge
Type: Program

Created on Thurs Oct 18 14:00:00 2018
Last Updated: 10/18/2018

Purpose: 
The purpose of this program is to demonstrate how to use the .pyd file created
with f2py for the Streeter-Phelps model created in FORTRAN. 
Notes:
This program includes one application of the Streeter-Phelps model. The 
application loads the same input file used in the FORTRAN program. The results 
of the model run are saved to a text file (DO_Curve.txt) and saved to a jpg.
HWR Disclaimer:
This script was created by Hodge.WaterResources, LLC (HWR). HWR makes no warranty,
expressed or implied, as to its usefulness or correctness.
===============================================================================
"""
# import necessary modules
import pdb
import os
import sys
import matplotlib.pyplot as plt
import numpy as np

if __name__ == "__main__":
    # import Streeter-Phelps
    os.chdir(r'..\fortran')
    sys.path.append(os.getcwd())
    import str_phps as sp
    ##--------Run From Input File-----------------
    # input file
    file_path = 'str_phps.inp'
    # load input file
    (rlen, rwid, rq, r_s, man_n, rint, wwq, rtmp, bod_k, rbod0, rdo0, wwbod0, wwdo0) = sp.str_phps_mod.read_inp(file_path)
    # change to python directory
    os.chdir(r'..\python')
    # run FORTRAN model and return results
    (riv_x, riv_dos, riv_dod) = sp.str_phps_mod.run(rlen, rwid, rq, r_s, man_n, rint, wwq, rtmp, bod_k, rbod0, rdo0, wwbod0, wwdo0)
    riv_do = riv_dos - riv_dod
    # plot DO sag curve & DO sat
    plt.plot(riv_x, riv_do, color = 'b', linewidth = 1)
    plt.plot(riv_x, riv_dos, color = 'b', linestyle = '-.', linewidth = .5)
    plt.xlabel('Distance from WWTP (m)')
    plt.ylabel('DO (mg/L)')
    plt.title('FORTmod DO Sag Curve')
    plt.savefig('str-phps_SagCurve.png')
    plt.show()