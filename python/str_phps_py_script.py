# -*- coding: utf-8 -*-
"""
===============================================================================
Hodge.WaterResources, LLC
Project Number: AD006
Project Name: FORTmod
Developed by: Matt Hodge
Type: Script

Created on Thurs Oct 18 13:30:00 2018
Last Updated: 10/18/2018

Purpose: 
The purpose of this script is to demonstrate how to use the .pyd file created
with f2py for the Streeter-Phelps model created in FORTRAN. 
Notes:
This script includes two applications of the Streeter-Phelps model. The first
application loads the same input file used in the FORTRAN program. The results 
of the model run are saved to a text file (DO_Curve.txt) and plotted using
Matplotlib. The second application uses values specified in the script to run
the Streeter-Phelps model. This application saves the results to a text file 
and plots them using Matplotlib.
HWR Disclaimer:
This script was created by Hodge.WaterResources, LLC (HWR). HWR makes no warranty,
expressed or implied, as to its usefulness or correctness.
===============================================================================
"""
# import necessary modules
import pdb
import os
import matplotlib.pyplot as plt
import numpy as np

os.chdir(r'..\fortran')
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
plt.show()

##--------Run From Scipt Input----------------
rlen = 1000000 # river length (m)
rwid = 11.21 # river width (m)
rq = 50 # river flow (m^3/s)
r_s = 0.002 # river slope (m/m)
man_n = 0.04 # Manning's n (unitless)
wwq = 0.5 # WWTP flow (m^3/s)
rtmp = 24.2 # river temperature (C) (assumed constant)
bod_k = 0.607 # BOD decay rate (1/d)
rbod0 = 6.7 # ambient BOD load in river (mg/L)
rdo0 = 8.3 # ambient DO in river (mg/L)
wwbod0 = 97 # WWTP effluent BOD load (mg/L)
wwdo0 = 2.7 # WWTP effluent DO (mg/L)
# run FORTRAN model and return results
(riv_x, riv_dos, riv_dod) = sp.str_phps_mod.run(rlen, rwid, rq, r_s, man_n, rint, wwq, rtmp, bod_k, rbod0, rdo0, wwbod0, wwdo0)
riv_do = riv_dos - riv_dod
# plot DO sag curve & DO sat
plt.plot(riv_x, riv_do, color = 'b', linewidth = 1)
plt.plot(riv_x, riv_dos, color = 'b', linestyle = '-.', linewidth = .5)
plt.xlabel('Distance from WWTP (m)')
plt.ylabel('DO (mg/L)')
plt.title('FORTmod DO Sag Curve')
plt.show()
