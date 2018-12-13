# -*- coding: utf-8 -*-
"""
===============================================================================
Hodge.WaterResources, LLC
Project Number: AD006
Project Name: FORTmod
Developed by: Matt Hodge
Type: Script

Created on Tue Oct 23 11:35:34 2018
Last Updated: 10/23/2018

Purpose: 
The purpose of this program is to learn how to make a graphical user interface
with TKinter.
Notes:
---
---
---
HWR Disclaimer:
This script was created by Hodge.WaterResources, LLC (HWR). HWR makes no warranty,
expressed or implied, as to its usefulness or correctness.
===============================================================================
"""

# import necessary modules
import pdb
import os
import sys
# import matplotlib.pyplot as plt
import matplotlib
matplotlib.use('TkAgg')
import matplotlib.pyplot as plt

import numpy as np
import Tkinter as tk
import tkMessageBox

class Application(tk.Frame):
    def is_number(self, string):
        try: 
            float(string)
            return True
        except ValueError:
            return False
        
    def input_error(self):
        tkMessageBox.showinfo("Input Error", "All inputs must be numbers. Model elements must be an integer. Please check and rerun.")
    def run_model(self):
        print "running the model"
        # check inputs
        rlen = self.r_len.get()
        rwid = self.r_wid.get()
        rq = self.r_flow.get()
        r_s = self.r_s.get()
        man_n = self.man_n.get()
        rint = self.r_int.get()
        wwq = self.ww_flow.get()
        rtmp = self.r_t0.get()
        bod_k = self.ww_bodk.get()
        rbod0 = self.r_bod0.get()
        rdo0 = self.r_do0.get()
        wwbod0 = self.ww_bod0.get()
        wwdo0 = self.ww_do0.get()

        str_flt_list = [rlen, rwid, rq, r_s, man_n, wwq, rtmp, bod_k, rbod0, rdo0, wwbod0, wwdo0]
        str_int_list = [rint]

        for item in str_flt_list:
            flt_chk = self.is_number(item)
            if flt_chk == False:
                break
        for item in str_int_list:
            int_chk = self.is_number(item)
            if int_chk == False:
                break
        if int_chk == False or flt_chk == False:
            self.input_error()
        else:
            # convert floats to float
            rlen = float(rlen)
            rwid = float(rwid)
            rq = float(rq)
            r_s = float(r_s)
            man_n = float(man_n)
            wwq = float(wwq)
            rtmp = float(rtmp)
            bod_k = float(bod_k)
            rbod0 = float(rbod0)
            rdo0 = float(rdo0)
            wwbod0 = float(wwbod0)
            wwdo0 = float(wwdo0)
            # convert ints to int
            rint = int(rint)
   
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
    
    def createRiver(self):
        # label
        self.r_head = tk.Label(self, text="River Inputs").grid(row = 0, columnspan = 2)
        # river length
        self.r_len_nm = tk.Label(self, text="length (m): ").grid(sticky = "e", row = 1, column = 0)
        self.r_len = tk.Entry(self)
        self.r_len.grid(row = 1, column = 1)
        self.r_len.insert(0, 1000000)
        # river width
        self.r_wid_nm = tk.Label(self, text="width (m): ").grid(sticky = "e", row = 2, column = 0)
        self.r_wid = tk.Entry(self)
        self.r_wid.grid(row = 2, column = 1)
        self.r_wid.insert(0, 11.21)
        # river flow
        self.r_flow_nm = tk.Label(self, text="flow (cms): ").grid(sticky = "e", row = 3, column = 0)
        self.r_flow = tk.Entry(self)
        self.r_flow.grid(row = 3, column = 1)
        self.r_flow.insert(0, 50.0)
        # river slope
        self.r_s_nm = tk.Label(self, text="slope: ").grid(sticky = "e", row = 4, column = 0)
        self.r_s = tk.Entry(self)
        self.r_s.grid(row = 4, column = 1)
        self.r_s.insert(0, 0.002)
        # Manning's n
        self.man_n_nm = tk.Label(self, text="Manning's n: ").grid(sticky = "e", row = 5, column = 0)
        self.man_n = tk.Entry(self)
        self.man_n.grid(row = 5, column = 1)
        self.man_n.insert(0, 0.04)
        # river temp
        self.r_t0_nm = tk.Label(self, text="temperature (C): ").grid(sticky = "e", row = 6, column = 0)
        self.r_t0 = tk.Entry(self)
        self.r_t0.grid(row = 6, column = 1)
        self.r_t0.insert(0, 24.2)
        # river bod
        self.r_bod0_nm = tk.Label(self, text="initial BOD (mg/L): ").grid(sticky = "e", row = 7, column = 0)
        self.r_bod0 = tk.Entry(self)
        self.r_bod0.grid(row = 7, column = 1) 
        self.r_bod0.insert(0, 6.7)
        # river do
        self.r_do0_nm = tk.Label(self, text="initial DO (mg/L): ").grid(sticky = "e", row = 8, column = 0)
        self.r_do0 = tk.Entry(self)
        self.r_do0.grid(row = 8, column = 1) 
        self.r_do0.insert(0, 8.3)
        # model elements
        self.r_int_nm = tk.Label(self, text="model elements: ").grid(sticky = "e", row = 9, column = 0)
        self.r_int = tk.Entry(self)
        self.r_int.grid(row = 9, column = 1)
        self.r_int.insert(0, 100)
        # empty row
        self.empty_nm = tk.Label(self, text="       ").grid(sticky = "e", row = 1, column = 2)
        self.empty_row_nm = tk.Label(self, text=" ").grid(row = 10, columnspan = 5)
        
    def createWWTP(self):
        # label
        self.ww_head = tk.Label(self, text="WWTP Inputs").grid(row = 0, column = 3, columnspan = 2)
        # WWTP flow
        self.ww_flow_nm = tk.Label(self, text="flow (cms): ").grid(sticky = "e", row = 1, column = 3)
        self.ww_flow = tk.Entry(self)
        self.ww_flow.grid(row = 1, column = 4)
        self.ww_flow.insert(0, 0.5)
        # WWTP bod
        self.ww_bod0_nm = tk.Label(self, text="initial BOD (mg/L): ").grid(sticky = "e", row = 2, column = 3)
        self.ww_bod0 = tk.Entry(self)
        self.ww_bod0.grid(row = 2, column = 4) 
        self.ww_bod0.insert(0, 97)
        # WWTP do
        self.ww_do0_nm = tk.Label(self, text="initial DO (mg/L): ").grid(sticky = "e", row = 3, column = 3)
        self.ww_do0 = tk.Entry(self)
        self.ww_do0.grid(row = 3, column = 4)
        self.ww_do0.insert(0, 2.7)
        # BOD k
        self.ww_bodk_nm = tk.Label(self, text="BOD decay (1/d): ").grid(sticky = "e", row = 4, column = 3)
        self.ww_bodk = tk.Entry(self)
        self.ww_bodk.grid(row = 4, column = 4)
        self.ww_bodk.insert(0, 0.607)

    def createWidgets(self):
        self.run = tk.Button(self, text = "RUN", command = self.run_model).grid(sticky = "e", row = 11, columnspan = 2)
        self.quit = tk.Button(self, text = "QUIT", command = self.quit).grid(sticky = "w", row = 11, column = 3, columnspan = 2)
    
    def __init__(self, master = None):
        tk.Frame.__init__(self, master)
        master.title("Streeter-Phelps Model: DO Sag Curve")
        if sys.platform == 'darwin':
            master.geometry('600x375')
        else:
            master.geometry('600x275')
        self.pack()
        self.createRiver()
        self.createWWTP()
        self.createWidgets()
      
# importing FORTRAN module
if __name__ == "__main__":
    # import Streeter-Phelps
    import str_phps as sp
    # start GUI
    root = tk.Tk()
    app = Application(master=root)
    app.mainloop()
    root.destroy()

