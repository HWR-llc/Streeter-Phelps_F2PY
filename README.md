# FORTmod
In the field of water resources, many of the models that we use today were written 20 years ago or more (legacy models). It is increasingly hard to use these models because they were made for older computer systems. 16-bit and 32-bit computer programs do not run on 64-bit operating systems. Some engineers keep old computers (with old operating systems) on hand for just this purpose. Others set up a virtual machine to run an operating system that is compatible with the program. Neither of these work-arounds are ideal. Many of the legacy models had computational engines that were written in [FORTRAN](https://en.wikipedia.org/wiki/Fortran). We set out to find a way to bring these legacy FORTRAN models into an environment that is compatible with modern computers.  

FORTmod is an example of how a legacy FORTRAN model can be made to work with [Python](https://en.wikipedia.org/wiki/Python_(programming_language)). We wrote a [Streeter-Phelps](https://en.wikipedia.org/wiki/Streeter%E2%80%93Phelps_equation) river dissolved oxygen (DO) model in FORTRAN, and then w used [f2py](https://www.numpy.org/devdocs/user/c-info.python-as-glue.html#f2py) to create a Python interface for the model. The interface allows the user to run the FORTRAN Streeter-Phelps model within Python. Below are installation instructions to use both the FORTRAN version of our simple model and the Python version of the model.

FORTmod is just a proof of concept. We are actively looking for a regulatory or analysis model in the field of water resources to apply this approach. In order to do this, we will need to access to the original FORTRAN files (and any other coding used to develop a GUI). We have put some details on this [wiki page](link) that are relevant to applying this approach to an existing FORTRAN model.

## Installation
In order to use FORTmod, the user needs three things.
 1. FORTmod (a release or fork)
 2. a FORTRAN compiler
 3. Python Distribution (2.7 including latest versions of Numpy and Matplotlib)

There are lots of ways to make FORTmod work, but FORTmod depends on many computer specific details. The following set of instructions is not a comprehensive description of installation options. It is just one example of how FORTmod can be used. The steps described below work on a **Windows 10 64-bit operating system**. As we test out installation on different operating systems, we will expand these installation instructions. 

### FORTmod
Download the latest release from the Github page.

### FORTRAN Compiler
We used gfortran, but gfortran compiles in a GNU environment (i.e., Linux not Windows). So in order to use gfortran, we have used MinGW-w64 (Minimalist GNU Environment for Windows).
 - Download MingGW-w64
 - Install to C:\mingw with:
	 -  Architecture: x86-64
	 - Threads: posix
	 - Exception: seh
 - After installation, add mingw to the system path (Control Panel --> Advanced System Settings --> Environment Variables --> System Variables --> Path --> Add C:\mingw\mingw64\bin
 - **When adding mingw, make sure you do not accidentally delete any values already in the Path.**

### Python Distribution

Anaconda is a Python distribution that includes NumPy and Matplotlib. We recommend using Anaconda. Regardless of Python distribution, make sure it has NumPy (1.14.3 or higher) and Matplotlib (2.2.2) packages installed.

 - Download Anaconda
 - Install to C:\Anaconda2
 - You can add the path to Anaconda2 to your System Path as well, but we don't recommend that if you already have another distribution of Python installed on your computer (e.g., ArcGIS installs a Python distribution)
## Running FORTmod
The [Streeter-Phelps model (equation)](https://en.wikipedia.org/wiki/Streeter%E2%80%93Phelps_equation) calculates dissolved oxygen along the length of river based on the initial concentrations of DO and biochemical oxygen demand (BOD), the rate at which the BOD consumes DO, the rate at which oxygen enters the river, and the distance/time from the initial concentrations. FORTmod is a simple model that simulates the effluent discharge from a wastewater treatment plant (WWTP), and looks at how DO in the river changes as a function of distance from the discharge point. The model includes some model inputs, including:
 - Ambient River Conditions
	 - Flow
	 - Concentrations of DO/BOD
	 - temperature (assumed constant)
	 - Width (assumed rectangular channel)
	 - Slope (Manning's equation for velocity calculation)  
 - Effluent Conditions
	 - Flow
	 - Concentrations of DO/BOD
	 - BOD decay rate
 - Model Limits
	 - Total Length
	 - Model Calculation Step (distance)

The following figure shows a diagram of the model setup.
![Streeter-Phelps Diagram](https://1drv.ms/u/s!AkzKzpQT4t9wgY0pe6RJSFt3li4V_Q)

The user can run FORTmod in three different ways:
 1. As a FORTRAN executable
 2. As a Python script
 3. As a Python program

The first option is not relevant to the purpose of the FORTmod. It just demonstrates that FORTmod is functional FORTRAN code. It also provide an opportunity to discuss the structure of the FORTRAN code. The second and third options are more interesting because they are examples of how the FORTRAN code can be accessed through Python.

### FORTRAN Executable
FORTmod includes two .f95 files and one .inp file in the fortran directory. 

 - str_phps_mod.f95 is a module that includes all of the subroutines and functions necessary to run the Streeter-Phelps model. 
 - str_phps_main.f95 is the program file that calls the modules and executes the model run. 
 - str_phps.inp is a text file with a set of model inputs used in the model.

Before the executable can be built, the individuals files must be compiled. Open a command line window and navigate to `../Fortmod/fortran`. Type the following commands:

    gfortran -c str_phps_mod.f95 
    gfortran -c str_phps_main.f95
    gfortran -o str_phps str_phps_main.o str_phps_mod.o

 The first two lines compile each file and the last line, links the files together creating the executable str_phps.exe. Now you can run the program by typing `str_phps.exe`. The program will run and print model results and generate a model results file called `DO_curve.txt`.  The returned results are dissolved oxygen concentrations along the length of the river downstream of the WWTP. 
 
Now that the program runs, you can adjust the model inputs by editing the values in `str_phps.inp` and look at how changes to ambient conditions or WWTP operations will impact water quality in the river. 

## Python Script
The executable version of the model is instructive, but the real purpose of FORTmod is to show how the FORTRAN model can be brought into Python. We can do that now by using f2py. In the same command window (and in the same directory) type:

    python C:\Anaconda2\Scripts\f2py.py -c --compiler=mingw32 --fcompiler=gnu95 -m str_phps str_phps_mod.f95
Note that we are only using the module file and not the program file. This gives us direct access to the subroutines and functions in the model. This command generates the file `str_phps.pyd`. A .pyd file is a dynamic link library that contains a Python module  composed of the subroutines and functions in the FORTRAN module file. 
If we open the file `..\FORTmod\python\str_phps_py_script.py`, we see a script that uses the str_phps module. The script runs two instances of the model. The first instance uses the same `str_phps.inp` input file from the FORTRAN executable. The second instance  specifies model inputs within the script. When the script is run, both instances will run the model, output a model results file and will graph the model results using Matplotlib. 
Again, the user can change model inputs (either in the text file or in the script) and see how the changes to the model inputs change model outputs. 

## Python Program
The last way to use FORTmod is through a Python program. The program uses the same `str_phps.pyd`.  To run the program, return to the command window and navigate to `..\FORTmod\python` and type `python str_phps_py_program.py`. The program reads the same `str_phps.inp` input file, runs the model, outputs the same model results file, generates a graph of model results and saves the model results graph to the file `str_phps_SagCurve.png`.
Again, the user can change model inputs (either in the text file or in the script) and see how the changes to the model inputs change model outputs. 

> Written with [StackEdit](https://stackedit.io/).