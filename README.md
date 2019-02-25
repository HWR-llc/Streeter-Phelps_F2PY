

# Streeter-Phelps Model with F2PY
In the field of water resources, many of the models that we use today were written 20 years ago or more. I call these models legacy models. It is increasingly hard to use these models because they were made for older computer systems. As operating systems advance, we have to come up with creative ways to use legacy models. Some people keep old computers (with old operating systems) on hand for just this purpose. Other people set up a virtual machine to run an operating system that is compatible with the program. Neither of these work-arounds are ideal. 

I think the best solution would be to have an updated version of the program that is compatible with current operating systems. Many of the legacy models have computational engines that were written in [Fortran](https://en.wikipedia.org/wiki/Fortran). So I set out to find a reasonable way to bring these legacy Fortran models into an easily accessible environment that is compatible with modern computers.  

You can read about what I came up with below, but if you just want to try out the code and you have all the tools you need (Git, Fortran compiler, Anaconda/Miniconda), you can use this [Quick Start](https://github.com/HWR-llc/Streeter-Phelps_F2PY#quick-start).
### Streeter-Phelps Model in Fortran
The first thing that I needed was a Fortran model to work with. So I wrote a (relatively) simple [Streeter-Phelps](https://en.wikipedia.org/wiki/Streeter%E2%80%93Phelps_equation) model in Fortran95. Streeter-Phelps predicts dissolved oxygen (DO) in a river as a function of:
 - DO at the upstream end of model,
 - DO consumption by biochemical oxygen demand (BOD),
 - reaeration from the atmosphere, and
 - distance from the upstream end of the model.

Streeter-Phelps is most commonly used to assess the impact of a wastewater treatment plant (WWTP) on a receiving water. The following figure shows a conceptual diagram of a WWTP discharging effluent to a river. This is a good visual to keep in mind as we use the Streeter-Phelps model.
![Diagram of WWTP Effluent and Streeter-Phelps](https://farm5.staticflickr.com/4813/46302667011_9419465e95_b.jpg)
I augmented this simple model by simulating open-channel hydraulics with [Manning's Equation](https://en.wikipedia.org/wiki/Manning_formula) and including the O'Connor Dobbins formulation of reaeration where the reaeration rate is a function of depth and velocity (see *Water Supply and Pollution Control* by Viessman & Hammer). 

The Fortran code has three files:
 - `str_phps_main.f95` - main program file that calls subroutines to run the model and output results
 - `str_phps_mod.f95` - module file that contains all of the subroutines and functions that makeup the model
 - `str_phps.inp` - the input file that is used when the model is run

The module file contains all of the computational components of the model, and it is what we need to access in a modern computing environment. Here is a list of important subroutines and functions.

 - `run(args)` - subroutine that runs the entire model
 - `read_inp(args)` - subroutine that reads the model input file
 - `init_geo(args)` - subroutine that initializes the geometry of the river
 - `calc_step(args)` - subroutine that calculates a solution for a single time and position
 - `save_result(args)` - subroutine that saves the model output to a text file
 - `mannings_iter(args)` - function that calculates the water depth given information about the channel
 - `mannings_v(args)` - function that calculates channel velocity given information about flow
 - `do_sat_calc(args)` - function that calculates DO saturation given water temperature
 - `reaer_calc(args)` - function that calculates the reaeration rate coefficient given information about flow
 - `diff_calc(args)` -  function that calculates the diffusivity of oxygen in water given water temperature
### F2PY
Once I had a functional Fortran program, my next step was to figure out a way to access the  Fortran code in a modern environment. There are multiple options to do this, depending on what programming language that I want to work with. I was particularly interested in doing it with [Python](https://www.python.org/) because it is [open and accessible](https://www.python.org/about/). I soon found [F2PY](https://www.numpy.org/devdocs/user/c-info.python-as-glue.html#f2py). F2PY stands for *Fortran to Python Interface Generator*. As the name indicates, it is designed for what I want to accomplish. 

When you run F2PY on Fortran code, it will read the uncompiled file and create Python [wrapper functions](https://en.wikipedia.org/wiki/Wrapper_function) for the functions in the Fortran code. Here is a brief explanation of how F2PY does its magic.
 1. F2PY either reads an existing or creates a new signature file. The signature file is a list of all subroutines and functions that are in the FORTRAN file. The signature file also indicates all of the arguments that are passed to the subroutines/functions. Note that F2PY has no way to tell what the intent (i.e., input or output) of those arguments are, but the signature file is a plain text file and you can edit it to make the final interface more Pythonic.
 2. F2PY uses the signature file to construct an [extension module](https://docs.python.org/2/extending/extending.html) that is written in [C](https://en.wikipedia.org/wiki/C_(programming_language)). C is readily integrated into Python.
 3. F2PY then compiles all relevant Fortran and C sources and links them to the final extension module. The final extension module provides an application program interface (API) for Python to interface with the original functions/subroutines. The final extension module may be a .pyd or a .so file.

Using F2PY, you are really using both Python and C to get access to the Fortran code. So there are two steps here, and it is fair to ask, why not just work with the Fortran code in C. My honest answer is that I don't know C well enough to do it this way, and I know Python. I do think Python offers some advantages over C in terms of accessibility to the casual coder. I discuss that more in the Discussion section.

We have a Fortran model, and we have a method for accessing it in a modern computing environment. In the next section, I'm going to walk through the necessary steps to use this example and run the model.

### Working with the Streeter-Phelps Model

If you want to work with the model, you can access it two different ways. You can either compile and run the Fortran code or you can use F2PY to create a Python interface. The second option is more exciting because it allows you to access all of the powerful packages that other people have written for Python (e.g., GUIs). These instructions tell you how to do both.

#### Get the Model and the Supporting Tools
There are operating system specific installation instructions below. In general, you will need three things to run the Streeter-Phelps model. You will need: the model files, a Fortran compiler, and an installation of Python. A quick note for Mac users. These instructions use Anaconda/Miniconda, and Anaconda/Miniconda require 64-bit operating systems. So if you have a 32-bit Mac, you will not be able to use this Streeter-Phelps model in Python.
##### Windows Install
For Windows operating systems, you can use a non-GNU compiler to run the FORTRAN, but since you will need a GNU-compiler for the Python interface, I'm ignoring that option. 
 1. Download the Streeter-Phelps model files from [here](https://github.com/HWR-llc/Streeter-Phelps_F2PY/releases/tag/v1.1) or you can connect to the [repo](https://github.com/HWR-llc/Streeter-Phelps_F2PY.git).
 2. Download [MingGW-w64](https://sourceforge.net/projects/mingw-w64/) (Minimalist GNU Environment for Windows))
 3. Install to `C:\mingw` (this location is not necessarily required, but it will help you stay on the same page with subsequent directions) with these options: 
   -  Architecture: x86-64
   - Threads: posix
   - Exception: seh
 4. Add MinGW to the system path 
     - **When adding MinGW, make sure you do not accidentally delete any values already in the Path.**
     - (Control Panel --> Advanced System Settings --> Environment Variables --> System Variables --> Path --> Add C:\mingw\mingw64\bin
 5. To make sure the System Path is updated, restart your computer.
 6. Open a Command Prompt and type `gfortran --verson`. You should get a response that includes: `...(x86_64-posix-seh-rev0, Built by MinGW-W64 project) 8.1.0 ...`

The next tool to get is Ananconda/Miniconda. If already have one of these you are all set (I recommend doing step 9 below to make sure what you have is working correctly). It does not matter whether you have Python 2.X or Python 3.X. Python versioning gets dealt with later in these instructions as a part of the setting up the [conda environment](https://conda.io/docs/user-guide/tasks/manage-environments.html). If you don't already have Anaconda/Miniconda, I recommend working with Miniconda Python 2.7 for the Streeter-Phelps model. The following instructions assume you make that choice.

 7. Download the [Miniconda](https://conda.io/miniconda.html) .exe installer.
 8. After it downloads, use the default installation options except for the installation location. Install the program to `C:\Miniconda2`.  
 9. A program called Anaconda Prompt will be installed on your computer and it should be in your Start menu. Double click on the program and it opens a command line window ("Anaconda Prompt").
 10. In the Anaconda Prompt, type `conda list`. You should get response that begins: `# packages in environment at C:\Anaconda2:`
 11. For troubleshooting the install, see the [detailed installation instructions](https://conda.io/docs/user-guide/install/index.html). 

If you have MingGW and Miniconda installed correctly, you are all set. You can now start working with the Streeter-Phelps model.

 ##### Linux Install
I have tested this with [Ubuntu](https://www.ubuntu.com/) LTS 16.04. If you are using a different version of Linux, some changes to the installation steps may be required, but I assume if you are running Linux that you know how to work this out on your own. The first tool you will need is a Fortran compiler. I have tested everything with [gfortran](https://gcc.gnu.org/wiki/GFortran), so I recommend that.

 1. Download the Streeter-Phelps model files from [here](https://github.com/HWR-llc/Streeter-Phelps_F2PY/releases/tag/v1.1) or you can connect to the [repo](https://github.com/HWR-llc/Streeter-Phelps_F2PY.git).
 2. Open a Terminal and type `sudo apt install gfortran`. 
 3. After the installation is complete (with default options), type `gfortran --version`. You should get a response that includes: `...(Ubuntu 5.4.0-6ubuntu1~16.04.10) 5.4.0 20160609 ...`

The next tool to get is Anaconda/Miniconda. If already have one of these you are all set (I recommend doing step 8 below to make sure what you have is working correctly). It does not matter whether you have Python 2.X or Python 3.X. Python versioning gets dealt with later in these instructions as a part of the setting up the [conda environment](https://conda.io/docs/user-guide/tasks/manage-environments.html). If you don't already have Anaconda/Miniconda, I recommend working with Miniconda Python 2.7 for the Streeter-Phelps model. The following instructions assume you made that choice.

 4. Download the [Miniconda](https://conda.io/miniconda.html) bash installer.
 5. Open a Terminal and change directories to the directory where the bash script was saved. 
 6. Type `bash Miniconda2-latest-Linux-x86_64.sh`.
 7. Accept default options until it asks if you want to `...prepend the Miniconda install location to PATH...`. Type `yes`. This will allow you to run the conda commands from your terminal.
 8. Close the active Terminal and open a new Terminal. In the new Terminal type `conda list`. You should get response that begins: `# packages in environment at /home/username/miniconda2:`
 9. For troubleshooting the install, see the [detailed installation instructions](https://conda.io/docs/user-guide/install/index.html). 

If you have gfortran and Miniconda installed correctly, you are all set. You can now start working with the Streeter-Phelps model. 

##### MacOSX Install
I have tested this with MacOSX 10.10.5 [Yosemite](https://en.wikipedia.org/wiki/OS_X_Yosemite). I already mentioned that you cannot run the Streeter-Phelps model on a 32-bit Mac computer. I also do not know if the steps listed here will work on anything earlier than Yosemite. The first tool you will need is a Fortran compiler. I have tested everything with [gfortran](https://gcc.gnu.org/wiki/GFortran), so I recommend that. In order to install gfortran, you first have to enable the [Xcode](https://en.wikipedia.org/wiki/Xcode) Command Line Tools. Note, you do not need an Apple developer account to access these tools.

 1. Download the Streeter-Phelps model files from [here](https://github.com/HWR-llc/Streeter-Phelps_F2PY/releases/tag/v1.1) or you can connect to the [repo](https://github.com/HWR-llc/Streeter-Phelps_F2PY.git).
 2. Open a terminal and type `xcode-select --install`.  After the installation is complete (with default options), you have a number of new command line tools available to you. Unfortunately, gfortran is not one of them. So we need to add that.
 3. go to this [gfortran distribution page](https://github.com/fxcoudert/gfortran-for-macOS/releases) and download the operating system appropriate version of gfortran.
 4. Double click the .dmg file. Extract the folder.
 5. In your Terminal, navigate to the folder that has the .pkg file.
 6. Type `sudo installer -pkg gfortran.pkg -target /`. This will install gfortran.
 7. type `gfortran --version`. You should get a response that includes: `...GNU Fortran (GCC) 5.2.0`

The next tool to get is Anaconda/Miniconda. If already have one of these you are all set (I recommend doing step 12 below to make sure what you have is working correctly). It does not matter whether you have Python 2.X or Python 3.X. Python versioning gets dealt with later in these instructions as a part of the setting up the [conda environment](https://conda.io/docs/user-guide/tasks/manage-environments.html). If you don't already have Anaconda/Miniconda, I recommend working with Miniconda Python 2.7 for the Streeter-Phelps model. The following instructions assume you made that choice.

 8. Download the [Miniconda](https://conda.io/miniconda.html) bash installer.
 9. Open a terminal and change directories to the directory where the bash script was downloaded. 
 10. Type `bash Miniconda2-latest-MacOSX-x86_64.sh`.
 11. Accept default options until it asks if you want to `...prepend the Miniconda install location to PATH...`. Type `yes`. This will allow you to run conda commands from your terminal.
 12. Close the active Terminal and open a new Terminal. In the new Terminal type `conda list`. You should get response that begins: `# packages in environment at /home/username/miniconda2:`
 13. For troubleshooting the install, see the [detailed installation instructions](https://conda.io/docs/user-guide/install/index.html). 

#### Running the Streeter-Phelps Model
Now that we have a Fortran compiler (gfortran) and Miniconda, we are ready to work with the Streeter-Phelps model. We are going to look at two ways to work with Streeter-Phelps model: 
 - as a compiled FORTRAN program (*the boring way*)
 - through a Python interface created with F2PY (*the exciting way*)
##### Fortran Version of the Model
Follow these steps to compile and link the FORTRAN program. Open a Terminal/Command Prompt. Navigate to the directory where you saved the model files and go to `../fortran`. When you are in that directory, type the following commands:

    gfortran -c str_phps_mod.f95 
    gfortran -c str_phps_main.f95
    gfortran -o str_phps str_phps_main.o str_phps_mod.o

Each line is a separate command (so hit return after each one!). The first two lines compile the module file and the main file, respectively. The last line links the files together creating the executable str_phps. Note, the file extension will depend on your operating system. To run the model in Windows type `str_phps.exe`, and to run the model in Linux/Mac type `./str_phps`. Your Terminal/Command Prompt will show the model results, and it will look something like this:

![Fortran Screen View](https://farm5.staticflickr.com/4847/45234508854_8f38f2e825_b.jpg)

In addition to the results being written to Terminal/Command Prompt, a text file of model results will be saved to the same directory with the file name `DO_curve.txt`. You can adjust the model inputs in the file `str_phps.inp` and re-run the model to see how changes to the model inputs influence DO downstream. 

The purpose of the previous step is to demonstrate that the Fortran code is functional, but what we really want to see is how this works within Python. 
##### Python Version of the Model
Before we can run the Streeter-Phelps model in Python, we need to set up the [environment](https://conda.io/docs/user-guide/tasks/manage-environments.html). This will ensure that you use the correct version of all the packages that work in the background, including F2PY. Open a Terminal/Anaconda Prompt. Note. for Windows users, you need to open Anaconda Prompt, not Command Prompt. Navigate to the `../python`. In that directory are three `.yml` files, one for each operating system. Then follow these two steps:
 1. Note the file name for your operating system, and type `conda env create -f OSfilename` where 'OSfilename' is replaced with the appropriate file name (e.g., `fm_win_environment.yml` for Windows). 
 2. Activate the environment you just created by typing either:
    - `activate fm_win` (for Windows)
    - `activate source fm_linux` (for Linux)
    - `activate source fm_mac` (for Mac)

Now run the script F2PY setup script by typing `python f2py_setup_check.py`. The script checks to make sure the environment is setup correctly to run F2PY. If something is wrong, it will print a message to the Terminal/Anaconda Prompt that will tell you where it found a problem. If everything is setup correctly, the script will generate the extension module you need and tell you to try a Python program!

You can now write scripts or programs to use the Streeter-Phelps extension module. The `../python` directory has one example that includes a GUI for the Streeter-Phelps model. So type `python str_phps_py_gui.py` into the Terminal/Anaconda Prompt. A window will open with a set of Streeter-Phelps model inputs. Click *RUN* to run the model and see the results. It will look something like this:

![FORTmod GUI example](https://farm5.staticflickr.com/4885/45958834141_9885f3aa94_b.jpg)

Further exploration of this simple Streeter-Phelps model is up to you. The model is not appropriate for engineering/design, but I believe it does a good job of demonstrating just how straight forward it can be to access old Fortran code in a modern computing environment. 

### Discussion
#### Problem Definition
My motivation for figuring out how to run a Fortran model in Python comes from my own professional experiences and from conversations I have had with my colleagues. Some legacy models that I know about are:

 - QUAL2E - a stream water quality model that is used/required in the permitting of National Pollutant Discharge Elimination System (NPDES) discharges.
 - STFATE - a dredged material disposal model that is required to be used in evaluating off shore disposal of dredge material
 - HELP - a landfill hydrologic model  that simulates water movement into and through a landfill
 - HEC-5Q - a planning model for reservoir systems

I am confident that there are many other models that fit my definition of legacy models. Most of these models were originally developed by state and federal agencies. They were used in projects at the time of development and in some cases they found their way into regulations. Even as new models are developed that achieve the same end goals, these legacy models remain relevant. Updates to existing projects and planning or permitting often require their use. I will also add another reason why the models are still used. **They are good models!** As an industry, we want to have access to these models, but technological changes is making that increasingly difficult.

#### Solutions Requirements
Any new methods for accessing these models requires a few things.
 - The impramatur of the original agency to any new method for accessing the model
 - Fidelity to the original model (e.g., no computation changes, an ability to replicate legacy model results)
 - Method for managing authenticity 
 - Easy distribution and access

The first two requirements call for action by the state and federal agencies that released the original program. This is a dubious solution. One or two of these models may have a champion within government who supports new development of old models, but it is unlikely that many of the models will achieve new life with this approach. The last two requirements are seemingly contradictory, or at least competing goals. They also call for a system solution that would not require management tools for each model.

#### Proposed Solution
I don't have a complete solution that meets all of the requirements stated above, but I think this Streeter-Phelps model is an example of a good starting point for a comprehesive solution. The approach used here would, if applied to legacy models, provide fidelity to the original models (i.e., same source code used) and provide easy distribution and access (i.e., accessibility on all major operating systems). This approach also provides the opportunity to quickly integrate legacy models into a much more complete programming environment. 

Imagine an open source library of Fortran source code that comes with an F2PY setup file for creating extension modules. Anyone with access to this library would be able to immediately use the models, develop user interfaces for the models, and share these developments with other users. By connecting Fortran source code to Python through F2PY, there would be a whole new set of tools that can help address the two remaining solution requirements (impramatur of original agency & method for managing authenticity).

With that concept in mind, give the Streeter-Phelps model a try and please let me know your thoughts on the concepts I've described in this discussion. 

### Quick Start
1. Connect to Repo: https://github.com/HWR-llc/Streeter-Phelps_F2PY.git
2. `cd python`
3. `conda env create -f fm_{OS}_environment.yml`
4. `conda activate fm_{OS}`
5. `python f2py_setup_check.py`
6. `python str_phps_py_gui.py`

> Written with [StackEdit](https://stackedit.io/).
