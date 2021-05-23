QuickHash<br>
=========<br>

Quickhash-GUI is a cross platform, free, open-source data hashing tool to text strings and text files, a single file, a forensic E01 image, a folder of files, and disks. It can also be used to compare two files, compare two folders, copy folders of files with hashing, and do some Base64 hashing. 
It ships with some dependancies so if everything is unzipped properly and to the same folder, the pre-compiled binary program 
should work on all three platforms, perhaps with some security permissions being required or being adjusted to execute.<br>

For Windows, it comes as an exe with some DLLs. For Apple OSX it comes as a .app. For Linux it comes as a Debian package. <br>

For those wishing to compile the program themselves, from source, instructions to do so are below, along with instructions on compiling the dependant files from their respective source libraries too. <br>

Compiling the Project<br>
---------------------<br>

Pre-compiled binaries for Windows, Linux and Apple macOS are available from http://quickhash-gui.org.<br>

If you need, or want, to compile it yourself, then first clone the source code using git : `git clone https://github.com/tedsmith/quickhash.git`<br>

There is an LPR file that is the Lazarus Project File. So you need the Lazarus IDE and Freepascal Compiler (fpc v3.0.4 and Lazarus v1.8.4 or above) for your chosen platform, available from www.lazarus-ide.org. <br>

After installation of Lazarus and Freepascal, you need to set some things up.<br>

HashLib4Pascal package: The library is included in the GitHub QuickHash project but more ideally it is within the Lazarus IDE via the Online Package Manager (ideally use that). If you want to use the shipped package, simply choose "Package" from the main Lazarus IDE menu, then "Open Package File (lpk)".<br>
Choose and navigate to `CloneOfQuickHashProject/HashLib4Pascal/HashLib/src/Packages/FPC/HashLib4PascalPackage.lpk` then click the 'Compile' button. Then later, when you open the QuickHash project, you need to add it to the project via the Dependancies section of the Object Inspector, but that will be explained later.<br>

DateTimePicker : Repeat the same process for the package [DateTimePicker](http://wiki.lazarus.freepascal.org/ZVDateTimeControls_Package) which was added in v2.8.3 to enable scheduled hashing.<br>
So again simply choose 'Package --> Open Package File (lpk)' from the top menu of Lazarus and navigate to `CloneOfQuickHashProject/DateTimePicker/zvdatetimectrls.lpk`, choose "Compile" and then Install to ensure it is part of the IDE.<br>

LazDBExport : in Lazarus, go to Package --> Install\Uninstall Packages --> find 'lazdbexport' 1.0' in the right hand pane under 'Available for installation'. Select the package and click 'Install Selection' button.<br>
Click 'Save and Rebuild IDE'. The next window will show a list of installed packages and the ones to be installed. Click 'Continue'. The IDE will rebuild, although since v3.3.0, this may not be required for much longer, if at all.<br>

SQLdb Tab : You MAY also need to ensure you have the 'SQLdb' tab in your Lazarus IDE interface. It is important since v3.0.0 because it uses the 'TSQLDBLibraryLoader', which is used to try and enable QuickHash to load the default SQLite library on your system. It is possible your IDE will not have this tab (most do, but I've experienced it not to be there). To get it, ensure the SQLdbLaz package is installed, again via 'Tools --> Install\Uninstall Packages', find SQLdbLaz in the right and add it to the "To be installed" pane.<br>

Windows developers also need to ensure they  have the two SQLite DLL files in the root of the project folder, otherwise when they compile an exe it will not find the DLLs it needs to build the tables. SQLite DLL's for 32 and 64 bit systems are part of the main download from www.quickhash-gui.org if you want to get them as pre-compiled. Or you can compile them from source, yourself (instructions are below)<br> 

Now you can open the QuickHash project itself. Choose "Project --> Open Project". Lazarus looks for LPI files by default (local config file for a project) but there often is not one in the GitHub project, although I do often include when just as a guide. If there is not one, or if the one supplied does not work, simply adjust the drop down menu for file type (bottom right) to "All files", and then select the LPR file instead. Lazarus will then warn you that a project session file is missing and would you like to create one.
Choose "Yes" and then just click OK in the next window (the one that asks what type of project you are making - it should default to 'Application'). After clicking OK for the last time, a local LPI file will be created for your computer session.<br>

Assuming it opens OK, when you go to compile it, Lazarus may report that the project can not find certain packages that are declared in the uses clauses. So use the "Package" menu again to ensure it knows where these units are. So choose "Open loaded package", and for "HashLib4Pascal" package and "LazDBExport" package and "ZVDateTimeCtrls" package, select each one (one at a time) and choose "open". Once open in the package manager, choose "Use --> Add to Project". Now when you compile, it should find all the units and compile OK. You may also need to do this for SQLDb package.<br>

Now save your project (Project --> Save Project) which will create a new LPI file. Then you can compile QuickHash yourself using Lazarus, but it will not run properly without the SQLite DLLs (on Windows at least).<br>

For Windows users wishing to compile SQLite DLL's themselves, you can do so by: <br>

  Downloading and installing the MinGW compiler (for 32-bit) and the  MINGW64 Compiler for Windows, <br>
  Downloading the SQLite "Amalgamation files" from here : https://sqlite.org/download.html<br>
  And finally by using the following syntax in either MinGW or MINGW64 :<br>

  gcc -shared sqlite3.c -o sqlite3.dll<br>

  (https://sqlite.org/howtocompile.html)<br>
  
  or you can download the latest compiled version of Quickhash that includes the DLL's already from www.quickhash-gui.org <br> 
  
For Linux users wishing to compile SQLite SO.0 files themselves, you can also do so by: <br>  

  Following the steps for Windows users as above, but then running the following compiler code: <br>
  gcc -shared -o libsqlite3.so.0 -fPIC sqlite3.c <br>

For OSX users wishing to compile SQLite libsqlite3.dylib files themselves, I assume the process is as for Linux but I have never done it (disclaimer) : <br>  
    Following the steps for Windows users as above, but then running the following compiler code: <br>
    gcc -shared -o libsqlite3.dylib -fPIC sqlite3.c <br>
  
I am hopeful this guide might encourage collaborators and also help various Linux distributors include QuickHash into their package management platforms. <br>
