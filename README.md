QuickHash
=========

Compiling the Project
---------------------

Binaries for Windows, Linux and Apple macOS are available from http://quickhash-gui.org.
But if you need or want to compile yourself, then first clone the source code: `git clone https://github.com/tedsmith/quickhash.git`

There is an LPR file that is the Lazarus Project File. So you need the Lazarus IDE and Freepascal Compiler (v3.0 or above) for your chosen platform, available from www.lazarus-ide.org. 

After installation of Lazarus and Freepascal, you need to set some things up.

HashLib4Pascal package: The library is included in the GitHub QuickHash project. So simply choose "Package", then "Open Package File (lpk)" from the top menu of Lazarus.
Choose and navigate to `CloneOfQuickHashProject/HashLib4Pascal/HashLib/src/Packages/FPC/HashLib4PascalPackage.lpk` then click the 'Compile' button. Then later, when you open the QuickHash project, you need to add it to the project, but that will be explained later.

DateTimePicker : Repeat the same process for the package [DateTimePicker](http://wiki.lazarus.freepascal.org/ZVDateTimeControls_Package) which was added in v2.8.3 to enable scheduled hashing.
So simply choose 'Package --> Open Package File (lpk)' from the top menu of Lazarus and navigate to `CloneOfQuickHashProject/DateTimePicker/zvdatetimectrls.lpk`, choose "Compile" and then Install to ensure it is part of the IDE.

LazDBExport : in Lazarus, go to Package --> Install\Uninstall Packages --> find 'lazdbexport' 1.0' in the right hand pane under 'Available for installation'. Select the package and click 'Install Selection' button.
Click 'Save and Rebuild IDE'. The next window will show a list of installed packages and the ones to be installed. Click 'Continue'. The IDE will rebuild.

SQLdb Tab : You MAY also need to ensure you have the 'SQLdb' tab in your Lazarus IDE interface. It is important since v3.0.0 because it uses the 'TSQLDBLibraryLoader', which is used to try and enable QuickHash to load the default SQLite library on your system. It is possible your IDE will not have this tab (most do, but I've experienced it not to be there). To get it, ensure the SQLdbLaz package is installed, again via 'Tools --> Install\Uninstall Packages', find SQLdbLaz in the right and add it to the "To be installed" pane.

Now you can open the QuickHash project. Choose Project --> Open Project". Lazarus looks for LPI files by default (local config file for a project) but there isn't one in the GitHub project. Simply adjust the drop down menu for file type (bottom right) to "All files", and then select the LPR file. Lazarus will then warn you that a project session file is missing and would you like to create one.
Choose "Yes" and then just click OK in the next window (the one that asks what type of project you are making - it should default to 'Application'). After clicking OK for the last time, a local LPI file will be created for your computer session.

Assuming it opens OK, when you go to compile it, Lazarus will report that the project can not find certain packages that are declared in the uses clause. So use the "Package" menu again to ensure it knows where these units are. So choose "Open loaded package", and for "HashLib4Pascal" package and "LazDBExport" package and "ZVDateTimeCtrls" package, select each one (one at a time) and choose "open". Once open in the package manager, choose "Use --> Add to Project". Now when you compile, it should find all the units and compile OK. You may also need to do this for SQLDb package.

Now save your project (Project --> Save Project) which will create a new LPI file. Then you can compile QuickHash yourself using Lazarus.

I am hopeful this guide might encourage collaborators and also help various Linux distributors include QuickHash into their package management platforms. 

Ted Smith
