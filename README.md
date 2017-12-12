QuickHash
=========

Compiling the Project
---------------------

Binaries for Windows, Linux and Apple macOS are available from http://quickhash-gui.org.
But if you need or want to compile yourself, then first clone the source code: `git clone https://github.com/tedsmith/quickhash.git`

There is an LPR file that is the Lazarus Project File. So you need the Lazarus IDE and Freepascal Compiler (v3.0 or above) for your chosen platform, available from www.lazarus-ide.org. 

After installation of Lazarus and Freepascal, choose "Open Project" and navigate to the folder where you cloned QuickHash.
Lazarus looks for LPI files by default (local config file for a project) but there isn't one in the GitHub project.
Simply adjust the drop down menu for file type (bottom right) to "All files", and then select the LPR file.
Lazarus will then warn you that a project session file is missing and would you like to create one.
Choose "Yes" and then just click OK in the next window (the one that asks what type of project you are making - it should default to 'Application').
After clicking OK for the last time, a local LPI file will be created for your computer session. 

Lazarus will then complain about some missing packages, unless they happen to be installed in your IDE already. HashLib4Pascal, DateTimePicker, 'CSVExporter' (which is part of the Lazarus 'lazdbexport' package) and potentially SQLdbLaz. 
So you need to install those. Here is how.  

HashLib4Pascal package: The library is included in the GitHub QuickHash project. So simply choose "Package", then "Open Package File (lpk)" from the top menu of Lazarus.
Choose and navigate to `CloneOfQuickHashProject/HashLib4Pascal/HashLib/src/Packages/FPC/HashLib4PascalPackage.lpk` then click the 'Compile' button.
Then use the next button to the right called 'Use >>' and click 'Add to Project' from the drop-down menu. HashLib4Pascal is now added to your QuickHash project.

DateTimePicker : Repeat the same process for the package [DateTimePicker](http://wiki.lazarus.freepascal.org/ZVDateTimeControls_Package) which was added in v2.8.3 to enable scheduled hashing.
So simply choose 'Package --> Open Package File (lpk)' from the top menu of Lazarus and navigate to `CloneOfQuickHashProject/DateTimePicker/zvdatetimectrls.lpk`, choose "Compile" and then click 'Use >> Add to Project'.
Better yet, choose "Install" next, so that it becomes a component of your Lazarus IDE, which will further reduce warnings and prevent the project bugging you. 

LazDBExport : Again, in Lazarus, go to Package --> Install\Uninstall Packages --> find 'lazdbexport' 1.0' in the right hand pane under 'Available for installation'. Select the package and click 'Install Selection' button. 
Click 'Save and Rebuild IDE'. The next window will show a list of installed packages and the ones to be installed. Click 'Continue'. The IDE will rebuild.

SQLdb Tab : You MAY also need to ensure you have the 'SQLdb' tab in your Lazarus IDE interface. It is important since v3.0.0 because it uses the 'TSQLDBLibraryLoader', which is used to try and enable QuickHash to load the default SQLite library on your system. 
It is possible your IDE will not have this tab (most do, but I've experienced it not to be there). To get it, ensure the SQLdbLaz package is installed, again via 'Tools --> Install\Uninstall Packages', find SQLdbLaz in the right and add it to the "To be installed" pane.

Lazarus will re-launch after you hit the button to rebuild IDE and probably re-open the QH project, hopefully now without errors. 

Now save your project (Project --> Save Project) which will create a new LPI file. Then you can compile QuickHash yourself using Lazarus. 

I am hopeful this guide might encourage collaborators and also help various Linux distributors include QuickHash into their package management platforms. 

Ted Smith
