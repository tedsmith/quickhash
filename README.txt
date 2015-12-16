V2.6.5

At user request, the "Text" tab now allows line-by-line hashing of each line. The results are saved to a comma separated text file that can be opened in a text file editor or spreadsheet software.

For example, Google Adwords requires SHA256 lowercase hashes of customer e-mail addresses. So with QuickHash, you can easily paste your list of addresses into the text field, click the "Hash Line-By-Line" button and the output is saved as CSV output for you, ready for use with Google Adwords or any similar product line (https://support.google.com/adwords/answer/6276125?hl=en-GB). Tested with data sets of the low tens of thousands. Would be interested to hear how it copes with larger volumes of data. 

V2.6.4-a Dec 2015 Bug #16 (https://sourceforge.net/p/quickhash/tickets/16/) highlighted an issues with the “Don't rebuild path' option of the “Copy” tab wherein the copy failed. This was tracked back to v2.6.3 when the new treeview feature was added, replacing the former button path selection functionality. The bug was caused as a result to a path parameter that no longer existed. That was fixed. 

V2.6.4 Nov 2015

QuickHash can now READ and WRITE from and to folders that exceed the MAX_PATH limit of MS Windows, which is 260 characters. A limit of 32K is now adhered to with QuickHash 2.6.4, meaning files may be found on filesystems that were put there by software that is able to bypass the MAX_PATH limit even if regular software like Windows Explorer is unaware of their existence. 

“UNC Mode” added to the “Copy” tab, specifically to enable the user to operate in pure UNC mode but with the new 32K path length limits. Useful for comparing data on multiple network nodes that may not be mapped as a drive letter. Windows only feature (not needed for Linux and Apple Mac). 

The tree view in the copy tab are now sorted alphabetically. 

The “Choose file types” option that has existed in the “Copy” tab for a while has now been added to the “Files” tab by user request. Meaning the user can now recursively hash a folder and sub-folder of files but choose which files to include and which to include. Extension basis only and not file type signature analysis. 

Further GUI anchoring improvements, to make the program display elements better when maximised, with less overlapping hopefully. 

Some historic error messages updated and improved, and made more OS specific.  

User manual updated and revised for v2.6.4

Some other minor improvements

V2.6.3 – Sept 2015

NEW: Replaced two buttons with two treeview panes in the 'Copy' tab. Left pane is for the user to choose where to copy files FROM. Right pane is for the user to choose where to copy files TO. On appropriate selection, the user needs just press 'Go' and on completion a new form shows the results. 

FIX: In the 'Compare Directories' tab, the save button will now also save the hash comparison result to the log file, i.e. did the comparison match or not? And how many files were counted in grids A and B (feature request #20 http://sourceforge.net/p/quickhash/feature-requests/20/). 

FIX: In the 'Compare Directories' tab, the file counts of the grids and difference counts were overlapping with the labels when high file counts were examined (tens of thousands upwards). Fixed by anchoring the elements. 


V2.6.2.b – August 2015 – Linux only

The exclusion of files that were zero bytes (functionality that was introduced in v2.1 back in 2013) meant that block devices in Linux, like /dev/sda or /dev/sda1, were simply ignored if selected by the user and not hashed. A new compiler directive added to ensure that if the file is reported as zero byte that a secondary check is then done to see if its a block device in Linux. If so, it will be hashed providing QuickHash is ran as root or sudo.  

V2.6.2 – August 2015

As per feature request #15, and in part request #7, added an 'Expected Hash Value' field to “Text” and “File” tabs to enable the user to paste an already computed hash value (perhaps from another tool, e-mail, webpage) into QuickHash. If the field contains anything other than three dots, once the data hash is generated in QuickHash, it will compare it against the expected hash in this field and report match or mis-match. 

Corrected the fact that that the values for “Total Files in Dir A” and “Dir B” in the comparison of two directories, were the wrong way round. 

Updated copyright date range in the form captions for both the disk hashing module and QuickHash itself

Minor GUI improvements like anchoring.

User manual updated 

V2.6.1 – 31/03/15

Added two buttons for copying the grid content of “Compare Directories” to the clipboard, to enable the user to simply paste the results of one or both grids to another tool like Excel, Notepad etc. See ticket ref #9 (https://sourceforge.net/p/quickhash/feature-requests/8/)

Added a “Save to Files” button in the same tab to allow the content of grids A and B to be saved as two seperate CSV files (one for each grid) and a single combined HTML file (with the content of table A displayed in one table, the content of table B displayed in the other). 

Throughout all of Quickhash, a line is automatically inserted into both CSV and HTML output stating the name and version of QuickHash used and the date the log file was generated. See ticket ref 7 (https://sourceforge.net/p/quickhash/feature-requests/7/) 

Fixed the truncation of “Total Files in DirA” and “Total Files in DirB” in Compare Directories tab, where counts more than 99 (i.e. 100+) were being truncated. So 150 files was being written as “15”. Note this only affected the user display – not the log or display grid. 

Ensured that if the user re-runs a comparison of two directories using the “Compare Directories” tab, any values from the previous comparisons are cleared, such as the values in the display grids, the time ended, the hash match status, etc. Prior to 2.6.1, once a scan had been conducted, the display was not updated until the second scan had finished, as opposed to clearing it at the start of the subsequent scan. 

Added a clickable link to the QuickHash projects homepage at sourceforge. 

V2.6.0

New tab added titled 'Compare Two Files' to allow the user to check if two files in two different places (folders) are identical, or not, without having to hash all the other files in those respective folders. For example, C:\Data\FileA.doc and C:\BackupFiles\FileA.doc

Fixed column mis-alignment for HTML output of the “FileS” tab; the mis-alignment was caused by the seperation of file name and file path into two different columns in v2.5.2. where the seperation in the grid was not carried forward to the HTML output.  

Added the ability to delete duplicate files where found, if the user chooses to detect duplicate files only. 

Further hints corrected in 'Copy' tab. 

Manual updated to incorporate changes brought in versions 2.5.3 and 2.6.0

v2.5.3

Further features to try and help users who have a small screen or have set a very low screen resolution. QuickHash will now detect the users screen settings, and, if they are smaller than the default size of QuickHash, QuickHash will be scaled down at the top and the left to that resolution high and wide, less 50 pixels, to be on the safe side. That will, at least, enable such users to get some, if not all, of the functionality from QuickHash and enable them to move it around the screen etc. Wher as before, QuickHash would load bigger than the users screen (if they used a small resolution) preventing therm from being able to drag it and resize it. 

Added the ability to move data to very long folders where the total length of the reconstructed folder might exceeded the maximum allowed length of a folder (as dictated by Microsoft Windows, not NTFS) of 260 characters. Not that it only allows the copying of files TO a folder with a length > 260. If the source folder is itself longer than that, the files in those longer folders will not be found yet (will add the ability to do so in later versions). 

Several hints on various buttons and labels corrected to show informative instruction.

The file type mask told users to separate extensions with a space, when no space is needed. In fact, adding a space might case file types not to be found.   

The “Disks” tab was made accessible in the Linux version, but the button disabled and a descriptor to users to just use the “File” tab instead, because users were confused thinking they could use the tab on the Linux platform but they were unsure why it was greyed out. 

When hashing individual files in the “File” tab, if the user single clicked a file, but then clicked 'Cancel', the file was still being passed to the hashing procedures. That was fixed so that if the user cancels, the file is not hashed. 

v2.5.2 October 2014

For the Windows version only : Implemented a scheduler for disk hashing, allowing the user the ability to schedule a start time for their chosen disk. Useful, for example, if a disk is currently being used or examined with an estimated completion time of 2 hours which is after the examining user may have gone home and unable to start the disk hashing process. Now, the user can specify a start date and time that is two or 3 hours after the estimated end time of the other task, and QuickHash will then commence hashing automatically without the need for the user to start it. If no valid start time is entered, the program starts hashing as soon as the chosen disk is double clicked, as normal. 

For all versions : At user request, added an additional column to “FileS” tab to seperate the path from the filename. So now the FileName column contains just the filename. And the new 'Path' column contains the files path.  

Added an option in “Copy” tab called “Don't rebuild path?”. If checked, the files in the source directory and all sub-directories will simply be dumped into the root of the destination directory without having the original path rebuilt. Any files with the same name will be appended with 'Filename.ext_DuplicatedFileNameX'. 

Changed progress status labelling to look neater and more compact. 


v2.5.1 September 2014

The new dynamic text hashing worked fine - new hashes appeared as the user typed, but if the user then chose a different hash algorithm, without changing the text, users felt it would be better for the hash to update dynamically too. So that was applied.

When you clicked in the text area, it was always cleared automatically, for convenience. However, users felt it might be better to only clear the default standing text on entering the text field, rather than always clearing it. So now it only clears it if the default standing text is in the box. After that, it only clears the box if the user consciously clicks the "Clear Text Box" button. This allows the user to add text, then add some more text a few minutes later without losing what they had first.

Drag and drop functionality added for SINGLE FILES in the 'File' tab. So users can now simply drag their file onto QuickHash. Switching the hash algorithm choice in that same tab will dynamically update the hash, as seen with the new text hashing changes reported above. And it will switch the user to that tab, if they do a drag and drop from another tab. Support for folder based drag and drop will not be added. 

Adjusted the 'Started at:' value in 'File' tab from just the time to date and time, to account for large files that may exceed 24 hours to hash. 

All hash value strings assigned as ansistrings. Not strictly necessary as SHA512 as hex is 128 characters, but future algorithms may exceed that. 

Added an advisory to ensure users run QuickHash as administrator for hashing disks and that Windows 8 users might wish to consider other options due to a lack of testing on that rather unpredictable platform. In tests, unexpected read errors were reported on Windows 8. 

v2.5.0 - September 2014

New tab added : 'Hash Compare Directories'. Choose one directory, then choose another directory, and QuickHash will compare one against the other based on 
the number of files and the hashes of all of those files. If both the file count and all the file hashes match, you can be sure that DirB is an exact copy of 
the files in DirA. This does not mean that the directory STRUCTURE is exactly the same - only that the files in those directories are the same.

Adjusted text hashing to dynamic output - as you type in the text field, the hash is recomputed. No need to press a button anymore.

The 'Text Hashing' tab, when given lots of data, is now better able to accomodate more data and compute correct hashes without overflow - many Kb is feasible up to a reasonable limit.

Created more meaningful mouseover hints to each tab, to help users understand what each does. Also renamed them for easier understanding. i.e. 'File' tab, for hashing files. 'FileS' for hashing multiple files. 'Disks' for hashing disks. And so on. 

Assembly coded versions of MD5Transform and SHA1Transform incorporated into source code but NOT the program to allow for more testing. When implemented, tests show that they accelerates QuickHash to one of the fastest (maybe THE fastest?) hashing utility in the world. 1Gb file in 4-6 seconds, hardware permitting. However, portability and CPU architectures need to be better considered before release. 

v2.4.2 - July 2014

Adjusted interface to make it better on small screens like notebook computers.
Removed a message dialog that appeared when there was an error. Instead, QH will continue when an error is enountered but warn you at the end about the error, instead.  

v2.4.1 July 2014

Switched the SHA-1 file hashing functionality to the same transform function as used in the disk hashing module, for speed increases. Meaning QuickHash will compute the hashes of files around 40% faster than in any earlier version.

Customised versions of SHA1 library merged into one unit (called 'sha1customised') that incorporate both the fixes for Unicode file handling 
and the faster transform routines introduced in the disk hashing module, that are now needed for both disks and files. In v2.4.0, there were two seperate customised SHA1 units which made life confusing. 

Entire process repeated for MD5, too. It too has it's own customised unit and seems to be around 3 times faster!! 

Start Times and End Times provided as a pair, making them more useful and where possible computing the time actually taken to do the task. 

Fixed status bar - the status bar in 'File Hashing' was being populated by 'Hash, Copy, Hash' processes instead of just the 'File Hashing' progress tab. The status bar in 'Hash, Copy, Hash' was not being populated. That was fixed. 

Redundant Unit1 code (applied to versions prior to v2.0) removed. 

v2.4.0 July 2014

After several years of trying, the functionality to hash physical disks in Windows is now part of QuickHash. It has been implemented by means of a seperate self-contained module that is launched on press of a button in the fourth tabsheet titled "Disk Hashing (for Windows)". The Linux version does not need this tab or this module so neither are available to Linux users. Linux users have always had the option of hashing disks with QuickHash by running it as root or sudo and using the "Hash File" tabsheet and navigating to /dev/hdX or /dev/sdaX or whatever. Note SHA1 only, for now. Others will follow in X.X.X sub releases, e.g. 2.4.1. Speeds are fast - approx 3.5Gb per minute via Firewire800 and up to 8Gb per minute with direct SATA connection. 

Some redundant unused variables removed to optimise memory usage

Some minor improvements to the interface - a few buttons moved around, extra hints added etc
  
v2.3 June 2014

Complete support for Unicode on Windows, ensuring filenames or directotries containing Chinese or Arabic or Hebrew (etc) characters can now be processed using QuickHash without the user having to change their language and region settings. Prior to this, QuickHash was generating the default initialisation hashes for such files but not actually hashing them. All Windows users are encouraged to discard any version prior to v2.2 and adopt v2.3. 

v2.2 - Nov 2013

It was reported that large files failed to hash properly with SHA256 or SHA512 implementation. It turned out this was due to a 32-bit integer delcaration in the DCPCrypt  library that is used by QuickHash for those two algorithms. Updated by using QWord instead Longword variables. Output checked against SHA256SUM and SHA512SUM and found to be OK now. 

Linux version brought to same level as Windows version. Interface improved to better display values.  

v2.1 - June 2013

All versions prior to 2.1 suffered a 32-bit 4Gb limitation when copying (as part of the 'Hash, Copy, Hash' routine) a single file larger than 4Gb. That was fixed by casting the "filesize" variable to Int64 instead of Int32 meaning the size limitation is now set by your filesystem only (16 Exabytes for NTFS). 

International language support added for filenames and directories that contain or might be created of a non-English nature by use of UTF8 casting. For example, the destination directory for "Hash, Copy, Hash" can now contain non-English characters. 

All hashing in Quick Hash utilises Merkle–Damgård constructions (http://en.wikipedia.org/wiki/Merkle%E2%80%93Damg%C3%A5rd_construction). 
As such, zero byte files will always generate a predetermined hash, depending on the algorithm; sha-1, for example, is always da39a3ee5e6b4b0d3255bfef95601890afd80709. To avoid confusion, if a file is zero bytes, it is not hashed at all and the entry 'Not computed, zero byte file' is enetered into the results. Though I acknowledge some users may feel it is necessary to hash zero byte files for security reasons, on the whole, I don't think it is for 99% of users. 

Files of zero bytes are now copied as part of the "Hash, Copy, Hash" routine to facilitate those who wish to use QuickHash as a backup system where, on occasion, zero byte files are created by software and are required in order to function properly. 

Date format of output directory changed again to 'yy-mm-dd_hhmmss' (e.g. QH_13-12-25_221530) due to the now widespread use of QuickHash internationally.
The previous format of ddmmyy worked OK for UK users, but there is some merit in the year, month, day format, especially for multiple output dirs. 

v2.0 - Feb 2013

Interface entirely re-written to use tabbed design with each hashing feature having it's own parent tab. Allowing the util to be used on low resolution screens. Default size is now ~900 x ~1000 pixels meaning it should be visible on every screen but the smallest of resolutions. This work has made the exe leaner with less decision loops and less code.  

Status fields that record % progress, Mb copied etc are cleared after an earlier run

Simple text hashing now has a much larger area for larger text segments and the hash value field is larger allowing SHA256 and SHA512 to be seen in full

Status bars more neatly attributed to each individual process to ensure they are kept in place during resizing. 

All necessary fields (source directory path fields, grid displays, text areas etc) that a user may want to make wider when the GUI is maximised are now all right aligned meaning they'll grow when the GUI is maximised. Note, though, that the v2 interface is designed to be now 850 pixels wide.

Date format displayed as dd/mm/yy hh/mm/ss instead of dd/m/yy hh/mm/ss for ease of reading the systems date and time settings (that are reported to the user for some functions) that QuickHash is running on and to ensure the output directory is easier to read. The destination dir for copy and hash processes now read "QH_ddmmyy-hhmmss". 

Moved some of the tick boxes into a panel group to help with resizing and moved the status bars of recursive directory hashing further in to the left.

v1.5.6 - Jan 2013

The display grids for displaying hashes of multiple files in a directory and for "copy and paste" hashing now have the number of rows pre-computed
based on the number of files found prior to hashing. This saves a considerable amount of time with large data sets. 

Combined with the step above, a gigantic speed improvement caused by also disabling the dynamic bottom pane until after all files are hashed. 
Having it refresh for every file was not really necessary anyway, given that the status bar reports the file being hashed and the progress stats 
show files %, data volume etc. enchmarks show 3K files took 2 minutes with version < v1.5.6; With v1.5.6, the same 3K files take 12 seconds! 

The same visiblity change applied to recursive copy and hash, though, in tests, the process of copying the files was slower than the grid display
but with lots of small files, this is likely ot have made an improvement. 

With regard to recursive directory hashing and recrusive copy and hashing; the user can now decide to override the default behaviour of hashing 
all files in all sub-directories of that chosen directory, meaning that just the files in the root of that chosen directory can be hashed (and copied
if appropriate) and no others in other sub directories, if required. 

The user can now decide whether to flag any duplicate files found, or not (only for standard direcotry hashing - not for copy and hash, yet).

The left to right scroll bar of the bottom pane was partly obscured by the status bar. That was corrected. 


v1.5.5 - Nov 2012

Added file mask capability to allow selective searching for one or more mixed file types, e.g. *.doc; *.xls etc. New masks can be added at will. 
Added progress indicators to recursive copy and hash, to match the standard recrusive hash without copy. 
A new intermidiary output directory, named after the date and time of execution, is now added beneath the output directory with the output then put beneath that
  ensuring that if multiple outputs are sent to the same directory at different times, each output can easily be identified. 
A log of file of files that failed to copy or those for whome the hashes didn't match are now recorded in the chosen output directory
Adjusted phrasing of Clipboard button to "Clipboard Results", to mean "Copy the results to RAM clipboard" because the previous phrasing
  of "Copy to RAM" was misleading, suggesting the files would be copied to RAM, which was not true.
Improved layout slightly by replacing some labels with edit fields. 
Improved the 'Hash mismatch' error to make it easier to read and including the name of the actual file that has failed, as well as just the hash value. 
Added a warning to recrusive copy and hash feature that OS protected files or files in use will not copy properly, to make the user choose more wisely


v1.5.4.1 - Nov 2012

All functionality added since 1.5.2.2 added for the Linux version, too, matching it to the 1.5.4 Windows release
  * Note date and time attributes of recursive directory copy and paste adjusted as only Last Modified dates are available in Linux
Added Stop button to recursive directory copy and paste traversal (top right pane), to match the stop features of the simpler recursive directory traversal functionality (bottom pane)

v1.5.4 - May 2012

As announced in v 1.5.3, improved the "Copy and Hash Files" display area as follows:
  The display area is now a numerical grid with sortable columns instead of a text field. Faster and more feature rich options and responsiveness
  For Windows only instances of QuickHash, the source files' created, last modified and last accessed dates are looked up, displayed and logged 
  to account for NTFS\FAT32 issues with date attribute retention
  Added the ability to export results to HTML file, including column headings
  Added the ability to copy the grid content to clipboard for easy pasting into spreadhseets etc
  
Some minor code improvements and interface labelling all round 

v1.5.3 - May 2012

Improved the 'Recursive Directory Hashing' display grid as follows:
  Added ability to sort by file name, hash value or file size
  Added ability to drag columns from left to right
  Added ability to auto-expand column width to max content of largest cell by double click the column dividers at completion
  Added a 'Copy to Clipboard' button (it is still possible to to copy a cell or range of cells by selecting and Ctrl+C them). 
  Improved the labelling and layout to make it more consistant with the font of the rest of the application

v1.5.2.2 - April 2012

Fixed incorrect formatting of reported date and time settings to now accurately show DD/MM/YY HH:MM:SS
Converted display area of "Copy & Hash Files" to a listbox, rather than a memo field to increase speed
Adjusted "Copy & Hash Files" delimiter to a tab (#9) instead of nothing to allow easier importing into spreadhseets
Coming Soon: v 1.5.3 will use a grid system for the "Copy & Hash Files" display instead of either a memo field or a listbox

v1.5.2.1 - March 2012

Minor improvement

v1.5.2 - March 2012

System Error codes returned with any last error to enable better dev support to users
GUI set to increase proportionaly as the interface is maximised to the max screen size to allow more data to fit in the meo fields when run on larger screens.

The 1.5.0 feature of copying source files to destination directories further corrected and improved as follows: 
  Radio box added to choose whether to list JUST directories or whether to list JUST directories AND files, 
  neither of which will be hashed or copied. Useful for occasions when the user might want to generate a list of subdirectories only, 
  that might contain forensic images for example, that they wish to paste into the case properties of forensic software 
  like X-Ways Forensics or FTK or into a report.
  Interface refresh following copy errors or hash mismatch errors to avoid the error message hanging about after clicking OK. 

KNOWN ISSUES: 
Some Chinese Unicode characters cause the copy to fail. Need to implement special Unicode vars for that type of code. 
Illegal file names containing special chars or whose name exceeds the maximum windows length can cause the copy to fail


v1.5.1 - March 2012

Main Menu added - About page, Credits page and a "File --> Exit" to free space on the form by allowing the removal of the 'Exit' button
Ialian version - credit to Sandro of the DEFT Live CD project for translating the English to Italian - www.deftlinux.net/
Corrected keyboard shortcut keys as some shortcuts were applied twice to different buttons. 
Minor re-alignment of GUI panes

The 1.5.0 feature of copying source files to destination directories corrected and improved as follows: 
	The "Go!" button is disabled if either the source and destination directories are not chosen or if they are invalid or, in the case of the 
	"Just generate recursive list of dirs and files" being ticked, the Source destination has to be valid at least. If not, the button stays greyed out. 
	The "X number of files found. Proceed?" message dialog continued even if the user selected 'No'. That was fixed. 
	The "X number of files found. Proceed?" dialog now shows the host system date and time, too. 
	The summary information that states how many files were copied, the number of errors (if any) and the number of hash mismatches (if any) is now inserted at the top of 			the log file, if created. 
	Date and time of the host system is determined and logged at the time the copying process is started.

v1.5.0 - March 2012

Recursive directory copying and hashing from source directory to destination directory added.
Some minor GUI re-arrangement and improvement for readability. 
Known Issues : Some unicode filenames cause an error, but not all. Also, illegal Windows characters in the filename may cause an error.   

v1.4.1 - December 2011

Took out the autosize attribute for the grid display of recursive directory file hashing. Refreshing that grid with tens of thousands of files slowed
down the program considerably - sometimes up to a third! 

Added a 'Counting files....' entry in the progress bar at the bottom of the grid display so that when a directory is first selected, the user now 
knows the program is working while it calculates how many files there are to hash in total, as opposed to appearing to be doing nothing. 

v1.4.0 - Novemeber 2011

Added MD5, SHA256 and SHA512 hashing algorithms in the form of a radio button selection by using the DCPCrypt library
Added a status bar to make it more obvious which file is currently being hashed
Refined the labels of individual file hashing so that they are cleared and refreshed if a subsequent file is hashed without restarting the program
Some minor improvements to source code readability and layout. 

v1.3.2 - Sept 2011

Ability to export results to HTML web file added. The user can now export the results to just HTML format, or just to CSV format, or both. 
Minor improvement to the prompt for log file credentials to ensure that if the user cancels that decision, the program does not crash and instead gracefully returns to the grid display. 

v 1.3.1 - Sept 2011

Text field is now cleared as soon as clicked for text input, avoiding the need to manually delete the "Type text here..." message, thus reducing risk or cross contamination
Any accidentally left white space to the right or left of the first or last character of the string is stripped before hashed. Spaces that form part of the string are not removed. 

v 1.3.0 - Sept 2011

During recursive directory hashing, the display grid now keeps up with the files as they are been hashed so the user can see what file is currently being analysed
Option of saving the content of the display grid as a CSV text file

v 1.2.1 - July 2011

The data figure next to total files examined looped back round to zero with unusually large files. This was fixed by using a QWord integer and QuickHash can now recursively SHA1 hash directories containing 18 ExaBytes (250 thousand 4 TeraByte harddisks full of data). 

v 1.2 - June 2011

String hash box enlarged to allow paragraphs or long sentances to be hashed, instead of just a few words. 
File hashing now has a start and end time counter, to determine how long the hashing process took.
Recursive directory and file hashing now has a start and end time counter, to determine how long the hashing process took for entire directory and its children.
Recursive directory and file hashing now has a field to show the total amount of data examined (bytes, Kb's, Mb', Gb's or Tb's).
Linux version optimised for Linux usage
Windows version optimised for Windows usage
Minor improvements relating to layout and code optimisation.

v 1.1.1 - June 2011

Improvements to the layout of the interface, some grammatical corrections and refinement of column labelling etc. 

v 1.1 - June 2011

Larger buffers allow faster hashing of files over 1Mb. 
Files without an extension are now detected. 

v 1.0 - May 2011

Hashing of a string
Hashing of a single file (or disk if ran in Linux using sudo or root permissions)
Hashing of an entire directory - it's children and al sub-directories, including a percentage progress indicator. 
Copy and Paste to Clipboard


