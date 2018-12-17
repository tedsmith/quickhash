; TODO Add File Header Doc Block with LICENSE

; NSIS Installation Script with Modern User Interface
; for QuickHash GUI for Windows

; TODO Implement Add/Remove Programs functionality from https://nsis.sourceforge.io/A_simple_installer_with_start_menu_shortcut_and_uninstaller

; --------------------------------
; Include Modern UI

!include "MUI2.nsh"

; --------------------------------
; General

; Title and file name of the installer
Name "QuickHash GUI"
OutFile "QuickHash_GUI_v3.0.2_Windows.exe"

!define APPNAME "QuickHash"

; Default installation folder
InstallDir "$PROGRAMFILES\${APPNAME}"
  
; Get installation folder from registry if available
InstallDirRegKey HKLM "Software\${APPNAME}" ""

; Request application privileges to write programs folder (for Windows Vista and above)
RequestExecutionLevel admin

;--------------------------------
; Interface Settings

!define MUI_ABORTWARNING

!define MUI_FINISHPAGE_NOAUTOCLOSE
!define MUI_UNFINISHPAGE_NOAUTOCLOSE

!define MUI_ICON ..\quickhash.ico
!define MUI_UNICON ..\quickhash.ico

;--------------------------------
; Pages

; Installer Pages
;!insertmacro MUI_PAGE_WELCOME ; A welcome page is superfluous
!insertmacro MUI_PAGE_LICENSE "..\LICENSE" ; GNU General Public License v2.0
;!insertmacro MUI_PAGE_COMPONENTS ; We currently need no components page as we install everything
!insertmacro MUI_PAGE_DIRECTORY
; A Start menu folder selection is a nice to have
;Var StartMenuFolder
; !insertmacro MUI_PAGE_STARTMENU "Application" $StartMenuFolder
!insertmacro MUI_PAGE_INSTFILES
;!insertmacro MUI_PAGE_FINISH ; A finish page is superfluous

; Uninstaller Pages  
;!insertmacro MUI_UNPAGE_WELCOME ; A welcome page is superfluous
!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES
;!insertmacro MUI_UNPAGE_FINISH ; A finish page is superfluous
  
; --------------------------------
; Languages

; Load English only at the moment (for simplicity)
!insertmacro MUI_LANGUAGE "English"

; --------------------------------
; Installer Sections

Section "-QuickHash GUI" SecMainApplication
  
  ; Install application files to folder selected in MUI_PAGE_DIRECTORY
  SetOutPath "$INSTDIR"
  File ..\LICENSE
  File ..\UserManual.pdf
  File ..\sqlite3-win32.dll
  File ..\sqlite3-win64.dll
  File QuickHash-Windows-x64.exe ; TODO Set to build path instead
  File QuickHash-Windows-x86.exe ; TODO Set to build path instead
  
  ; Add start menu shortcuts for both binaries and the uninstaller
  createDirectory "$SMPROGRAMS\${APPNAME}"
  createShortCut "$SMPROGRAMS\${APPNAME}\QuickHash-Windows-x64.lnk" "$INSTDIR\QuickHash-Windows-x64.exe"
  createShortCut "$SMPROGRAMS\${APPNAME}\QuickHash-Windows-x86.lnk" "$INSTDIR\QuickHash-Windows-x86.exe"
  createShortCut "$SMPROGRAMS\${APPNAME}\Uninstall.lnk" "$INSTDIR\Uninstall.exe"

  ; Store installation folder in Registry
  WriteRegStr HKLM "Software\${APPNAME}" "" $INSTDIR
  
  ; Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall.exe"

SectionEnd

;--------------------------------
; Descriptions

; Currently not used, as the components pages is not used

  ;Language strings
  ;LangString DESC_SecMainApplication ${LANG_ENGLISH} "The application itself."

  ;Assign language strings to sections
  ;!insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
    ;!insertmacro MUI_DESCRIPTION_TEXT ${SecMainApplication} $(DESC_SecMainApplication)
  ;!insertmacro MUI_FUNCTION_DESCRIPTION_END

;--------------------------------
; Uninstaller Section

Section "Uninstall"

  ; Files to remove from installation folder
  Delete "$INSTDIR\Uninstall.exe"
  Delete "$INSTDIR\LICENSE"
  Delete "$INSTDIR\UserManual.pdf"
  Delete "$INSTDIR\sqlite3-win32.dll"
  Delete "$INSTDIR\sqlite3-win64.dll"
  Delete "$INSTDIR\QuickHash-Windows-x64.exe"
  Delete "$INSTDIR\QuickHash-Windows-x86.exe"
  RMDir "$INSTDIR"

  ; Files to remove from start menu  
  Delete "$SMPROGRAMS\${APPNAME}\QuickHash-Windows-x64.lnk"
  Delete "$SMPROGRAMS\${APPNAME}\QuickHash-Windows-x86.lnk"
  Delete "$SMPROGRAMS\${APPNAME}\Uninstall.lnk"  
  RMDir "$SMPROGRAMS\${APPNAME}"

  ; Delete from registry key
  DeleteRegKey /ifempty HKLM "Software\${APPNAME}"

SectionEnd