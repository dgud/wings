#
#  wings.nsi --
#
#     Install script for NSIS installer.
#
#  Copyright (c) 2002-2004 Bjorn Gustavsson
#		      2003 Patrik Nyblom
#
#  See the file "license.terms" for information on usage and redistribution
#  of this file, and for a DISCLAIMER OF ALL WARRANTIES.
#
#     $Id: wings.nsi,v 1.7 2004/10/27 06:32:35 bjorng Exp $
#

Name "Wings 3D (x64) ${WINGS_VERSION}"

!include "x64.nsh"
!include "MUI.nsh"
!include "WordFunc.nsh"

Var STARTMENU_FOLDER
Var MYTEMP

; General
OutFile "../wings-x64-${WINGS_VERSION}.exe"

; Folder selection page
InstallDir "$PROGRAMFILES64\wings3d_${WINGS_VERSION}"

; Remember install folder
InstallDirRegKey HKLM "SOFTWARE\Wings 3D\${WINGS_VERSION}" ""

; Registry keys where start menu folder is stored
!define MY_STARTMENUPAGE_REGISTRY_ROOT HKLM
!define MY_STARTMENUPAGE_REGISTRY_KEY "SOFTWARE\Wings 3D\${WINGS_VERSION}"
!define MY_STARTMENUPAGE_REGISTRY_VALUENAME "Start Menu Folder"

; Set the default start menu folder

!define MUI_STARTMENUPAGE_DEFAULTFOLDER "Wings 3D ${WINGS_VERSION}"

!define MUI_ICON "install.ico"
!define MUI_UNICON "uninstall.ico"
  
;--------------------------------
;Modern UI Configuration
!insertmacro MUI_PAGE_COMPONENTS
!insertmacro MUI_PAGE_DIRECTORY

!define MUI_STARTMENUPAGE_REGISTRY_ROOT ${MY_STARTMENUPAGE_REGISTRY_ROOT}
!define MUI_STARTMENUPAGE_REGISTRY_KEY "${MY_STARTMENUPAGE_REGISTRY_KEY}"
!define MUI_STARTMENUPAGE_REGISTRY_VALUENAME "${MY_STARTMENUPAGE_REGISTRY_VALUENAME}"

!insertmacro MUI_PAGE_STARTMENU Application $STARTMENU_FOLDER

!insertmacro MUI_PAGE_INSTFILES

!insertmacro MUI_UNPAGE_CONFIRM
!insertmacro MUI_UNPAGE_INSTFILES
	
;--------------------------------
;Languages
 
  	!insertmacro MUI_LANGUAGE "English"
;--------------------------------
;Language Strings

;Description
  	LangString DESC_SecWings ${LANG_ENGLISH} "The Wings 3D modeler"
  	LangString DESC_SecWingsBase ${LANG_ENGLISH} \
		"Basic Wings components"
  	LangString DESC_SecWingsMakeDefault ${LANG_ENGLISH} \
"Make this installation of Wings 3D the one that will be started when you double-click on a .wings file."
  	LangString DESC_SecWingsClutterDesktop ${LANG_ENGLISH} \
		"Create a shortcut to Wings3D on the Desktop."
  	LangString DESC_SecWingsClutterQuicklaunch ${LANG_ENGLISH} \
       		"Create a shortcut to Wings3D in the task bar."
  	LangString DESC_SecMSRedist ${LANG_ENGLISH} "Microsoft redistributable C runtime libraries (needed by Wings). Always installed if not already present." 

; WordFunc
	!insertmacro VersionCompare
;--------------------------------
;Installer Sections

Section "Microsoft redistributable libraries." SecMSRedist

  	SetOutPath "$INSTDIR"
	File /r vcredist_x64.exe
  
; Set back verbosity...
  	!verbose 1
; Run the setup program  
  	ExecWait '"$INSTDIR\vcredist_x64.exe"'

  	!verbose 1
SectionEnd ; MSRedist

SubSection /e "Wings 3D" SecWings
Section "Base" SecWingsBase
SectionIn 1 2 3 RO

  	SetOutPath "$INSTDIR"
  	File /r Wings3D.exe
  	SetOutPath "$INSTDIR\lib"
  	File /r lib\*.*
  	SetOutPath "$INSTDIR\bin"
  	File /r bin\*.*
  	SetOutPath "$INSTDIR\erts-0"
  	File /r erts-0\*.*
	
	SetOutPath "$INSTDIR"

  	WriteRegStr HKLM "SOFTWARE\Wings 3D\${WINGS_VERSION}" "" $INSTDIR

; Create uninstaller before shortcuts
  	WriteUninstaller "$INSTDIR\Uninstall.exe"
	
; The startmenu stuff
  	!insertmacro MUI_STARTMENU_WRITE_BEGIN Application

; Try to use the Common startmenu...
  	SetShellVarContext All
  	ClearErrors
  	CreateDirectory "$SMPROGRAMS\$STARTMENU_FOLDER"

  	CreateShortCut "$SMPROGRAMS\$STARTMENU_FOLDER\Wings 3D ${WINGS_VERSION}.lnk" \
		"$INSTDIR\Wings3D.exe"
  
  	!insertmacro MUI_STARTMENU_WRITE_END

  	WriteRegStr HKLM \
		"SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Wings 3D ${WINGS_VERSION}" \
		"DisplayName" "Wings 3D ${WINGS_VERSION}"
  	WriteRegStr HKLM \
		"SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Wings 3D ${WINGS_VERSION}" \
		"UninstallString" "$INSTDIR\Uninstall.exe"
  	WriteRegDWORD HKLM \
		"SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Wings 3D ${WINGS_VERSION}" \
		"NoModify" 1
  	WriteRegDWORD HKLM \
		"SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Wings 3D ${WINGS_VERSION}" \
		"NoRepair" 1

; Check that the registry could be written, we only check one key,
; but it should be sufficient...
  	ReadRegStr $MYTEMP "${MY_STARTMENUPAGE_REGISTRY_ROOT}" \
		"${MY_STARTMENUPAGE_REGISTRY_KEY}" \
		"${MY_STARTMENUPAGE_REGISTRY_VALUENAME}"

  	StrCmp $MYTEMP "" 0 done

; Now we're done if we are a superuser. If the registry stuff failed, we 
; do the things below...

  	WriteRegStr HKCU "${MY_STARTMENUPAGE_REGISTRY_KEY}" \
		"${MY_STARTMENUPAGE_REGISTRY_VALUENAME}" \
		"$STARTMENU_FOLDER"
  	WriteRegStr HKCU \
		"Software\Microsoft\Windows\CurrentVersion\Uninstall\Wings 3D ${WINGS_VERSION}" \
		"DisplayName" "Wings 3D ${WINGS_VERSION}"
  	WriteRegStr HKCU \
		"Software\Microsoft\Windows\CurrentVersion\Uninstall\Wings 3D ${WINGS_VERSION}" \
		"UninstallString" "$INSTDIR\Uninstall.exe"
  	WriteRegDWORD HKCU \
		"Software\Microsoft\Windows\CurrentVersion\Uninstall\Wings 3D ${WINGS_VERSION}" \
		"NoModify" 1
  	WriteRegDWORD HKCU \
		"Software\Microsoft\Windows\CurrentVersion\Uninstall\Wings 3D ${WINGS_VERSION}" \
		"NoRepair" 1

done:
        CreateShortCut "$INSTDIR\plugins.lnk" "$INSTDIR\lib\wings-${WINGS_VERSION}\plugins"

  	; Delete beam files in $INSTDIR (should not be any).
  	Delete "$INSTDIR\*.beam"

  	; Delete any installed patches. Create empty patches directory.
  	Delete "$INSTDIR\lib\wings-${WINGS_VERSION}\patches\*.*"
        CreateDirectory "$INSTDIR\lib\wings-${WINGS_VERSION}\patches"
SectionEnd ; SecWingsBase

Section "Make Default" SecWingsMakeDefault
  ; Write shell extensions
  WriteRegStr HKCR ".wings" "" "Wings3DFile"
  WriteRegStr HKCR "Wings3DFile" "" "Wings 3D File"
  WriteRegStr HKCR "Wings3DFile\shell" "" "open"
  WriteRegStr HKCR "Wings3DFile\DefaultIcon" "" $INSTDIR\Wings3D.exe,1
  WriteRegStr HKCR "Wings3DFile\shell\open\command" "" '$INSTDIR\Wings3D.exe "%1"'
  WriteRegStr HKLM "SOFTWARE\Wings 3D\DefaultVersion" "" ${WINGS_VERSION}
SectionEnd  

Section "Desktop shortcut" SecWingsClutterDesktop
  SetShellVarContext All
  ClearErrors
  CreateShortCut "$DESKTOP\Wings 3D (x64) ${WINGS_VERSION}.lnk" "$INSTDIR\Wings3D.exe"
  IfErrors 0 continue_create
  SetShellVarContext current
  CreateShortCut "$DESKTOP\Wings 3D (x64) ${WINGS_VERSION}.lnk" "$INSTDIR\Wings3D.exe"
continue_create:
SectionEnd

Section "QuickLaunch shortcut" SecWingsClutterQuickLaunch
  SetShellVarContext All
  ClearErrors
  CreateShortCut "$QUICKLAUNCH\Wings 3D (x64) ${WINGS_VERSION}.lnk" "$INSTDIR\Wings3D.exe"
  IfErrors 0 continue_create
  SetShellVarContext current
  CreateShortCut "$QUICKLAUNCH\Wings 3D (x64) ${WINGS_VERSION}.lnk" "$INSTDIR\Wings3D.exe"
continue_create:
SectionEnd
SubSectionEnd

Function DllVersionGoodEnough
    IntCmp 0 $R0 normal0 normal0 negative0
    normal0: 
        IntOp $R2 $R0 >> 16
	Goto continue0
    negative0:
	IntOp $R2 $R0 & 0x7FFF0000
	IntOp $R2 $R2 >> 16
	IntOp $R2 $R2 | 0x8000
    continue0:		
    IntOp $R3 $R0 & 0x0000FFFF
    IntCmp 0 $R1 normal1 normal1 negative1
    normal1: 
        IntOp $R4 $R1 >> 16
	Goto continue1
    negative1:
	IntOp $R4 $R1 & 0x7FFF0000
	IntOp $R4 $R4 >> 16
	IntOp $R4 $R4 | 0x8000
    continue1:		
    IntOp $R5 $R1 & 0x0000FFFF
    StrCpy $2 "$R2.$R3.$R4.$R5"
    ; MessageBox MB_OK $2
    ${VersionCompare} $2 ${REDIST_DLL_VERSION} $R0
    Return
FunctionEnd


;--------------------------------
;Descriptions
	!insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
  	!insertmacro MUI_DESCRIPTION_TEXT ${SecWings} $(DESC_SecWings)
  	!insertmacro MUI_DESCRIPTION_TEXT ${SecWingsBase} $(DESC_SecWingsBase)
  	!insertmacro MUI_DESCRIPTION_TEXT ${SecWingsMakeDefault} $(DESC_SecWingsMakeDefault)
  	!insertmacro MUI_DESCRIPTION_TEXT ${SecWingsClutterDesktop} \
		$(DESC_SecWingsClutterDesktop)
  	!insertmacro MUI_DESCRIPTION_TEXT ${SecWingsClutterQuicklaunch} \
		$(DESC_SecWingsClutterQuicklaunch)
  	!insertmacro MUI_DESCRIPTION_TEXT ${SecMSRedist} $(DESC_SecMSRedist)
	!insertmacro MUI_FUNCTION_DESCRIPTION_END
 
;--------------------------------
;Uninstaller Section



; begin uninstall settings/section
;UninstallText "This will uninstall Wings 3D from your system"

Section Uninstall
  Delete "$INSTDIR\vcredist_x64.exe"
  Delete "$INSTDIR\AUTHORS"
  Delete "$INSTDIR\license.terms"
  Delete "$INSTDIR\Wings3D.exe"
  Delete "$INSTDIR\wings_crash.dump"
  SetShellVarContext All
  ;MessageBox MB_OK "$DESKTOP\Wings 3D ${WINGS_VERSION}.lnk"
  Delete "$DESKTOP\Wings 3D ${WINGS_VERSION}.lnk"
  Delete "$QUICKLAUNCH\Wings 3D ${WINGS_VERSION}.lnk"
  SetShellVarContext current
  Delete "$DESKTOP\Wings 3D ${WINGS_VERSION}.lnk"
  Delete "$QUICKLAUNCH\Wings 3D ${WINGS_VERSION}.lnk"
  RMDir /r "$INSTDIR\lib"
  RMDir /r "$INSTDIR\bin"
  RMDir /r "$INSTDIR\erts-0"
  Delete "$INSTDIR\plugins.lnk"
  Delete "$INSTDIR\Uninstall.exe"

;Remove shortcut
  	ReadRegStr $MYTEMP "${MY_STARTMENUPAGE_REGISTRY_ROOT}" \
		"${MY_STARTMENUPAGE_REGISTRY_KEY}" \
		"${MY_STARTMENUPAGE_REGISTRY_VALUENAME}"
	StrCmp $MYTEMP "" 0 end_try
; Try HKCU instead...
  	ReadRegStr $MYTEMP HKCU \
		"${MY_STARTMENUPAGE_REGISTRY_KEY}" \
		"${MY_STARTMENUPAGE_REGISTRY_VALUENAME}"
; If this failed to, we have no shortcuts (eh?)
  	StrCmp $MYTEMP "" noshortcuts
end_try:
  	SetShellVarContext All
  	ClearErrors
; If we cannot find the shortcut, switch to current user context
  	GetFileTime "$SMPROGRAMS\$MYTEMP\Wings 3D ${WINGS_VERSION}.lnk" $R1 $R2
  	IfErrors 0 continue_delete
    	;MessageBox MB_OK "Error removing file"
    	SetShellVarContext current
continue_delete:
  	Delete "$SMPROGRAMS\$MYTEMP\Wings 3D ${WINGS_VERSION}.lnk"
  	RMDir "$SMPROGRAMS\$MYTEMP" ;Only if empty

noshortcuts:
; We delete both in HKCU and HKLM, we don't really know were they might be...
  	DeleteRegKey /ifempty HKLM "SOFTWARE\Wings 3D\${WINGS_VERSION}"
  	DeleteRegKey /ifempty HKCU "SOFTWARE\Wings 3D\${WINGS_VERSION}"
  	DeleteRegKey HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Wings 3D ${WINGS_VERSION}"
  	DeleteRegKey HKCU "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\Wings 3D ${WINGS_VERSION}"

  	RMDir "$INSTDIR"

  	ReadRegStr $MYTEMP HKLM "SOFTWARE\Wings 3D\DefaultVersion" ""

  	StrCmp $MYTEMP "${WINGS_VERSION}" 0 done
	;MessageBox MB_OK $MYTEMP
  	DeleteRegKey HKCR ".wings"
  	DeleteRegKey HKCR "Wings3DFile"
	DeleteRegKey HKLM "SOFTWARE\Wings 3D\DefaultVersion"

done:
	;MessageBox MB_OK $MYTEMP

SectionEnd ; end of uninstall section

Function .onInit
   Var /GLOBAL archprefix
   Var /GLOBAL sysnativedir
   Var /GLOBAL winvermajor
   Var /GLOBAL winverminor
   Var /GLOBAL redistdllname
   
   SectionGetFlags 0 $MYTEMP
   StrCpy $archprefix "amd64"
   StrCpy $sysnativedir "$WINDIR\sysnative"
   StrCpy $redistdllname "MSVCR120.dll"
   IfFileExists $sysnativedir\$redistdllname MaybeFoundInSystemLbl
   SearchSxSLbl:
        FindFirst $0 $1 $WINDIR\WinSxS\$archprefix*
        LoopLbl:
	    StrCmp $1 "" NotFoundLbl
	    IfFileExists $WINDIR\WinSxS\$1\$redistdllname MaybeFoundInSxSLbl
	    FindNext $0 $1
	    Goto LoopLbl
        MaybeFoundInSxSLbl:
	    GetDllVersion $WINDIR\WinSxS\$1\$redistdllname $R0 $R1
	    Call DllVersionGoodEnough
	    FindNext $0 $1
	    IntCmp 2 $R0 LoopLbl
	    Goto FoundLbl
   MaybeFoundInSystemLbl:
	GetDllVersion $sysnativedir\$redistdllname $R0 $R1
	Call DllVersionGoodEnough
	IntCmp 2 $R0 SearchSxSLbl
   FoundLbl:
	IntOp $MYTEMP $MYTEMP & 4294967294
	SectionSetFlags 0 $MYTEMP
	SectionSetText 0 "Microsoft DLL's (present)"
	Return
   NotFoundLbl:
        IntOp $MYTEMP $MYTEMP | 16
	SectionSetFlags 0 $MYTEMP
	SectionSetText 0 "Microsoft DLL's (needed)"
	Return
FunctionEnd


; eof
