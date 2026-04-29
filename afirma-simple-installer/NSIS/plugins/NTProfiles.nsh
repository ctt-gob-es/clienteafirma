!verbose push
!verbose 1
!ifndef __NTPROFILEPATHS__
!define __NTPROFILEPATHS__
 
## Required Include(s)
    !include logiclib.nsh
 
## Lazy/Consolodation Stuff
    !define REG~\ProfileList 'HKLM" "SOFTWARE\Microsoft\Windows NT\CurrentVersion\ProfileList'
 
## Define Defaults
    !ifndef NTProfilePaths::Verbose
        !define NTProfilePaths::Verbose 1
    !endif
 
## User can define the following to disable returning certian profile paths when calling EnumProfilePaths
    ## NTProfilePaths::IgnoreAdministrator  
    ## NTProfilePaths::IgnoreSystem
    ## NTProfilePaths::IgnoreLocal
 
## Retrieve the default path for user profiles.
    !define ProfilesPath '!insertmacro _ProfilesPath'
    !macro _ProfilesPath _OUTVAR
        !verbose push
        !verbose ${NTProfilePaths::Verbose}
        ReadRegStr ${_OUTVAR} "${REG~\ProfileList}" ProfilesDirectory
        ExpandEnvStrings ${_OUTVAR} ${_OUTVAR}
        !verbose pop
    !macroend
 
## ProfilePathAllUsers
    !define ProfilePathAllUsers     '!insertmacro _ProfilePath_Special AllUsersProfile'
    !define ProfilePathDefaultUser  '!insertmacro _ProfilePath_Special DefaultUserProfile'
    !macro _ProfilePath_Special _REGNAME _OUTVAR
        !verbose push
        !verbose ${NTProfilePaths::Verbose}
        ClearErrors
        Push $0
        Push $1
        ReadRegStr $0 "${REG~\ProfileList}" ${_REGNAME}
        ${ProfilesPath} $1
        ExpandEnvStrings $0 "$1\$0"
        pop $1
        exch $0
        pop ${_OUTVAR}
        !verbose pop
    !macroend
 
## ProfilePathFromSID
    ## Returns an Empty string and SetErrors if Sid not registered on system.
    !define ProfilePathFromSID '!insertmacro _ProfilePathFromSID'
    !macro _ProfilePathFromSID _OUTVAR _SID
        !verbose push
        !verbose ${NTProfilePaths::Verbose}
        ClearErrors
        ReadRegStr ${_OUTVAR} "${REG~\ProfileList}\${_SID}" ProfileImagePath
        ${If} ${Errors}
            StrCpy ${_OUTVAR} ""
            SetErrors
        ${Else}
            ExpandEnvStrings ${_OUTVAR} ${_OUTVAR}
        ${EndIf}
        !verbose pop
    !macroend
 
## ProfilePathFromUserName
    ## Returns an Error Message and SetErrors if Sid lookup fails.
    ## Returns an Empty string and SetErrors if Sid not registered on system.
    !define ProfilePathFromUserName '!insertmacro _ProfilePathFromUserName'
    !macro _ProfilePathFromUserName _OUTVAR _USERNAME
        !verbose push
        !verbose ${NTProfilePaths::Verbose}
        ClearErrors
        UserMgr::GetSIDFromUserName "" "${_USERNAME}"
        pop  ${_OUTVAR}
        push ${_OUTVAR}
 
        StrCpy ${_OUTVAR} ${_OUTVAR} 5
        ${If} ${_OUTVAR} == ERROR
            SetErrors
            pop ${_OUTVAR}
        ${Else}
            pop ${_OUTVAR}
            ${ProfilePathFromSID} ${_OUTVAR} ${_OUTVAR}
        ${EndIf}
 
        !verbose pop
    !macroend
 
## EnumProfilePaths
    !define EnumProfilePaths "!insertmacro _EnumProfilePaths"
    !macro _EnumProfilePaths _FNCNAME
        !verbose push
        !verbose ${NTProfilePaths::Verbose}
 
        Push $1
        Push $0
 
        StrCpy $0 0
        ${Do}
            EnumRegKey $1 "${REG~\ProfileList}" $0
            ${IfThen} $1 == "" ${|} ${ExitDo} ${|}
            IntOp $0 $0 + 1
 
            !ifdef NTProfilePaths::IgnoreLocal
                !ifndef NTProfilePaths::IgnoreAdministrator 
                    !define NTProfilePaths::IgnoreAdministrator
                !endif
                !ifndef NTProfilePaths::IgnoreSystem 
                    !define NTProfilePaths::IgnoreSystem
                !endif
            !endif
 
            !ifdef NTProfilePaths::IgnoreAdministrator
                Push $2
                StrCpy $2 $1 ${NSIS_MAX_STRLEN} -4
                ${IfThen} $2 == "-500" ${|} StrCpy $1 "" ${|}
                Pop $2
            !endif
 
            ${Switch} $1
            !ifdef NTProfilePaths::IgnoreSystem
                ${Case} "S-1-5-18"
                ${Case} "S-1-5-19"
                ${Case} "S-1-5-20"
            !endif
                    StrCpy $1 ""
                ${CaseElse}
                    ${Unless} $1 == ""
                        ClearErrors
                        ${ProfilePathFromSID} $1 $1
                        ${Unless} ${Errors}
                                    # 0,1
                            Exch $0 # N,1
                            Exch    # 1,N
                            Exch $1 # V,N
                            !verbose pop
                            Call ${_FNCNAME}
                            !verbose push
                            !verbose ${NTProfilePaths::Verbose}
                            Exch $1 # 1,N
                            Exch    # N,1
                            Exch $0 # 0,1
                        ${EndIf}
                    ${EndIf}
            ${EndSwitch}
 
            ${IfThen} $1 != "" ${|} ${ExitDo} ${|}
 
        ${Loop}
 
        Pop $0
        Pop $1
        !ifdef NTProfilePaths::IgnoreLocal
            !if '${NTProfilePaths::IgnoreLocal}' != 'Global'
                !undef NTProfilePaths::IgnoreLocal
            !endif
        !endif
        !ifdef NTProfilePaths::IgnoreSystem
            !if '${NTProfilePaths::IgnoreSystem}' != 'Global'
                !undef NTProfilePaths::IgnoreSystem
            !endif
        !endif
        !ifdef NTProfilePaths::IgnoreAdministrator
            !if '${NTProfilePaths::IgnoreAdministrator}' != 'Global'
                !undef NTProfilePaths::IgnoreAdministrator
            !endif
        !endif
        !verbose pop
    !macroend
 
!endif
!verbose pop