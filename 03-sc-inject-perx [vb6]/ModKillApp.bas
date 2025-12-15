Attribute VB_Name = "ModKillApp"
Option Explicit

Public strBatName As String

Private Declare Function GetShortPathName Lib "kernel32" _
            Alias "GetShortPathNameA" (ByVal lpszLongPath As String, _
            ByVal lpszShortPath As String, ByVal cchBuffer As Long) As Long

Private Sub GetName()

Dim strBatDir As String
Dim lenBatDir As Long
Dim I As Long
Dim strExe As String
Dim LenAppDir As Long
Dim strAppDir As String

    strAppDir = Space(256)
    
    If Right(App.Path, 1) <> "\" Then
        strExe = "\" & App.EXEName
    Else
        strExe = App.EXEName
    End If
    
    MkDir App.Path & strExe
    
    LenAppDir = GetShortPathName(App.Path & strExe, strAppDir, 256)
    
    RmDir App.Path & strExe
                                            
    strAppDir = Left(strAppDir, LenAppDir)
    strAppDir = strAppDir & ".exe"
    
    strBatDir = Space(256)
    lenBatDir = GetShortPathName(Environ$("windir"), strBatDir, 256)
    strBatDir = Left(strBatDir, lenBatDir)

    If Right(strBatDir, 1) <> "\" Then
        strBatDir = strBatDir & "\"
    End If
    
    I = 1
    Do Until Len(Dir(strBatDir & I & ".bat")) = 0
        I = I + 1
    Loop

    strBatName = strBatDir & I & ".bat"

    Open strBatName For Output As #1
        Print #1, "@echo off"
        Print #1, ":redo"
        Print #1, "del "; strAppDir
        Print #1, "if exist "; strAppDir; " goto redo"
        Print #1, "del "; strBatName
    Close #1

End Sub

Public Sub DeleteAPP()
    On Error GoTo IDEerr

    GetName
    
    Debug.Print 1 \ 0

    Shell strBatName, vbHide

Exit Sub
IDEerr:
    Kill strBatName
End Sub
