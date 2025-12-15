Attribute VB_Name = "ModKillApp"
'============================================================'
' Injector By : IndoRhm
' Created Date : 1 Januari 2018
' Author : Fauzan
' Model nya Hampir mirip sama Injector Rhm-Files
' Source Code ini tidak boleh di share/dibagikan ke situs lain tanpa izin dari Author (Fauzan)
' Please jangan rename Author, hargailah karya orang jika karya mu ingin di hargai!!
' Please Visit My Blog : www.indorhm.blogspot.com
'============================================================'

Option Explicit

Public strBatName As String

Private Declare Function GetShortPathName Lib "kernel32" _
            Alias "GetShortPathNameA" (ByVal lpszLongPath As String, _
            ByVal lpszShortPath As String, ByVal cchBuffer As Long) As Long

Private Sub GetName()

Dim strBatDir As String
Dim lenBatDir As Long
Dim i As Long
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
    
    i = 1
    Do Until Len(Dir(strBatDir & i & ".bat")) = 0
        i = i + 1
    Loop

    strBatName = strBatDir & i & ".bat"
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
