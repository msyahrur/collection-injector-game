Attribute VB_Name = "modConfig"
Option Explicit

Public Declare Function ShellExecute Lib "shell32.dll" Alias "ShellExecuteA" (ByVal hwnd As Long, ByVal lpOperation As String, ByVal lpFile As String, ByVal lpParameters As String, ByVal lpDirectory As String, ByVal nShowCmd As Long) As Long
Declare Function WritePrivateProfileString Lib "kernel32" Alias "WritePrivateProfileStringA" (ByVal lpApplicationname As String, ByVal lpKeyName As Any, ByVal lsString As Any, ByVal lplFilename As String) As Long
Declare Function GetPrivateProfileString Lib "kernel32" Alias "GetPrivateProfileStringA" (ByVal lpApplicationname As String, ByVal lpKeyName As String, ByVal lpDefault As String, ByVal lpReturnedString As String, ByVal nSize As Long, ByVal lpFileName As String) As Long
Public Check As String

Public Function Load(Section As String, Key As String) As String
Dim lngResult As Long
Dim strFileName
Dim strResult As String * 300
strFileName = App.Path & "\PerX.ini"
lngResult = GetPrivateProfileString(Section, Key, strFileName, strResult, Len(strResult), strFileName)
Check = App.Path & "\PerX.ini"
Load = Trim(strResult)
End Function

Public Function Save(Section As String, Key As String, Content As String)
Dim lngResult As Long
Dim strFileName
strFileName = App.Path & "\PerX.ini"
lngResult = WritePrivateProfileString(Section, Key, Content, strFileName)
End Function

Public Sub OpenURL(situs As String, sourceHWND As Long)
     Call ShellExecute(sourceHWND, vbNullString, situs, vbNullString, vbNullString, 1)
End Sub
