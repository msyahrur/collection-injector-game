Attribute VB_Name = "modInjection"
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

Private Declare Function GetProcAddress Lib "kernel32" (ByVal hModule As Long, ByVal lpProcName As String) As Long
Private Declare Function GetModuleHandle Lib "kernel32" Alias "GetModuleHandleA" (ByVal lpModuleName As String) As Long
Private Declare Function LoadLibrary Lib "kernel32" Alias "LoadLibraryA" (ByVal lpLibFileName As String) As Long
Private Declare Function VirtualAllocEx Lib "kernel32" (ByVal hProcess As Long, lpAddress As Any, ByVal dwSize As Long, ByVal fAllocType As Long, FlProtect As Long) As Long
Public Declare Function WriteProcessMemory Lib "kernel32" (ByVal hProcess As Long, ByVal lpBaseAddress As Any, lpBuffer As Any, ByVal nSize As Long, lpNumberOfBytesWritten As Long) As Long
Private Declare Function CreateRemoteThread Lib "kernel32" (ByVal ProcessHandle As Long, lpThreadAttributes As Long, ByVal dwStackSize As Long, ByVal lpStartAddress As Any, ByVal lpParameter As Any, ByVal dwCreationFlags As Long, lpThreadID As Long) As Long
Public ProsH As Long

Public Function InjectDll(DllPath As String, ProsH As Long)
Dim DLLVirtLoc As Long, DllLength, Inject As Long, LibAddress As Long
Dim CreateThread As Long, ThreadID As Long
Beep
Form1.Label4.Caption = "Dll succesfully injected!"
DllLength = Len(DllPath)
DLLVirtLoc = VirtualAllocEx(ProsH, ByVal 0, DllLength, &H1000, ByVal &H4)
If DLLVirtLoc = 0 Then Form1.Label4.Caption = "VirtualAllocEx API failed!": Exit Function

Inject = WriteProcessMemory(ProsH, DLLVirtLoc, ByVal DllPath, DllLength, vbNull)
If Inject = 0 Then Form1.Label4.Caption = "Failed to Write DLL to Process!"
Form1.Label4.Caption = "Dll Injected...Creating Thread....."

LibAddress = GetProcAddress(GetModuleHandle("kernel32.dll"), "LoadLibraryA")
If LibAddress = 0 Then Form1.Label4.Caption = "Can't find LoadLibrary API from kernel32.dll": Exit Function

CreateThread = CreateRemoteThread(ProsH, vbNull, 0, LibAddress, DLLVirtLoc, 0, ThreadID)
If CreateThread = 0 Then Form1.Label4.Caption = "Failed to Create Thread!"
Form1.Label4.Caption = "Dll Injection Successful!"
End Function
