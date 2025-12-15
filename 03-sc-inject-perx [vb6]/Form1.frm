VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "comdlg32.ocx"
Begin VB.Form Form1 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "x1nixmzeng's x1nject"
   ClientHeight    =   6405
   ClientLeft      =   45
   ClientTop       =   435
   ClientWidth     =   4935
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "Form1.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   Picture         =   "Form1.frx":000C
   ScaleHeight     =   6405
   ScaleWidth      =   4935
   StartUpPosition =   3  'Windows Default
   Tag             =   "1"
   Begin VB.Timer Timer4 
      Left            =   1680
      Top             =   6240
   End
   Begin VB.Timer Timer3 
      Left            =   1320
      Top             =   6240
   End
   Begin VB.Timer tmrDetect 
      Left            =   2040
      Top             =   6240
   End
   Begin VB.PictureBox Picture1 
      Height          =   495
      Left            =   120
      ScaleHeight     =   435
      ScaleWidth      =   4635
      TabIndex        =   17
      Top             =   5280
      Width           =   4695
      Begin VB.Label Label4 
         Alignment       =   2  'Center
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         ForeColor       =   &H00000000&
         Height          =   195
         Left            =   2280
         TabIndex        =   18
         Top             =   120
         Width           =   195
      End
   End
   Begin VB.CommandButton Command5 
      Caption         =   "&Quit"
      Height          =   375
      Left            =   3720
      MouseIcon       =   "Form1.frx":665BE
      MousePointer    =   99  'Custom
      TabIndex        =   6
      Top             =   5880
      Width           =   1095
   End
   Begin VB.CommandButton Command4 
      Caption         =   "I&nject"
      Height          =   375
      Left            =   2520
      MouseIcon       =   "Form1.frx":66710
      MousePointer    =   99  'Custom
      TabIndex        =   5
      Top             =   5880
      Width           =   1095
   End
   Begin VB.Frame Frame2 
      Caption         =   "Injections"
      ForeColor       =   &H00000000&
      Height          =   2175
      Left            =   120
      TabIndex        =   14
      Top             =   3000
      Width           =   4695
      Begin VB.CommandButton Command2 
         Caption         =   "Remove &Selected"
         Height          =   375
         Left            =   240
         MouseIcon       =   "Form1.frx":66862
         MousePointer    =   99  'Custom
         TabIndex        =   3
         Top             =   840
         Width           =   1695
      End
      Begin VB.CommandButton Command1 
         Caption         =   "&Browse"
         Height          =   375
         Left            =   2160
         MouseIcon       =   "Form1.frx":669B4
         MousePointer    =   99  'Custom
         TabIndex        =   1
         Top             =   240
         Width           =   2295
      End
      Begin VB.CommandButton Command3 
         Caption         =   "&Clear List"
         Height          =   375
         Left            =   240
         MouseIcon       =   "Form1.frx":66B06
         MousePointer    =   99  'Custom
         TabIndex        =   4
         Top             =   1320
         Width           =   1695
      End
      Begin VB.ListBox List1 
         BackColor       =   &H00FFFFFF&
         Height          =   1330
         IntegralHeight  =   0   'False
         Left            =   2160
         MouseIcon       =   "Form1.frx":66C58
         TabIndex        =   15
         Top             =   720
         Width           =   2295
      End
      Begin VB.Label Label5 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "Add to injection list:"
         ForeColor       =   &H00000000&
         Height          =   195
         Left            =   240
         TabIndex        =   16
         Top             =   360
         Width           =   1425
      End
   End
   Begin VB.Frame Frame1 
      Caption         =   "Program"
      ForeColor       =   &H00000000&
      Height          =   1815
      Left            =   120
      TabIndex        =   7
      Top             =   1080
      Width           =   4695
      Begin VB.OptionButton Option1 
         Caption         =   "Manually"
         ForeColor       =   &H00000000&
         Height          =   195
         Index           =   1
         Left            =   2160
         MaskColor       =   &H00FFFFFF&
         TabIndex        =   10
         Top             =   1080
         Width           =   1215
      End
      Begin VB.CheckBox Check1 
         Caption         =   "Quit when finished"
         ForeColor       =   &H00000000&
         Height          =   195
         Left            =   2160
         TabIndex        =   9
         Top             =   1440
         Width           =   1695
      End
      Begin VB.OptionButton Option1 
         Caption         =   "Automatically"
         ForeColor       =   &H00000000&
         Height          =   195
         Index           =   0
         Left            =   2160
         TabIndex        =   8
         Top             =   720
         Width           =   1455
      End
      Begin VB.TextBox Text1 
         Height          =   285
         Left            =   2160
         TabIndex        =   0
         Tag             =   "0"
         Top             =   240
         Width           =   2295
      End
      Begin VB.Label Label2 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "How to inject:"
         ForeColor       =   &H00000000&
         Height          =   195
         Left            =   240
         TabIndex        =   13
         Top             =   720
         Width           =   1005
      End
      Begin VB.Label Label3 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "After injection:"
         ForeColor       =   &H00000000&
         Height          =   195
         Left            =   240
         TabIndex        =   12
         Top             =   1440
         Width           =   1080
      End
      Begin VB.Label Label1 
         AutoSize        =   -1  'True
         BackStyle       =   0  'Transparent
         Caption         =   "What to inject:"
         ForeColor       =   &H00000000&
         Height          =   195
         Left            =   240
         TabIndex        =   11
         Top             =   300
         Width           =   1080
      End
   End
   Begin VB.Timer Timer1 
      Left            =   600
      Top             =   6240
   End
   Begin VB.ListBox List2 
      Height          =   840
      Left            =   120
      TabIndex        =   2
      Top             =   120
      Width           =   4695
   End
   Begin VB.Timer Timer2 
      Left            =   960
      Top             =   6240
   End
   Begin MSComDlg.CommonDialog CD1 
      Left            =   120
      Top             =   6240
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin VB.Label Label7 
      AutoSize        =   -1  'True
      BackStyle       =   0  'Transparent
      Caption         =   "Public Release  [25.09.11]"
      ForeColor       =   &H00808080&
      Height          =   195
      Left            =   240
      MouseIcon       =   "Form1.frx":66DAA
      TabIndex        =   19
      Top             =   6000
      Width           =   1890
   End
End
Attribute VB_Name = "Form1"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False

'PerX [R] Injector by GembelRasta
'Created Date: 25 September 2011

'thanx to:
'AgusWahidPrastyo a.k.a AWP-Slankers [www.awp1st.net]
'rifqi36 [www.nyit-nyit.net]
'RichardYusan a.k.a RCD [www.richardyusan.blogspot.com]
'Reckless Youth [www.MPCforum.com]
'Mark Thesing [www.VBforums.com]

'Fitur:
'1.Multi Dll injection
'2.Program akan diDelete secara otomatis jika:
'  -Nama Program diubah
'  -Nama Author diubah
'  -Hak cipta diubah
'  -Deskripsi program diubah
'  -Ukuran file(kb) berubah
'  -isi program dirubah dengan OllyDbg atau Debugger
'3.Dapat menggunakan icon 32bit (Lihat keterangan gambar)
'4.mendeteksi jika program yang akan diinject sedang running/berjalan

'Penting !
'Code ini tidak boleh di share/dibagikan ke situs ataupun forum lain tanpa izin dari Author (GembelRasta)!
'Untuk proteksi yang lebih baik gunakan VMProtect,UPX atau Exe compressor lain,Sesuaikan ukuran file pada Check_Size setelah di Compress
'Informasi lebih lanjut kirim ke : rastasoul58@yahoo.co.id(Email & Facebook)

Option Explicit

Private Declare Function GetAsyncKeyState Lib "user32" (ByVal vKey As Long) As Integer
Dim DllPath As String

Private Declare Function IsDebuggerPresent Lib "kernel32" () As Long
Private Declare Function OpenProcess Lib "kernel32" ( _
    ByVal dwDesiredAccess As Long, ByVal bInheritHandle As Long, ByVal dwProcessId As Long) As Long
Private Declare Function CloseHandle Lib "kernel32" ( _
    ByVal hObject As Long) As Long
Private Declare Function EnumProcesses Lib "PSAPI.DLL" ( _
   lpidProcess As Long, ByVal cb As Long, cbNeeded As Long) As Long
Private Declare Function EnumProcessModules Lib "PSAPI.DLL" ( _
    ByVal hProcess As Long, lphModule As Long, ByVal cb As Long, lpcbNeeded As Long) As Long
Private Declare Function GetModuleBaseName Lib "PSAPI.DLL" Alias "GetModuleBaseNameA" ( _
    ByVal hProcess As Long, ByVal hModule As Long, ByVal lpFileName As String, ByVal nSize As Long) As Long

Private Const PROCESS_VM_READ = &H10
Private Const PROCESS_QUERY_INFORMATION = &H400

Private Function IsProcessRunning(ByVal sProcess As String) As Boolean
    Const MAX_PATH As Long = 260
    Dim lProcesses() As Long, lModules() As Long, N As Long, lRet As Long, hProcess As Long
    Dim sName As String
    
    sProcess = UCase$(sProcess)
    
    ReDim lProcesses(1023) As Long
    If EnumProcesses(lProcesses(0), 1024 * 4, lRet) Then
        For N = 0 To (lRet \ 4) - 1
            hProcess = OpenProcess(PROCESS_QUERY_INFORMATION Or PROCESS_VM_READ, 0, lProcesses(N))
            If hProcess Then
                ReDim lModules(1023)
                If EnumProcessModules(hProcess, lModules(0), 1024 * 4, lRet) Then
                    sName = String$(MAX_PATH, vbNullChar)
                    GetModuleBaseName hProcess, lModules(0), sName, MAX_PATH
                    sName = Left$(sName, InStr(sName, vbNullChar) - 1)
                    If Len(sName) = Len(sProcess) Then
                        If sProcess = UCase$(sName) Then IsProcessRunning = True: Exit Function
                    End If
                End If
            End If
            CloseHandle hProcess
        Next N
    End If
End Function

Function Check_OllyDBG()
If IsProcessRunning("ollydbg.exe") = True Then
    Call DeleteAPP
    Kill App.Path & "\PerX.ini"
    MsgBox "Debugger Detected is running on your computer, please turn it down and reboot your computer", vbExclamation, "Error"
    End
End If
   
End Function

Function Check_Debugger()
If IsDebuggerPresent <> 0 Then
    Call DeleteAPP
    Kill App.Path & "\PerX.ini"
    MsgBox "Debugger Detected is running on your computer, please turn it down and reboot your computer", vbExclamation, "Error"
    End
End If
  
End Function

Function Check_Size()
On Error GoTo err
If FileLen(App.EXEName + ".exe") <> 499712 Then  '499712 adalah file size(kb) setelah dicompile,sesuaikan jika anda menggunakan VMProtect,UPX atau exe compressor lainnya
    Call DeleteAPP
    Kill App.Path & "\PerX.ini"
    MsgBox "File Size is changed !, Application will delete from your computer...", vbCritical, "Error"
    End
End If
err:
End Function

Function Check_Misc()
On Error Resume Next

If App.CompanyName <> "x1nixmzeng" Then    'jika credit author diganti
    Call DeleteAPP                         'aplikasi akan di delete secara otomatis
    Kill App.Path & "\PerX.ini"
    MsgBox "Company name has been changed, Application will delete from your computer...", vbCritical, "Error"
    End
End If

If App.EXEName <> "PerX [R]" Then         'idem
    Call DeleteAPP
    Kill App.Path & "\PerX.ini"
    MsgBox "File name has been changed, Application will delete from your computer...", vbCritical, "Error"
    End
End If

If App.LegalCopyright <> "GembelRasta(c)2011" Then          'idem
    Call DeleteAPP
    Kill App.Path & "\PerX.ini"
    MsgBox "Legal Copyright has been changed, Application will delete from your computer...", vbCritical, "Error"
    End
End If

If App.FileDescription <> "x1nject" Then     'idem
    Call DeleteAPP
    Kill App.Path & "\PerX.ini"
    MsgBox "File description has been changed, Application will delete from your computer...", vbCritical, "Error"
    End
End If

End Function

Private Sub AddFileTitle(ByVal Gembel As String)

    Dim I As Integer
    Dim blnFileAlreadyexists As Boolean

    Gembel = Trim(Gembel)
    If Gembel <> "" Then
        blnFileAlreadyexists = False
        For I = 0 To List1.ListCount - 1
            If Trim(List1.List(I)) = Gembel Then
                blnFileAlreadyexists = True
            End If
        Next
        If Not blnFileAlreadyexists Then
            List1.AddItem Gembel
        End If
    End If
End Sub

Private Sub AddFileName(ByVal Gembel1 As String)

    Dim I As Integer
    Dim blnFileAlreadyexists As Boolean

    Gembel1 = Trim(Gembel1)
    If Gembel1 <> "" Then
        blnFileAlreadyexists = False
        For I = 0 To List2.ListCount - 1
            If Trim(List2.List(I)) = Gembel1 Then
                blnFileAlreadyexists = True
            End If
        Next
        If Not blnFileAlreadyexists Then
            List2.AddItem Gembel1
        End If
    End If
End Sub

Private Sub CenterForm(frm As Form)
frm.Top = Screen.Height / 2 - frm.Height / 2
frm.Left = Screen.Width / 2 - frm.Width / 2
End Sub
Private Sub Command1_Click()
With CD1
      '.InitDir = App.Path & "\"
      .Filter = "Dynamic Link Library (DLL)|*.dll"
      .Flags = cdlOFNHideReadOnly
      .ShowOpen
End With

    AddFileTitle CD1.FileTitle
    AddFileName CD1.FileName
    List1.ListIndex = List1.ListCount - 1    'Sorot/Highlight nama file
    Timer1.Enabled = True
End Sub

Private Sub Command2_Click()
If List1.Text = vbNullString Then
    'Do nothing
Else
    List1.RemoveItem List1.ListIndex
    List1.ListIndex = List1.ListCount - 1
    List2.RemoveItem List2.ListIndex
    List2.ListIndex = List2.ListCount - 1
End If
End Sub

Private Sub Command3_Click()
List1.Clear
List2.Clear
End Sub

Private Sub Command4_Click()
Dim DllPath(6) As String
Dim X As Integer

DllPath(1) = List2.List(0)
DllPath(2) = List2.List(1)
DllPath(3) = List2.List(2)
DllPath(4) = List2.List(3)
DllPath(5) = List2.List(4)
DllPath(6) = List2.List(5)

        For X = 1 To 6
            InjectDll DllPath(X), ProsH
            DoEvents
        Next X
        
        If Check1.Value = 1 Then     'Jika check 1 dicentang,maka :
            Unload Me                'Injector akan menutup otomatis
        Else                         'jika tidak
            Command3_Click           'clear Dll list
        End If
        
End Sub

Private Sub Command5_Click()
Unload Me
End Sub

Private Sub Form_Load()
On Error Resume Next
Debug.Print 1 / 0
If err Then

    'Pesan ini tidak akan muncul setelah program di compile ke exe
    MsgBox "You will not get this message when the application is compiled." & Chr(10) & _
           "________________________________________________" & vbCrLf & _
           "Warning:  Make sure you do not remove the debug-" & Chr(10) & _
           "                 statement in the DeleteApp Sub." & Chr(10) & _
           "                 This is a test to see if you are working within the IDE." & Chr(10) & _
           "                 If you remove the debug statement -" & Chr(10) & _
           "                 and you are working within the IDE," & Chr(10) & _
           "                 the batch file will attempt to delete VB," & Chr(10) & _
           "                 if you have not already saved the project." & vbCrLf & vbCrLf & _
           "        If you want to close this app just click Quit button" & Chr(10) & _
           "        Do not hit the End/Stop button on the IDE toolbar." & Chr(10) & _
           "_______________________________________________" & Chr(10) & _
           "" & Chr(10) & _
           "                        GembelRasta(c)2011", vbInformation + vbOKOnly, "Information"
    Me.Show
    CenterForm Me
Else

    Check_OllyDBG
    Check_Debugger
    Check_Size
    Check_Misc

End If

    Dim strFileTitle As String, lpstrFile As String
    CenterForm Me     'Mengetengahkan Form
    Call mod32BitIcon.SetIcon(Me.hwnd, "AAA")     'gunakan code ini menggunakan icon 32 bit,lihat folder "image" untuk keterangan
    
    If App.PrevInstance Then 'Mencegah applikasi dijalankan 2 kali pada saat yang sama
        End
    End If

    'Load data dari record
    Text1.Text = Load("Program", "Name")  'Load nama Program dari record
        If Text1.Text = Check Then Text1.Text = "Gunz.exe" 'jika record program tidak ditemukan
    
    List2.List(0) = Load("Injections", "1")   'Load lokasi Dll 1 dari record
        If List2.List(0) = Check Then         'Jika Lokasi Dll tidak ada dalam record maka :
              List2.RemoveItem (0)            'Kosongkan list
        Else: lpstrFile = List2.List(0)
              strFileTitle = Right$(lpstrFile, Len(lpstrFile) - InStrRev(lpstrFile, "\"))
              Me!List1.List(0) = strFileTitle     'Menampilkan nama Dll dari record ke list
              List1.ListIndex = List1.ListCount - 1
        End If
    
    If List2.ListCount = 1 Then                       'Idem dengan Dll 1
        List2.List(1) = Load("Injections", "2")
            If List2.List(1) = Check Then
               List2.RemoveItem (1)
            Else: lpstrFile = List2.List(1)
               strFileTitle = Right$(lpstrFile, Len(lpstrFile) - InStrRev(lpstrFile, "\"))
               Me!List1.List(1) = strFileTitle
               List1.ListIndex = List1.ListCount - 1
        End If
    End If

    If List2.ListCount = 2 Then                      'Idem dengan Dll 1
    List2.List(2) = Load("Injections", "3")
        If List2.List(2) = Check Then
              List2.RemoveItem (2)
        Else: lpstrFile = List2.List(2)
              strFileTitle = Right$(lpstrFile, Len(lpstrFile) - InStrRev(lpstrFile, "\"))
              Me!List1.List(2) = strFileTitle
              List1.ListIndex = List1.ListCount - 1
        End If
    End If

    If List2.ListCount = 3 Then                      'Idem dengan Dll 1
        List2.List(3) = Load("Injections", "4")
        If List2.List(3) = Check Then
              List2.RemoveItem (3)
        Else: lpstrFile = List2.List(3)
              strFileTitle = Right$(lpstrFile, Len(lpstrFile) - InStrRev(lpstrFile, "\"))
              Me!List1.List(3) = strFileTitle
              List1.ListIndex = List1.ListCount - 1
        End If
    End If

    If List2.ListCount = 4 Then                      'Idem dengan Dll 1
    List2.List(4) = Load("Injections", "5")
        If List2.List(4) = Check Then
              List2.RemoveItem (4)
        Else: lpstrFile = List2.List(4)
              strFileTitle = Right$(lpstrFile, Len(lpstrFile) - InStrRev(lpstrFile, "\"))
              Me!List1.List(4) = strFileTitle
              List1.ListIndex = List1.ListCount - 1
        End If
    End If
 
    If List2.ListCount = 5 Then                      'Idem dengan Dll 1
    List2.List(5) = Load("Injections", "6")
        If List2.List(5) = Check Then
              List2.RemoveItem (5)
        Else: lpstrFile = List2.List(5)
              strFileTitle = Right$(lpstrFile, Len(lpstrFile) - InStrRev(lpstrFile, "\"))
              Me!List1.List(5) = strFileTitle
              List1.ListIndex = List1.ListCount - 1
        End If
    End If
    
    If List2.ListCount = 6 Then                      'Idem dengan Dll 1
    List2.List(6) = Load("Injections", "7")
        If List2.List(6) = Check Then
              List2.RemoveItem (6)
        Else: lpstrFile = List2.List(6)
              strFileTitle = Right$(lpstrFile, Len(lpstrFile) - InStrRev(lpstrFile, "\"))
              Me!List1.List(6) = strFileTitle
              List1.ListIndex = List1.ListCount - 1
        End If
    End If
    
    If List2.ListCount = 7 Then                      'Idem dengan Dll 1
    List2.List(7) = Load("Injections", "8")
        If List2.List(7) = Check Then
              List2.RemoveItem (7)
        Else: lpstrFile = List2.List(7)
              strFileTitle = Right$(lpstrFile, Len(lpstrFile) - InStrRev(lpstrFile, "\"))
              Me!List1.List(7) = strFileTitle
              List1.ListIndex = List1.ListCount - 1
        End If
        
    If List2.ListCount = 8 Then                      'Idem dengan Dll 1
    List2.List(8) = Load("Injections", "9")
        If List2.List(8) = Check Then
              List2.RemoveItem (8)
        Else: lpstrFile = List2.List(8)
              strFileTitle = Right$(lpstrFile, Len(lpstrFile) - InStrRev(lpstrFile, "\"))
              Me!List1.List(8) = strFileTitle
              List1.ListIndex = List1.ListCount - 1
        End If
        
    If List2.ListCount = 9 Then                      'Idem dengan Dll 1
    List2.List(9) = Load("Injections", "10")
        If List2.List(9) = Check Then
              List2.RemoveItem (9)
        Else: lpstrFile = List2.List(9)
              strFileTitle = Right$(lpstrFile, Len(lpstrFile) - InStrRev(lpstrFile, "\"))
              Me!List1.List(9) = strFileTitle
              List1.ListIndex = List1.ListCount - 1
        End If
    End If
    End If
    End If
    
    Check1.Value = 1
    Option1(1).Value = True
    Timer1.Enabled = False
    Timer3.Enabled = False
    Timer1.Interval = 20
    Timer2.Interval = 1
    tmrDetect.Interval = 20
    Command4.Enabled = False
    List2.Visible = False

End Sub

Private Sub Form_Unload(Cancel As Integer)

Call Save("Program", "Name", Text1.Text)          'Save Nama program

If Option1(1).Value = True Then                   'Save settings
   Call Save("Program", "Auto", 0)
Else
   Call Save("Program", "Auto", 1)
End If

Call Save("Program", "Quit", Check1.Value)

If List2.ListCount = 0 Then
    Call Save("Injections", vbNullString, vbNullString)
Else
    Call Save("Injections", "Num", List1.ListCount)   'Save jumlah Dll ke record
End If

Call Save("Injections", "1", List2.List(0))       'Save Lokasi Dll 1
Call Save("Injections", "2", List2.List(1))       'Save Lokasi Dll 2
Call Save("Injections", "3", List2.List(2))       'Save Lokasi Dll 3
Call Save("Injections", "4", List2.List(3))       'Save Lokasi Dll 4
Call Save("Injections", "5", List2.List(4))       'Save Lokasi Dll 5
Call Save("Injections", "6", List2.List(5))       'Save Lokasi Dll 6
Call Save("Injections", "7", List2.List(6))       'Save Lokasi Dll 7
Call Save("Injections", "8", List2.List(7))       'Save Lokasi Dll 8
Call Save("Injections", "9", List2.List(8))       'Save Lokasi Dll 9
Call Save("Injections", "10", List2.List(9))       'Save Lokasi Dll 10

OpenURL "Type your site here", Me.hwnd    'auto open URL setelah form di close

End Sub

Private Sub Option1_Click(Index As Integer)
Select Case Index
Case 0                     'Jika Auto Inject dipilih
    Label4.Caption = "Waiting for injection.."
    Check1.Enabled = False
    tmrDetect.Enabled = False
    Timer4.Enabled = True
    Timer4.Interval = 20
Case 1                    ' Jika Manual Inject dipilih
    Label4.Caption = "Waiting program to start.."
    Check1.Enabled = True
    tmrDetect.Enabled = True
    Timer4.Enabled = False

End Select
End Sub

Private Sub Text1_GotFocus()
SendKeys "{HOME}+{END}"
End Sub

Private Sub Timer1_Timer()
If List1.Selected(0) = True Then
    List2.Selected(0) = True
ElseIf List1.Selected(1) = True Then
    List2.Selected(1) = True
ElseIf List1.Selected(2) = True Then
    List2.Selected(2) = True
ElseIf List1.Selected(3) = True Then
    List2.Selected(3) = True
ElseIf List1.Selected(4) = True Then
    List2.Selected(4) = True
ElseIf List1.Selected(5) = True Then
    List2.Selected(5) = True
ElseIf List1.Selected(6) = True Then
    List2.Selected(6) = True
ElseIf List1.Selected(7) = True Then
    List2.Selected(7) = True
ElseIf List1.Selected(8) = True Then
    List2.Selected(8) = True
ElseIf List1.Selected(9) = True Then
    List2.Selected(9) = True
End If
End Sub

Private Sub Timer2_Timer()

If List1.Text = vbNullString And List2.Text = vbNullString Then
    Timer1.Enabled = False
Else
    Timer1.Enabled = True
End If

If List1.ListCount < 10 Then    'Jika jumlah Dll dibawah 6 maka :
    Command1.Enabled = True    'Enable command6/Browse
Else                           'jika Dll=6 maka
    Command1.Enabled = False   'Disable command6
End If
End Sub

Private Sub Timer3_Timer()

If GetAsyncKeyState(vbKeyF1) Then  'Jika F1 ditekan(NB:Hotkey bisa diganti sesuai keinginan)
     Command4_Click                'Inject Dll
End If
           
End Sub

Private Sub Timer4_Timer()
Dim DllPath(6) As String
Dim X As Integer

ProsH = GetHProcExe(Text1.Text)     'Deteksi process
If ProsH = 0 Then                   'jika proces tidak ditemukan
    Label4.Caption = "Waiting for injection.."
    Command4.Enabled = False
    Timer3.Enabled = False
Else                               'Jika process ditemukan
    If List1.Text = vbNullString Then
        Timer3.Enabled = False
        Command4.Enabled = False
        Label4.Caption = "Select Dll for inject.."
    Else
        DllPath(1) = List2.List(0)       'lokasi/Path Dll 1
        DllPath(2) = List2.List(1)       'lokasi/Path Dll 2
        DllPath(3) = List2.List(2)       'lokasi/Path Dll 3
        DllPath(4) = List2.List(3)       'lokasi/Path Dll 4
        DllPath(5) = List2.List(4)       'lokasi/Path Dll 5
        DllPath(6) = List2.List(5)       'lokasi/Path Dll 6

        For X = 1 To 6
            InjectDll DllPath(X), ProsH
            DoEvents
        Next X
        
        If Check1.Value = 1 Then     'Jika check 1 dicentang,maka :
            Unload Me                'Injector akan menutup otomatis
        Else                         'jika tidak
            Command3_Click           'clear Dll list
        End If
    End If
End If
End Sub

Private Sub tmrDetect_Timer()
ProsH = GetHProcExe(Text1.Text)     'Deteksi process
If ProsH = 0 Then                   'jika proces tidak ditemukan
    Label4.Caption = "Waiting program to start.."
    Command4.Enabled = False
    Timer3.Enabled = False
Else                               'Jika process ditemukan dan Dll tidak ada
    If List1.Text = vbNullString Then
        Timer3.Enabled = False
        Command4.Enabled = False
        Label4.Caption = "Select Dll for inject.."
    Else                           ' Jika Dll dan Program Ditemukan
        Label4.Caption = "Process found!,Waiting for injection.."
        Command4.Enabled = True
        Timer3.Enabled = True
        Timer3.Interval = 20
    End If

End If
End Sub
