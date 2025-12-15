VERSION 5.00
Object = "{248DD890-BB45-11CF-9ABC-0080C7E7B78D}#1.0#0"; "MSWINSCK.OCX"
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "ComDlg32.OCX"
Object = "{48E59290-9880-11CF-9754-00AA00C00908}#1.0#0"; "MSINET.ocx"
Begin VB.Form Form2 
   BackColor       =   &H00404040&
   BorderStyle     =   0  'None
   Caption         =   "IndoRhm | Cheat Lost Saga Indonesia"
   ClientHeight    =   4875
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   5535
   LinkTopic       =   "Form2"
   ScaleHeight     =   4875
   ScaleMode       =   0  'User
   ScaleWidth      =   6591.018
   StartUpPosition =   2  'CenterScreen
   Begin RxCheat_Production.ProgressBar ProgressBar1 
      Height          =   255
      Left            =   7200
      TabIndex        =   32
      Top             =   2280
      Width           =   3975
      _ExtentX        =   7011
      _ExtentY        =   450
      BrushStyle      =   0
      Color           =   16750899
      Color2          =   16750899
   End
   Begin RxCheat_Production.dcButton dcButton2 
      Height          =   495
      Left            =   2880
      TabIndex        =   30
      Top             =   3240
      Width           =   1695
      _ExtentX        =   2990
      _ExtentY        =   873
      ButtonStyle     =   11
      Caption         =   "About"
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial Narrow"
         Size            =   11.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      MouseIcon       =   "Form2.frx":0000
      MousePointer    =   99
   End
   Begin RxCheat_Production.dcButton cmdDownload 
      Height          =   495
      Left            =   960
      TabIndex        =   29
      Top             =   3240
      Width           =   1695
      _ExtentX        =   2990
      _ExtentY        =   873
      ButtonStyle     =   11
      Caption         =   "Update"
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "Arial Narrow"
         Size            =   11.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   0
      MouseIcon       =   "Form2.frx":0162
      MousePointer    =   99
   End
   Begin RxCheat_Production.dcButton dcButton3 
      Height          =   495
      Left            =   4200
      TabIndex        =   28
      Top             =   240
      Width           =   495
      _ExtentX        =   873
      _ExtentY        =   873
      ButtonStyle     =   11
      Caption         =   "-"
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   29.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      MouseIcon       =   "Form2.frx":02C4
      MousePointer    =   99
   End
   Begin RxCheat_Production.dcButton dcButton1 
      Height          =   495
      Left            =   4800
      TabIndex        =   27
      Top             =   240
      Width           =   495
      _ExtentX        =   873
      _ExtentY        =   873
      ButtonStyle     =   11
      Caption         =   "X"
      BeginProperty Font {0BE35203-8F91-11CE-9DE3-00AA004BB851} 
         Name            =   "MS Sans Serif"
         Size            =   13.5
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      MouseIcon       =   "Form2.frx":0426
      MousePointer    =   99
   End
   Begin VB.Timer Timer1 
      Left            =   6360
      Top             =   240
   End
   Begin VB.Timer tmrDetect 
      Left            =   7800
      Top             =   720
   End
   Begin VB.Timer Timer7 
      Interval        =   25
      Left            =   7320
      Top             =   720
   End
   Begin VB.Timer Timer6 
      Left            =   6840
      Top             =   720
   End
   Begin VB.Timer Timer5 
      Left            =   6360
      Top             =   720
   End
   Begin VB.Timer Timer4 
      Left            =   7800
      Top             =   240
   End
   Begin VB.Timer Timer3 
      Left            =   7320
      Top             =   240
   End
   Begin VB.Timer Timer2 
      Left            =   6840
      Top             =   240
   End
   Begin RxCheat_Production.ProgressBar LOS 
      Height          =   255
      Left            =   7200
      TabIndex        =   24
      Top             =   1920
      Width           =   3975
      _ExtentX        =   7011
      _ExtentY        =   450
      BrushStyle      =   0
      Color           =   16750899
      Color2          =   16750899
   End
   Begin VB.ListBox List2 
      Height          =   255
      Left            =   7800
      TabIndex        =   23
      Top             =   3000
      Width           =   1815
   End
   Begin VB.ListBox List1 
      Height          =   255
      Left            =   7800
      TabIndex        =   22
      Top             =   2640
      Width           =   1815
   End
   Begin InetCtlsObjects.Inet Inet1 
      Left            =   10080
      Top             =   720
      _ExtentX        =   1005
      _ExtentY        =   1005
      _Version        =   393216
   End
   Begin VB.CheckBox Check1 
      Caption         =   "Quit After Injections"
      Height          =   375
      Left            =   9000
      TabIndex        =   4
      Top             =   240
      Width           =   1815
   End
   Begin MSComDlg.CommonDialog CD1 
      Left            =   9480
      Top             =   720
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
   End
   Begin VB.Timer SUBtimer 
      Interval        =   100
      Left            =   8400
      Top             =   360
   End
   Begin MSWinsockLib.Winsock Winsock1 
      Left            =   9000
      Top             =   720
      _ExtentX        =   741
      _ExtentY        =   741
      _Version        =   393216
   End
   Begin VB.Frame Frame4 
      BackColor       =   &H00404040&
      Enabled         =   0   'False
      Height          =   615
      Left            =   120
      TabIndex        =   19
      Top             =   4100
      Width           =   5295
      Begin VB.Label Label17 
         BackColor       =   &H00404040&
         Caption         =   "Copyright © IndoRhm 2018  -  All Right Reserved"
         BeginProperty Font 
            Name            =   "Arial Narrow"
            Size            =   11.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00C0C0C0&
         Height          =   375
         Left            =   720
         TabIndex        =   20
         Top             =   195
         Width           =   4215
      End
   End
   Begin VB.Frame Frame3 
      BackColor       =   &H00404040&
      Enabled         =   0   'False
      Height          =   1215
      Left            =   120
      TabIndex        =   18
      Top             =   2820
      Width           =   5295
      Begin VB.Label Label5 
         BackColor       =   &H00404040&
         Caption         =   "Waiting lostsaga.exe"
         BeginProperty Font 
            Name            =   "Arial Narrow"
            Size            =   26.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   -1  'True
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   615
         Left            =   480
         TabIndex        =   31
         Top             =   360
         Visible         =   0   'False
         Width           =   4335
      End
   End
   Begin VB.Frame Frame2 
      BackColor       =   &H00404040&
      Enabled         =   0   'False
      Height          =   1920
      Left            =   120
      TabIndex        =   2
      Top             =   840
      Width           =   5295
      Begin VB.Label lblReceived 
         BackColor       =   &H00404040&
         BeginProperty Font 
            Name            =   "Arial Narrow"
            Size            =   11.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00C0C0C0&
         Height          =   255
         Left            =   2400
         TabIndex        =   26
         Top             =   1455
         Width           =   1695
      End
      Begin VB.Label Label15 
         BackColor       =   &H00404040&
         Caption         =   "IP Address"
         BeginProperty Font 
            Name            =   "Arial Narrow"
            Size            =   11.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00C0C0C0&
         Height          =   255
         Left            =   2400
         TabIndex        =   17
         Top             =   1155
         Width           =   1935
      End
      Begin VB.Label Label14 
         BackColor       =   &H00404040&
         Caption         =   "Nama PC"
         BeginProperty Font 
            Name            =   "Arial Narrow"
            Size            =   11.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00C0C0C0&
         Height          =   255
         Left            =   2400
         TabIndex        =   16
         Top             =   855
         Width           =   2295
      End
      Begin VB.Label LblJam 
         BackColor       =   &H00404040&
         Caption         =   "Jam"
         BeginProperty Font 
            Name            =   "Arial Narrow"
            Size            =   11.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00C0C0C0&
         Height          =   255
         Left            =   2400
         TabIndex        =   15
         Top             =   555
         Width           =   1815
      End
      Begin VB.Label LblTgl 
         BackColor       =   &H00404040&
         Caption         =   "Tanggal"
         BeginProperty Font 
            Name            =   "Arial Narrow"
            Size            =   11.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00C0C0C0&
         Height          =   375
         Left            =   2400
         TabIndex        =   14
         Top             =   240
         Width           =   1815
      End
      Begin VB.Label Label11 
         BackColor       =   &H00404040&
         Caption         =   ":"
         BeginProperty Font 
            Name            =   "Arial Narrow"
            Size            =   11.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00C0C0C0&
         Height          =   255
         Left            =   2160
         TabIndex        =   13
         Top             =   1455
         Width           =   135
      End
      Begin VB.Label Label10 
         BackColor       =   &H00404040&
         Caption         =   ":"
         BeginProperty Font 
            Name            =   "Arial Narrow"
            Size            =   11.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00C0C0C0&
         Height          =   255
         Left            =   2160
         TabIndex        =   12
         Top             =   1155
         Width           =   135
      End
      Begin VB.Label Label9 
         BackColor       =   &H00404040&
         Caption         =   ":"
         BeginProperty Font 
            Name            =   "Arial Narrow"
            Size            =   11.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00C0C0C0&
         Height          =   255
         Left            =   2160
         TabIndex        =   11
         Top             =   855
         Width           =   135
      End
      Begin VB.Label Label8 
         BackColor       =   &H00404040&
         Caption         =   ":"
         BeginProperty Font 
            Name            =   "Arial Narrow"
            Size            =   11.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00C0C0C0&
         Height          =   255
         Left            =   2160
         TabIndex        =   10
         Top             =   555
         Width           =   135
      End
      Begin VB.Label Label7 
         BackColor       =   &H00404040&
         Caption         =   ":"
         BeginProperty Font 
            Name            =   "Arial Narrow"
            Size            =   11.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00C0C0C0&
         Height          =   255
         Left            =   2160
         TabIndex        =   9
         Top             =   240
         Width           =   135
      End
      Begin VB.Label Label6 
         BackColor       =   &H00404040&
         Caption         =   "• Size"
         BeginProperty Font 
            Name            =   "Arial Narrow"
            Size            =   11.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00C0C0C0&
         Height          =   255
         Left            =   240
         TabIndex        =   8
         Top             =   1455
         Width           =   735
      End
      Begin VB.Label lblip 
         BackColor       =   &H00404040&
         Caption         =   "• Your IP Address"
         BeginProperty Font 
            Name            =   "Arial Narrow"
            Size            =   11.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00C0C0C0&
         Height          =   255
         Left            =   240
         TabIndex        =   7
         Top             =   1155
         Width           =   1575
      End
      Begin VB.Label lblpc 
         BackColor       =   &H00404040&
         Caption         =   "• Your PC Name"
         BeginProperty Font 
            Name            =   "Arial Narrow"
            Size            =   11.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00C0C0C0&
         Height          =   255
         Left            =   240
         TabIndex        =   6
         Top             =   855
         Width           =   1455
      End
      Begin VB.Label Label3 
         BackColor       =   &H00404040&
         Caption         =   "• Time"
         BeginProperty Font 
            Name            =   "Arial Narrow"
            Size            =   11.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00C0C0C0&
         Height          =   255
         Left            =   240
         TabIndex        =   5
         Top             =   550
         Width           =   615
      End
      Begin VB.Label Label2 
         BackColor       =   &H00404040&
         Caption         =   "• Date"
         BeginProperty Font 
            Name            =   "Arial Narrow"
            Size            =   11.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00C0C0C0&
         Height          =   255
         Left            =   240
         TabIndex        =   3
         Top             =   240
         Width           =   615
      End
   End
   Begin VB.Frame Frame1 
      BackColor       =   &H00404040&
      Enabled         =   0   'False
      Height          =   735
      Left            =   120
      TabIndex        =   0
      Top             =   60
      Width           =   5295
      Begin VB.Label RxCheat 
         BackColor       =   &H00404040&
         Caption         =   "IndoRhm | Cheat LSID"
         BeginProperty Font 
            Name            =   "Arial Narrow"
            Size            =   12.75
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00C0C0C0&
         Height          =   375
         Left            =   240
         TabIndex        =   1
         Top             =   240
         Width           =   2415
      End
   End
   Begin VB.Label lblTotal 
      Height          =   375
      Left            =   6720
      TabIndex        =   25
      Top             =   1320
      Width           =   855
   End
   Begin VB.Label Label4 
      Height          =   255
      Left            =   8040
      TabIndex        =   21
      Top             =   1440
      Width           =   2655
   End
End
Attribute VB_Name = "Form2"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'============================================================'
' Injector By : IndoRhm
' Created Date : 1 Januari 2018
' Author : Fauzan
' Model nya Hampir mirip sama Injector Rhm-Files
' Source Code ini tidak boleh di share/dibagikan ke situs lain tanpa izin dari Author (Fauzan)
' Please jangan rename Author, hargailah karya orang jika karya mu ingin di hargai!!
' Please Visit My Blog : www.indorhm.blogspot.com
'============================================================'

Private Const MAX_COMPUTERNAME_LENGTH As Long = 31
Private Declare Function GetComputerName Lib "kernel32" Alias "GetComputerNameA" (ByVal lpBuffer As String, nSize As Long) As Long

Option Explicit
Dim move_x As Integer, move_y As Integer

Private winHwnd   As Long
Private winHwnd1   As Long
Private NamaDll   As String
Private Const WM_NCLBUTTONDOWN    As Long = &HA1
Private Const HTCAPTION           As Integer = 2
Private Declare Function SendMessage Lib "user32" Alias "SendMessageA" (ByVal hWnd As Long, _
                                                                        ByVal wMsg As Long, _
                                                                        ByVal wParam As Long, _
                                                                        lParam As Any) As Long
Private Declare Function GetWindowLong Lib "user32" Alias "GetWindowLongA" (ByVal hWnd As Long, ByVal nIndex As Long) As Long
Private Declare Function SetWindowLong Lib "user32" Alias "SetWindowLongA" (ByVal hWnd As Long, ByVal nIndex As Long, ByVal dwNewLong As Long) As Long
Private Declare Function SetLayeredWindowAttributes Lib "user32" (ByVal hWnd As Long, ByVal crKey As Long, ByVal bAlpha As Byte, ByVal dwFlags As Long) As Long
Private Const GWL_STYLE = (-16)
Private Const GWL_EXSTYLE = (-20)
Private Const WS_EX_LAYERED = &H80000
Private Const LWA_COLORKEY = &H1
Private Const LWA_ALPHA = &H2
Private Declare Function ReleaseCapture Lib "user32.dll" () As Long
Dim Color As Long, flag As Byte

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
    Dim lProcesses() As Long, lModules() As Long, n As Long, lRet As Long, hProcess As Long
    Dim sName As String
    
    sProcess = UCase$(sProcess)
    ReDim lProcesses(1023) As Long
    If EnumProcesses(lProcesses(0), 1024 * 4, lRet) Then
        For n = 0 To (lRet \ 4) - 1
            hProcess = OpenProcess(PROCESS_QUERY_INFORMATION Or PROCESS_VM_READ, 0, lProcesses(n))
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
        Next n
    End If
End Function

Private Sub AddFileTitle(ByVal Gembel As String)

    Dim i As Integer
    Dim blnFileAlreadyexists As Boolean

    Gembel = Trim(Gembel)
    If Gembel <> "" Then
        blnFileAlreadyexists = False
        For i = 0 To List1.ListCount - 1
            If Trim(List1.List(i)) = Gembel Then
                blnFileAlreadyexists = True
            End If
        Next
        If Not blnFileAlreadyexists Then
            List1.AddItem Gembel
        End If
    End If
End Sub

Private Sub AddFileName(ByVal Gembel1 As String)

    Dim i As Integer
    Dim blnFileAlreadyexists As Boolean

    Gembel1 = Trim(Gembel1)
    If Gembel1 <> "" Then
        blnFileAlreadyexists = False
        For i = 0 To List2.ListCount - 1
            If Trim(List2.List(i)) = Gembel1 Then
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

Private Sub dcButton1_Click()
Unload Me
End Sub

Private Sub dcButton2_Click()
    ShellExecute Me.hWnd, "open", "http://www.indorhm.blogspot.com/", vbNullString, "", 0
End Sub

Private Sub dcButton3_Click()
Form2.WindowState = vbMinimized
End Sub

Private Sub Form_Load()
    Dim dwLen As Long
    Dim strString As String
    dwLen = MAX_COMPUTERNAME_LENGTH + 1
    strString = String(dwLen, "X")
    GetComputerName strString, dwLen
    strString = Left(strString, dwLen)
    Label14.Caption = "" & Winsock1.LocalHostName & ""
    Label15.Caption = "" & Winsock1.LocalIP & vbCrLf & ""
    
On Error Resume Next
Debug.Print 1 / 0
If Err Then
Dim i As Long

    CenterForm Me

End If

    Dim strFileTitle As String, lpstrFile As String
    CenterForm Me
    Call mod32BitIcon.SetIcon(Me.hWnd, "AAA")
    
    If App.PrevInstance Then
        End
    End If

    
    List2.List(0) = "C:\Windows\System32\RXCheat.dll"
        If List2.List(0) = Check Then
              List2.RemoveItem (0)
        Else: lpstrFile = List2.List(0)
              strFileTitle = Right$(lpstrFile, Len(lpstrFile) - InStrRev(lpstrFile, "\"))
              Me!List1.List(0) = strFileTitle
              List1.ListIndex = List1.ListCount - 1
        End If
        
    Check1.Value = 1

 i = 0
   Timer5.Interval = 10
   Timer6.Interval = 2000
   LOS.Visible = False

End Sub

Private Sub Timer1_Timer()
If List1.Selected(0) = True Then
    List2.Selected(0) = True
ElseIf List1.Selected(1) = True Then
    List2.Selected(1) = True
ElseIf List1.Selected(2) = True Then
    List2.Selected(2) = True
End If
End Sub

Private Sub Timer2_Timer()

If List1.Text = vbNullString And List2.Text = vbNullString Then
    Timer1.Enabled = False
Else
    Timer1.Enabled = True
End If

End Sub

Private Sub Timer3_Timer()

Timer4.Enabled = True
           
End Sub

Private Sub Timer4_Timer()
Dim DllPath(6) As String
Dim X As Integer

ProsH = GetHProcExe("lostsaga.exe")
If ProsH = 0 Then

    Label4.Caption = "Waiting for injection.."
    Timer3.Enabled = False
Else
    If List1.Text = vbNullString Then
        Timer3.Enabled = False
        Label4.Caption = "Select Dll for inject.."
    Else
        DllPath(1) = List2.List(0)
        DllPath(2) = List2.List(1)

        For X = 1 To 2
            InjectDll DllPath(X), ProsH
            DoEvents
        Next X
        
        If Check1.Value = 1 Then
            Unload Me

        End If
    End If
End If
End Sub

Private Sub Timer5_Timer()
LOS.Value = LOS.Value + 1
If LOS.Value = LOS.Max Then
LOS.Value = LOS.Max
Timer5.Enabled = False
Timer6.Enabled = True
LOS.Value = 0
End If
End Sub

Private Sub Timer6_Timer()

Timer6.Enabled = False
Timer5.Enabled = True

End Sub

Private Sub tmrDetect_Timer()
ProsH = GetHProcExe("lostsaga.exe")
If ProsH = 0 Then
    Timer3.Enabled = False
Else
    If List1.Text = vbNullString Then
        Timer3.Enabled = False
    Else
        Label4.Caption = "Process found!,Waiting for injection.."
        Timer3.Enabled = True
        Timer3.Interval = 20
    End If
End If
End Sub

Private Sub cmdDownload_Click()
Screen.MousePointer = vbHourglass

ProgressBar1.Value = 0

LOS.Visible = False
ProgressBar1.Visible = True

Dim Download As String

DownloadFile "https://sites.google.com/site/cobacobww1213/RXCheat.dll?attredirects=0&d=1", "C:\" & "\Windows\System32\RXCheat.dll"
MsgBox "Silahkan Login Lost Saga nya...!!!", vbInformation, "Info"
cmdDownload.Visible = False
dcButton2.Visible = False
Label5.Visible = True
Screen.MousePointer = vbDefault
    LOS.Visible = True
    Timer5.Enabled = True
    Timer6.Enabled = False
    Timer4.Enabled = False
    Timer4.Interval = 20
    Timer1.Enabled = False
    Timer3.Enabled = False
    Timer1.Interval = 20
    Timer2.Interval = 1
    tmrDetect.Interval = 20
    List2.Visible = False
    cmdDownload.Enabled = False
    Check1.Enabled = False
    tmrDetect.Enabled = True

ProgressBar1.Visible = False

End Sub

Sub DownloadProgress(intPercent As String)
    ProgressBar1.Value = intPercent
    
    If ProgressBar1.Value = 30 Then
    ProgressBar1.Color = &H80FF&
    ProgressBar1.Color2 = &H4080&
    End If
    If ProgressBar1.Value = 60 + 1 Then
    ProgressBar1.Color = &HFFFF&
    ProgressBar1.Color2 = &H8080&
    End If
    If ProgressBar1.Value = 80 Then
    ProgressBar1.Color = &HFF00&
    ProgressBar1.Color2 = &H8000&
    End If
    If ProgressBar1.Value = 100 Then
    ProgressBar1.Color = &HFF00&
    ProgressBar1.Color2 = &H8000&
    End If
    If ProgressBar1.Value = 30 Then
    ProgressBar1.Color = &H0&
    ProgressBar1.Color2 = &H0&
    End If

End Sub

Public Sub DownloadFile(strURL As String, strDestination As String)
Const CHUNK_SIZE As Long = 1024
Dim intFile As Integer
Dim lngBytesReceived As Long
Dim lngFileLength As Long
Dim strHeader As String
Dim B() As Byte
Dim i As Integer
DoEvents
    
With Inet1
    
.URL = strURL
.Execute , "GET", , "Range: bytes=" & CStr(lngBytesReceived) & "-" & vbCrLf
        
While .StillExecuting
DoEvents
Wend

strHeader = .GetHeader
End With
    
    
strHeader = Inet1.GetHeader("Content-Length")
lngFileLength = Val(strHeader)

DoEvents
lngBytesReceived = 0

intFile = FreeFile()

Open strDestination For Binary Access Write As #intFile

Do
B = Inet1.GetChunk(CHUNK_SIZE, icByteArray)
Put #intFile, , B
lngBytesReceived = lngBytesReceived + UBound(B, 1) + 1

 lblReceived.Caption = Format$(lngBytesReceived / 1024, "0") & " Kb"
 lblTotal.Caption = "Dari : " & Format$(lngFileLength / 1024, " 0") & " Kb"
DownloadProgress (Round((lngBytesReceived / lngFileLength) * 100))
DoEvents
Loop While UBound(B, 1) > 0

Close #intFile
 
 End Sub

Private Sub SUBtimer_Timer()
    LblTgl.Caption = Date
    LblJam.Caption = Time
End Sub

Private Sub Form_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
  move_x = X
  move_y = Y
End Sub

Private Sub Form_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
  If Button = vbLeftButton Then
    Me.Left = Me.Left - (move_x - X)
    Me.Top = Me.Top - (move_y - Y)
  End If
End Sub
