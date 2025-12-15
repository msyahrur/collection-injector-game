VERSION 5.00
Begin VB.Form Form1 
   BackColor       =   &H00404040&
   BorderStyle     =   0  'None
   Caption         =   "IndoRhm | Cheat Lost Saga Indonesia"
   ClientHeight    =   3225
   ClientLeft      =   0
   ClientTop       =   0
   ClientWidth     =   5415
   LinkTopic       =   "Form1"
   ScaleHeight     =   3225
   ScaleMode       =   0  'User
   ScaleWidth      =   5269.435
   StartUpPosition =   2  'CenterScreen
   Begin VB.Timer Timer1 
      Interval        =   100
      Left            =   5520
      Top             =   360
   End
   Begin VB.Frame Frame3 
      BackColor       =   &H00404040&
      Enabled         =   0   'False
      Height          =   615
      Left            =   120
      TabIndex        =   4
      Top             =   2470
      Width           =   5175
      Begin VB.Label Label2 
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
         Left            =   570
         TabIndex        =   5
         Top             =   195
         Width           =   4215
      End
   End
   Begin VB.Frame Frame2 
      BackColor       =   &H00404040&
      Enabled         =   0   'False
      Height          =   1560
      Left            =   120
      TabIndex        =   2
      Top             =   850
      Width           =   5175
      Begin RxCheat_Production.ProgressBar ProgressBar1 
         Height          =   255
         Left            =   240
         TabIndex        =   3
         Top             =   1050
         Width           =   4695
         _ExtentX        =   8281
         _ExtentY        =   450
         BrushStyle      =   0
         Color           =   4210752
         Style           =   6
         Color2          =   16750899
      End
      Begin VB.Label Label7 
         BackColor       =   &H00404040&
         BeginProperty Font 
            Name            =   "Arial Narrow"
            Size            =   9.75
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00C0C0C0&
         Height          =   255
         Left            =   1230
         TabIndex        =   10
         Top             =   600
         Width           =   975
      End
      Begin VB.Label Label6 
         BackColor       =   &H00404040&
         Caption         =   ":"
         BeginProperty Font 
            Name            =   "Arial Narrow"
            Size            =   9.75
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00C0C0C0&
         Height          =   255
         Left            =   1080
         TabIndex        =   9
         Top             =   600
         Width           =   135
      End
      Begin VB.Label Label5 
         BackColor       =   &H00404040&
         Caption         =   ":  Loading..."
         BeginProperty Font 
            Name            =   "Arial Narrow"
            Size            =   9.75
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00C0C0C0&
         Height          =   255
         Left            =   1080
         TabIndex        =   8
         Top             =   320
         Width           =   1215
      End
      Begin VB.Label Label4 
         BackColor       =   &H00404040&
         Caption         =   "• Instal"
         BeginProperty Font 
            Name            =   "Arial Narrow"
            Size            =   9.75
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
         Top             =   600
         Width           =   615
      End
      Begin VB.Label Label3 
         BackColor       =   &H00404040&
         Caption         =   "• Status"
         BeginProperty Font 
            Name            =   "Arial Narrow"
            Size            =   9.75
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
         Top             =   315
         Width           =   735
      End
   End
   Begin VB.Frame Frame1 
      BackColor       =   &H00404040&
      Enabled         =   0   'False
      Height          =   735
      Left            =   120
      TabIndex        =   0
      Top             =   60
      Width           =   5175
      Begin VB.Label Label1 
         BackColor       =   &H00404040&
         Caption         =   "IndoRhm | Cheat LSID "
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
End
Attribute VB_Name = "Form1"
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

Option Explicit
Dim move_x As Integer, move_y As Integer

Private Sub Timer1_Timer()
    Me.ProgressBar1.Value = Me.ProgressBar1.Value + 1
    Me.Label7.Caption = Me.ProgressBar1.Value & "%"
    If Me.ProgressBar1.Value = 100 Then
    Unload Me
    MsgBox "Silahkan Klik Update Pada Injector & Login Lost Saga nya...!!!", vbInformation, ""
    Form2.Show
    Unload Me
    End If
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
