VERSION 5.00
Object = "{F9043C88-F6F2-101A-A3C9-08002B2F49FB}#1.2#0"; "COMDLG32.OCX"
Begin VB.Form frmMain 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "C4 MP3 Compiler v1.00"
   ClientHeight    =   2550
   ClientLeft      =   45
   ClientTop       =   435
   ClientWidth     =   5055
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "frmMain.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   2550
   ScaleWidth      =   5055
   StartUpPosition =   2  'CenterScreen
   Begin VB.PictureBox pibSRC 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      ForeColor       =   &H80000008&
      Height          =   615
      Left            =   360
      ScaleHeight     =   585
      ScaleWidth      =   4305
      TabIndex        =   4
      Top             =   240
      Width           =   4335
      Begin VB.TextBox txtSRC 
         Alignment       =   2  'Center
         Appearance      =   0  'Flat
         BackColor       =   &H00C0E0FF&
         BeginProperty Font 
            Name            =   "Tahoma"
            Size            =   12
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   375
         Left            =   120
         Locked          =   -1  'True
         TabIndex        =   6
         Text            =   $"frmMain.frx":57E2
         Top             =   120
         Width           =   3135
      End
      Begin VB.CommandButton cmdBrowse 
         Caption         =   "..."
         BeginProperty Font 
            Name            =   "Tahoma"
            Size            =   14.25
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   375
         Left            =   3360
         TabIndex        =   5
         Top             =   120
         Width           =   855
      End
   End
   Begin VB.Timer Timer1 
      Left            =   4080
      Top             =   1080
   End
   Begin VB.PictureBox picCompile 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      ForeColor       =   &H80000008&
      Height          =   495
      Left            =   360
      ScaleHeight     =   465
      ScaleWidth      =   4305
      TabIndex        =   2
      Top             =   1320
      Width           =   4335
      Begin VB.CommandButton cmdCompile 
         BackColor       =   &H00C0C0FF&
         Caption         =   "&Compile to EXECUTABLE Mp3's"
         BeginProperty Font 
            Name            =   "Tahoma"
            Size            =   8.25
            Charset         =   0
            Weight          =   700
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   495
         Left            =   0
         MaskColor       =   &H00FFC0C0&
         Style           =   1  'Graphical
         TabIndex        =   3
         Top             =   0
         Width           =   4335
      End
   End
   Begin VB.PictureBox picLabel 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   600
      ScaleHeight     =   225
      ScaleWidth      =   3825
      TabIndex        =   0
      Top             =   2160
      Width           =   3855
      Begin VB.Label lbl1 
         Alignment       =   2  'Center
         BackColor       =   &H00FFC0C0&
         Caption         =   "©oded by '©$3'. Thanx to 'Justin' ..."
         BeginProperty Font 
            Name            =   "Tahoma"
            Size            =   9
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         Height          =   255
         Left            =   0
         TabIndex        =   1
         Top             =   0
         Width           =   3855
      End
   End
   Begin MSComDlg.CommonDialog cdlS 
      Left            =   0
      Top             =   2040
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
      DefaultExt      =   "*.ExE"
      DialogTitle     =   "Save Executable Mp3 Files..."
      Filter          =   "Executable Mp3 Files (*.ExE)|*.ExE"
   End
   Begin MSComDlg.CommonDialog cdlO 
      Left            =   4560
      Top             =   2040
      _ExtentX        =   847
      _ExtentY        =   847
      _Version        =   393216
      DefaultExt      =   "*.Mp3"
      DialogTitle     =   "Open Mp3 Files..."
      Filter          =   "Mp3 Files (*.Mp3)|*.Mp3"
   End
End
Attribute VB_Name = "frmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim X(100), Y(100), xSpeed(100), ySpeed(100) As Integer
Private Sub cmdCompile_Click()
On Error GoTo Errh

cdlS.ShowSave

Call Disable

RUNTIME = App.Path & "\c4r_mp310.dll"
Mp3FILE = cdlS.FileName
FileCopy RUNTIME, Mp3FILE
PUTMp3 = "<%MP%>" & GetFile(txtSRC.Text)

File1$ = Mp3FILE
File2$ = RUNTIME

Open File1$ For Output As #1        'Open Application
Open File2$ For Binary As #2        'Open DLL File

Do While Not EOF(2)
FileData = Input$(8000, #2)
Mp3Msg = FileData
Mp3Msg2 = Mp3Msg2 + Mp3Msg
Print #1, Mp3Msg2;
Mp3Msg2 = ""
Loop
Print #1, PUTMp3
Close #2
Close #1

MsgBox "Sucess...", vbInformation

Call Enable

Exit Sub
Errh:
    MsgBox Err.Description, , Err.Number
    Call Enable
End Sub

Private Sub cmdBrowse_Click()
On Error GoTo Errh

cdlO.ShowOpen
txtSRC.Text = cdlO.FileName
Exit Sub

Errh:
MsgBox Err.Description, vbInformation, Err.Number
End Sub

Public Function GetFile(FileName As String) As String
On Error Resume Next
Dim FileNumber
Dim TextData
    FileNumber = FreeFile
        If Len(FileName) Then
            Open FileName For Binary As #FileNumber
                TextData = Input(LOF(FileNumber), #FileNumber)
                DoEvents
            Close #FileNumber
        End If
        GetFile = TextData
End Function

Private Sub Form_Load()
txtSRC.Text = App.Path & "\Test.mp3"

Dim i As Integer
    Timer1.Interval = 4
        Me.BackColor = vbBlack
        Me.ForeColor = vbBlack
        Me.FillColor = vbBlack
For i = 0 To 99
            X(i) = -1
            Y(i) = -1
Next i

Randomize
Timer1.Enabled = True
End Sub

Private Sub Form_Unload(Cancel As Integer)
MsgBox "Program by '©$3'", , ""
End Sub

Sub Disable()
Me.cmdCompile.Enabled = False
Me.cmdBrowse.Enabled = False
Me.txtSRC.Enabled = False
End Sub

Sub Enable()
Me.cmdCompile.Enabled = True
Me.cmdBrowse.Enabled = True
Me.txtSRC.Enabled = True
End Sub

Private Sub Timer1_Timer()
   Dim i As Integer
    For i = 0 To 99
        PSet (X(i), Y(i)), &H0&
        If X(i) < 0 Or X(i) > Me.ScaleWidth Or Y(i) < 0 Or Y(i) > Me.ScaleHeight Then
            X(i) = Me.ScaleWidth \ 2
            Y(i) = Me.ScaleHeight \ 2
            xSpeed(i) = Int(Rnd(1) * 200) - 100
            ySpeed(i) = Int(Rnd(1) * 200) - 100
        End If
        X(i) = X(i) + xSpeed(i)
        Y(i) = Y(i) + ySpeed(i)
        PSet (X(i), Y(i)), vbWhite
Next i
End Sub
