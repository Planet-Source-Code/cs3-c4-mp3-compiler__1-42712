VERSION 5.00
Begin VB.Form frmLoader 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "loading..."
   ClientHeight    =   1815
   ClientLeft      =   45
   ClientTop       =   435
   ClientWidth     =   4695
   ControlBox      =   0   'False
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   8.25
      Charset         =   0
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "frmLoader.frx":0000
   LinkTopic       =   "Form1"
   MaxButton       =   0   'False
   MinButton       =   0   'False
   ScaleHeight     =   1815
   ScaleWidth      =   4695
   StartUpPosition =   2  'CenterScreen
   Begin VB.Timer Timer3 
      Interval        =   250
      Left            =   2760
      Top             =   720
   End
   Begin VB.Timer Timer2 
      Interval        =   150
      Left            =   3480
      Top             =   480
   End
   Begin VB.Timer Timer1 
      Left            =   1920
      Top             =   1080
   End
   Begin VB.Label lbl2 
      Alignment       =   2  'Center
      BackStyle       =   0  'Transparent
      Caption         =   "Coded by CS3"
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   14.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   495
      Left            =   0
      TabIndex        =   1
      Top             =   1440
      Width           =   4695
   End
   Begin VB.Label lbl1 
      Alignment       =   2  'Center
      Appearance      =   0  'Flat
      BackColor       =   &H00000000&
      BackStyle       =   0  'Transparent
      Caption         =   "loading the MP3 File..."
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   14.25
         Charset         =   0
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      ForeColor       =   &H00FFFFFF&
      Height          =   495
      Left            =   0
      TabIndex        =   0
      Top             =   0
      Width           =   4695
   End
End
Attribute VB_Name = "frmLoader"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim X(100), Y(100), xSpeed(100), ySpeed(100) As Integer

Private Sub Form_Load()
lbl1.ForeColor = vbWhite
Dim I As Integer
        Timer1.Interval = 4
        Me.BackColor = vbBlack
        Me.ForeColor = vbBlack
        Me.FillColor = vbBlack
For I = 0 To 99
            X(I) = -1
            Y(I) = -1
Next I

Randomize

Timer1.Enabled = True
End Sub

Private Sub Timer1_Timer()
    Dim I As Integer
    For I = 0 To 99
        PSet (X(I), Y(I)), &H0&
        If X(I) < 0 Or X(I) > Me.ScaleWidth Or Y(I) < 0 Or Y(I) > Me.ScaleHeight Then
            X(I) = Me.ScaleWidth \ 2
            Y(I) = Me.ScaleHeight \ 2
            xSpeed(I) = Int(Rnd(1) * 200) - 100
            ySpeed(I) = Int(Rnd(1) * 200) - 100
        End If
        X(I) = X(I) + xSpeed(I)
        Y(I) = Y(I) + ySpeed(I)
        PSet (X(I), Y(I)), vbWhite
    Next I
End Sub


Private Sub Timer2_Timer()
If lbl1.ForeColor = vbWhite Then
lbl1.ForeColor = vbYellow
ElseIf lbl1.ForeColor = vbYellow Then
lbl1.ForeColor = vbBlue
ElseIf lbl1.ForeColor = vbBlue Then
lbl1.ForeColor = vbWhite
End If
If lbl2.ForeColor = vbWhite Then
lbl2.ForeColor = vbYellow
ElseIf lbl2.ForeColor = vbYellow Then
lbl2.ForeColor = vbRed
ElseIf lbl2.ForeColor = vbRed Then
lbl2.ForeColor = vbWhite
End If
End Sub

Private Sub Timer3_Timer()
Timer3.Enabled = False
frmMain.Show
End Sub
