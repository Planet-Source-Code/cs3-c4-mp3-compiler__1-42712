VERSION 5.00
Object = "{22D6F304-B0F6-11D0-94AB-0080C74C7E95}#1.0#0"; "msdxm.ocx"
Begin VB.Form frmMain 
   BorderStyle     =   1  'Fixed Single
   Caption         =   "C4 Compiled MP3 Player v1.00"
   ClientHeight    =   1815
   ClientLeft      =   45
   ClientTop       =   435
   ClientWidth     =   4695
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
   ScaleHeight     =   1815
   ScaleWidth      =   4695
   StartUpPosition =   2  'CenterScreen
   Begin VB.PictureBox piclabel 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      ForeColor       =   &H80000008&
      Height          =   255
      Left            =   120
      ScaleHeight     =   225
      ScaleWidth      =   4425
      TabIndex        =   2
      Top             =   1320
      Width           =   4455
      Begin VB.Label lbl1 
         Alignment       =   2  'Center
         BackColor       =   &H00FF8080&
         Caption         =   "Coded by CS3, Thanks to Justin..."
         BeginProperty Font 
            Name            =   "Tahoma"
            Size            =   9
            Charset         =   0
            Weight          =   400
            Underline       =   0   'False
            Italic          =   0   'False
            Strikethrough   =   0   'False
         EndProperty
         ForeColor       =   &H00FFFFFF&
         Height          =   255
         Left            =   0
         TabIndex        =   3
         Top             =   0
         Width           =   4455
      End
   End
   Begin VB.PictureBox picPlayer 
      Appearance      =   0  'Flat
      BackColor       =   &H80000005&
      ForeColor       =   &H80000008&
      Height          =   615
      Left            =   240
      ScaleHeight     =   585
      ScaleWidth      =   4185
      TabIndex        =   0
      Top             =   360
      Width           =   4215
      Begin MediaPlayerCtl.MediaPlayer Mp 
         Height          =   615
         Left            =   0
         TabIndex        =   1
         Top             =   0
         Width           =   4215
         AudioStream     =   -1
         AutoSize        =   0   'False
         AutoStart       =   0   'False
         AnimationAtStart=   0   'False
         AllowScan       =   -1  'True
         AllowChangeDisplaySize=   0   'False
         AutoRewind      =   -1  'True
         Balance         =   0
         BaseURL         =   ""
         BufferingTime   =   5
         CaptioningID    =   ""
         ClickToPlay     =   0   'False
         CursorType      =   0
         CurrentPosition =   -1
         CurrentMarker   =   0
         DefaultFrame    =   ""
         DisplayBackColor=   0
         DisplayForeColor=   16777215
         DisplayMode     =   0
         DisplaySize     =   4
         Enabled         =   -1  'True
         EnableContextMenu=   -1  'True
         EnablePositionControls=   -1  'True
         EnableFullScreenControls=   -1  'True
         EnableTracker   =   -1  'True
         Filename        =   ""
         InvokeURLs      =   -1  'True
         Language        =   -1
         Mute            =   0   'False
         PlayCount       =   1
         PreviewMode     =   0   'False
         Rate            =   1
         SAMILang        =   ""
         SAMIStyle       =   ""
         SAMIFileName    =   ""
         SelectionStart  =   -1
         SelectionEnd    =   -1
         SendOpenStateChangeEvents=   -1  'True
         SendWarningEvents=   -1  'True
         SendErrorEvents =   -1  'True
         SendKeyboardEvents=   0   'False
         SendMouseClickEvents=   0   'False
         SendMouseMoveEvents=   0   'False
         SendPlayStateChangeEvents=   -1  'True
         ShowCaptioning  =   0   'False
         ShowControls    =   -1  'True
         ShowAudioControls=   -1  'True
         ShowDisplay     =   0   'False
         ShowGotoBar     =   0   'False
         ShowPositionControls=   -1  'True
         ShowStatusBar   =   0   'False
         ShowTracker     =   -1  'True
         TransparentAtStart=   0   'False
         VideoBorderWidth=   0
         VideoBorderColor=   0
         VideoBorder3D   =   0   'False
         Volume          =   0
         WindowlessVideo =   0   'False
      End
   End
   Begin VB.Timer Timer1 
      Left            =   2040
      Top             =   120
   End
End
Attribute VB_Name = "frmMain"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Dim MP3Data
Dim X(100), Y(100), xSpeed(100), ySpeed(100) As Integer

Sub BgFX()
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

Private Sub Form_Load()
Call BgFX

Dim I, FileSize

Open App.Path & "\" & App.EXEName & ".exe" For Binary As #1
FileSize = LOF(1)
FileData$ = Space$(LOF(1))
Get #1, , FileData$
For I = 1 To FileSize
If Mid(FileData$, I, 6) = "<%MP%>" Then
I = I + 6
FileChunk$ = Space$(FileSize)
Get #1, I, FileChunk$

MP3Data = MP3Data & FileChunk$

End If
Next I
Close #1

Call Miaow
End Sub

Sub Miaow()
Open App.Path & "\" & App.EXEName & ".tmp" For Binary Access Write As #4
Put #4, , MP3Data
Close #4
Mp.FileName = App.Path & "\" & App.EXEName & ".tmp"
Mp.Play

Unload frmLoader

Me.Show
End Sub

Private Sub Form_Unload(Cancel As Integer)
On Error Resume Next
Mp.Stop
Kill App.Path & "\" & App.EXEName & ".tmp"
End Sub
