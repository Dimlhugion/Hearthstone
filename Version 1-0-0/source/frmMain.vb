'Dave Kurtz
'Hearthstone Card Draw Simulator
'Last Revision: 3/12/15
'Currently Working On: Done!


Public Class frmMain

    Public Random As New Random
    Public Const TOTALGAMES As Integer = 200000

    Private Sub radSecond_CheckedChanged(sender As Object, e As EventArgs) Handles radSecond.CheckedChanged
        'Show or Hide card/mulligan option 4 depending on whether player goes second
        If radSecond.Checked = True Then
            radCard4.Visible = True
            radMull4.Visible = True
        Else
            radCard4.Visible = False
            radMull4.Visible = False

            'If card option 4 was checked, uncheck it now that it's hidden
            If radCard4.Checked = True Then
                radCard1.Checked = True
            End If

            'If mulligan option 4 was checked, uncheck it now that it's hidden
            If radMull4.Checked = True Then
                radMull0.Checked = True
            End If
        End If
    End Sub

    Private Sub radCard1_CheckedChanged(sender As Object, e As EventArgs) Handles radCard1.CheckedChanged, radCard2.CheckedChanged, radCard3.CheckedChanged, radCard4.CheckedChanged
        'Show only the appropriate amount of "Cards in Deck" boxes
        If radCard2.Checked = True Then
            grpCard2.Visible = True
            radCard2Amount1.Checked = True
            grpCard3.Visible = False
            radCard3Amount1.Checked = False
            radCard3Amount2.Checked = False
            grpCard4.Visible = False
            radCard4Amount1.Checked = False
            radCard4Amount2.Checked = False
        ElseIf radCard3.Checked = True Then
            grpCard2.Visible = True
            radCard2Amount1.Checked = True
            grpCard3.Visible = True
            radCard3Amount1.Checked = True
            grpCard4.Visible = False
            radCard4Amount1.Checked = False
            radCard4Amount2.Checked = False
        ElseIf radCard4.Checked = True Then
            grpCard2.Visible = True
            radCard2Amount1.Checked = True
            grpCard3.Visible = True
            radCard3Amount1.Checked = True
            grpCard4.Visible = True
            radCard4Amount1.Checked = True
        Else
            grpCard2.Visible = False
            radCard2Amount1.Checked = False
            radCard2Amount2.Checked = False
            grpCard3.Visible = False
            radCard3Amount1.Checked = False
            radCard3Amount2.Checked = False
            grpCard4.Visible = False
            radCard4Amount1.Checked = False
            radCard4Amount2.Checked = False
        End If
    End Sub

    Private Function doTheMath(intCase As Integer, intMaxDraws As Integer, intPlayerTwoBonus As Integer)
        Dim intRandom, gamesIncrementor, drawsIncrementor, intMatches As Integer
        Dim intCardWanted1, intCardWanted2, intCardWanted3, intCardWanted4, intCardWanted5, intCardWanted6, intCardWanted7, intCardWanted8 As Integer

        Select Case intCase
            Case "1" 'draw 1 (1) mulligan 0
                For gamesIncrementor = 1 To TOTALGAMES
                    intCardWanted1 = 1

                    For drawsIncrementor = 0 To intMaxDraws
                        intRandom = Random.Next(1, 31 - drawsIncrementor)

                        If intRandom = intCardWanted1 Then
                            intMatches += 1
                            intCardWanted1 = 0
                        End If
                    Next 'End Inner (draws) Loop

                Next 'End Outer (games) Loop

            Case "2" 'draw 1 (1) mulligan 1+
                For gamesIncrementor = 1 To TOTALGAMES
                    intCardWanted1 = 1

                    For drawsIncrementor = 0 To intMaxDraws
                        intRandom = Random.Next(1, 31 - drawsIncrementor)

                        If intRandom = intCardWanted1 Then
                            intMatches += 1
                            intCardWanted1 = 0
                        End If
                    Next 'End Inner (draws) Loop

                    intRandom = Random.Next(1, 28 - intPlayerTwoBonus) '1st turn draw

                    If intRandom = intCardWanted1 Then
                        intMatches += 1
                    End If
                Next 'End Outer (games) Loop

            Case "3" 'draw 1 (2) mulligan 0
                For gamesIncrementor = 1 To TOTALGAMES
                    intCardWanted1 = 1
                    intCardWanted2 = 2

                    For drawsIncrementor = 0 To intMaxDraws
                        intRandom = Random.Next(1, 31 - drawsIncrementor)

                        If intRandom = intCardWanted1 Or intRandom = intCardWanted2 Then
                            intMatches += 1
                            intCardWanted1 = 0
                            intCardWanted2 = 0
                        End If
                    Next 'End Inner (draws) Loop

                Next 'End Outer (games) Loop

            Case "4" 'draw 1 (2) mulligan 1+
                For gamesIncrementor = 1 To TOTALGAMES
                    intCardWanted1 = 1
                    intCardWanted2 = 2

                    For drawsIncrementor = 0 To intMaxDraws
                        intRandom = Random.Next(1, 31 - drawsIncrementor)

                        If intRandom = intCardWanted1 Or intRandom = intCardWanted2 Then
                            intMatches += 1
                            intCardWanted1 = 0
                            intCardWanted2 = 0
                        End If
                    Next 'End Inner (draws) Loop

                    intRandom = Random.Next(1, 28 - intPlayerTwoBonus) '1st turn draw

                    If intRandom = intCardWanted1 Or intRandom = intCardWanted2 Then
                        intMatches += 1
                    End If
                Next 'End Outer (games) Loop

            Case "5" 'draw 2 (1-1) mulligan 0
                For gamesIncrementor = 1 To TOTALGAMES
                    intCardWanted1 = 1
                    intCardWanted3 = 3

                    For drawsIncrementor = 0 To intMaxDraws
                        intRandom = Random.Next(1, 31 - drawsIncrementor)

                        If intRandom = intCardWanted1 Then
                            intCardWanted1 = 0
                        End If
                        If intRandom = intCardWanted3 Then
                            intCardWanted3 = 0
                        End If
                    Next 'End Inner (draws) Loop

                    If intCardWanted1 = 0 AndAlso intCardWanted3 = 0 Then
                        intMatches += 1
                    End If
                Next 'End Outer (games) Loop

            Case "6" 'draw 2 (1-1) mulligan 1+
                For gamesIncrementor = 1 To TOTALGAMES
                    intCardWanted1 = 1
                    intCardWanted3 = 3

                    For drawsIncrementor = 0 To intMaxDraws
                        intRandom = Random.Next(1, 31 - drawsIncrementor)

                        If intRandom = intCardWanted1 Then
                            intCardWanted1 = 0
                        End If
                        If intRandom = intCardWanted3 Then
                            intCardWanted3 = 0
                        End If
                    Next 'End Inner (draws) Loop

                    intRandom = Random.Next(1, 28 - intPlayerTwoBonus) '1st turn draw

                    If intRandom = intCardWanted1 Then
                        intCardWanted1 = 0
                    End If
                    If intRandom = intCardWanted3 Then
                        intCardWanted3 = 0
                    End If

                    If intCardWanted1 = 0 AndAlso intCardWanted3 = 0 Then
                        intMatches += 1
                    End If
                Next 'End Outer (games) Loop

            Case "7" 'draw 2 (1-2) mulligan 0
                For gamesIncrementor = 1 To TOTALGAMES
                    intCardWanted1 = 1
                    intCardWanted3 = 3
                    intCardWanted4 = 4

                    For drawsIncrementor = 0 To intMaxDraws
                        intRandom = Random.Next(1, 31 - drawsIncrementor)

                        If intRandom = intCardWanted1 Then
                            intCardWanted1 = 0
                        End If
                        If intRandom = intCardWanted3 Or intRandom = intCardWanted4 Then
                            intCardWanted3 = 0
                        End If
                    Next 'End Inner (draws) Loop

                    If intCardWanted1 = 0 AndAlso intCardWanted3 = 0 Then
                        intMatches += 1
                    End If
                Next 'End Outer (games) Loop

            Case "8" 'draw 2 (1-2) mulligan 1+
                For gamesIncrementor = 1 To TOTALGAMES
                    intCardWanted1 = 1
                    intCardWanted3 = 3
                    intCardWanted4 = 4

                    For drawsIncrementor = 0 To intMaxDraws
                        intRandom = Random.Next(1, 31 - drawsIncrementor)

                        If intRandom = intCardWanted1 Then
                            intCardWanted1 = 0
                        End If
                        If intRandom = intCardWanted3 Or intRandom = intCardWanted4 Then
                            intCardWanted3 = 0
                        End If
                    Next 'End Inner (draws) Loop

                    intRandom = Random.Next(1, 28 - intPlayerTwoBonus) '1st turn draw

                    If intRandom = intCardWanted1 Then
                        intCardWanted1 = 0
                    End If
                    If intRandom = intCardWanted3 Or intRandom = intCardWanted4 Then
                        intCardWanted3 = 0
                    End If

                    If intCardWanted1 = 0 AndAlso intCardWanted3 = 0 Then
                        intMatches += 1
                    End If
                Next 'End Outer (games) Loop

            Case "9" 'draw 2 (2-2) mulligan 0
                For gamesIncrementor = 1 To TOTALGAMES
                    intCardWanted1 = 1
                    intCardWanted2 = 2
                    intCardWanted3 = 3
                    intCardWanted4 = 4

                    For drawsIncrementor = 0 To intMaxDraws
                        intRandom = Random.Next(1, 31 - drawsIncrementor)

                        If intRandom = intCardWanted1 Or intRandom = intCardWanted2 Then
                            intCardWanted1 = 0
                        End If
                        If intRandom = intCardWanted3 Or intRandom = intCardWanted4 Then
                            intCardWanted3 = 0
                        End If
                    Next 'End Inner (draws) Loop

                    If intCardWanted1 = 0 AndAlso intCardWanted3 = 0 Then
                        intMatches += 1
                    End If
                Next 'End Outer (games) Loop

            Case "10" 'draw 2 (2-2) mulligan 1+
                For gamesIncrementor = 1 To TOTALGAMES
                    intCardWanted1 = 1
                    intCardWanted2 = 2
                    intCardWanted3 = 3
                    intCardWanted4 = 4

                    For drawsIncrementor = 0 To intMaxDraws
                        intRandom = Random.Next(1, 31 - drawsIncrementor)

                        If intRandom = intCardWanted1 Or intRandom = intCardWanted2 Then
                            intCardWanted1 = 0
                        End If
                        If intRandom = intCardWanted3 Or intRandom = intCardWanted4 Then
                            intCardWanted3 = 0
                        End If
                    Next 'End Inner (draws) Loop

                    intRandom = Random.Next(1, 28 - intPlayerTwoBonus) '1st turn draw

                    If intRandom = intCardWanted1 Or intRandom = intCardWanted2 Then
                        intCardWanted1 = 0
                    End If
                    If intRandom = intCardWanted3 Or intRandom = intCardWanted4 Then
                        intCardWanted3 = 0
                    End If

                    If intCardWanted1 = 0 AndAlso intCardWanted3 = 0 Then
                        intMatches += 1
                    End If
                Next 'End Outer (games) Loop

            Case "11" 'draw 3 (1-1-1) mulligan 0
                For gamesIncrementor = 1 To TOTALGAMES
                    intCardWanted1 = 1
                    intCardWanted3 = 3
                    intCardWanted5 = 5

                    For drawsIncrementor = 0 To intMaxDraws
                        intRandom = Random.Next(1, 31 - drawsIncrementor)

                        If intRandom = intCardWanted1 Then
                            intCardWanted1 = 0
                        End If
                        If intRandom = intCardWanted3 Then
                            intCardWanted3 = 0
                        End If
                        If intRandom = intCardWanted5 Then
                            intCardWanted5 = 0
                        End If
                    Next 'End Inner (draws) Loop

                    If intCardWanted1 = 0 AndAlso intCardWanted3 = 0 AndAlso intCardWanted5 = 0 Then
                        intMatches += 1
                    End If
                Next 'End Outer (games) Loop

            Case "12" 'draw 3 (1-1-1) mulligan 1+
                For gamesIncrementor = 1 To TOTALGAMES
                    intCardWanted1 = 1
                    intCardWanted3 = 3
                    intCardWanted5 = 5

                    For drawsIncrementor = 0 To intMaxDraws
                        intRandom = Random.Next(1, 31 - drawsIncrementor)

                        If intRandom = intCardWanted1 Then
                            intCardWanted1 = 0
                        End If
                        If intRandom = intCardWanted3 Then
                            intCardWanted3 = 0
                        End If
                        If intRandom = intCardWanted5 Then
                            intCardWanted5 = 0
                        End If
                    Next 'End Inner (draws) Loop

                    intRandom = Random.Next(1, 28 - intPlayerTwoBonus) '1st turn draw

                    If intRandom = intCardWanted1 Then
                        intCardWanted1 = 0
                    End If
                    If intRandom = intCardWanted3 Then
                        intCardWanted3 = 0
                    End If
                    If intRandom = intCardWanted5 Then
                        intCardWanted5 = 0
                    End If

                    If intCardWanted1 = 0 AndAlso intCardWanted3 = 0 AndAlso intCardWanted5 = 0 Then
                        intMatches += 1
                    End If
                Next 'End Outer (games) Loop

            Case "13" 'draw 3 (1-1-2) mulligan 0
                For gamesIncrementor = 1 To TOTALGAMES
                    intCardWanted1 = 1
                    intCardWanted3 = 3
                    intCardWanted5 = 5
                    intCardWanted6 = 6

                    For drawsIncrementor = 0 To intMaxDraws
                        intRandom = Random.Next(1, 31 - drawsIncrementor)

                        If intRandom = intCardWanted1 Then
                            intCardWanted1 = 0
                        End If
                        If intRandom = intCardWanted3 Then
                            intCardWanted3 = 0
                        End If
                        If intRandom = intCardWanted5 Or intRandom = intCardWanted6 Then
                            intCardWanted5 = 0
                        End If
                    Next 'End Inner (draws) Loop

                    If intCardWanted1 = 0 AndAlso intCardWanted3 = 0 AndAlso intCardWanted5 = 0 Then
                        intMatches += 1
                    End If
                Next 'End Outer (games) Loop

            Case "14" 'draw 3 (1-1-2) mulligan 1+
                For gamesIncrementor = 1 To TOTALGAMES
                    intCardWanted1 = 1
                    intCardWanted3 = 3
                    intCardWanted5 = 5
                    intCardWanted6 = 6

                    For drawsIncrementor = 0 To intMaxDraws
                        intRandom = Random.Next(1, 31 - drawsIncrementor)

                        If intRandom = intCardWanted1 Then
                            intCardWanted1 = 0
                        End If
                        If intRandom = intCardWanted3 Then
                            intCardWanted3 = 0
                        End If
                        If intRandom = intCardWanted5 Or intRandom = intCardWanted6 Then
                            intCardWanted5 = 0
                        End If
                    Next 'End Inner (draws) Loop

                    intRandom = Random.Next(1, 28 - intPlayerTwoBonus) '1st turn draw

                    If intRandom = intCardWanted1 Then
                        intCardWanted1 = 0
                    End If
                    If intRandom = intCardWanted3 Then
                        intCardWanted3 = 0
                    End If
                    If intRandom = intCardWanted5 Or intRandom = intCardWanted6 Then
                        intCardWanted5 = 0
                    End If

                    If intCardWanted1 = 0 AndAlso intCardWanted3 = 0 AndAlso intCardWanted5 = 0 Then
                        intMatches += 1
                    End If
                Next 'End Outer (games) Loop

            Case "15" 'draw 3 (1-2-2) mulligan 0
                For gamesIncrementor = 1 To TOTALGAMES
                    intCardWanted1 = 1
                    intCardWanted3 = 3
                    intCardWanted4 = 4
                    intCardWanted5 = 5
                    intCardWanted6 = 6

                    For drawsIncrementor = 0 To intMaxDraws
                        intRandom = Random.Next(1, 31 - drawsIncrementor)

                        If intRandom = intCardWanted1 Then
                            intCardWanted1 = 0
                        End If
                        If intRandom = intCardWanted3 Or intRandom = intCardWanted4 Then
                            intCardWanted3 = 0
                        End If
                        If intRandom = intCardWanted5 Or intRandom = intCardWanted6 Then
                            intCardWanted5 = 0
                        End If
                    Next 'End Inner (draws) Loop

                    If intCardWanted1 = 0 AndAlso intCardWanted3 = 0 AndAlso intCardWanted5 = 0 Then
                        intMatches += 1
                    End If
                Next 'End Outer (games) Loop

            Case "16" 'draw 3 (1-2-2) mulligan 1+
                For gamesIncrementor = 1 To TOTALGAMES
                    intCardWanted1 = 1
                    intCardWanted3 = 3
                    intCardWanted4 = 4
                    intCardWanted5 = 5
                    intCardWanted6 = 6

                    For drawsIncrementor = 0 To intMaxDraws
                        intRandom = Random.Next(1, 31 - drawsIncrementor)

                        If intRandom = intCardWanted1 Then
                            intCardWanted1 = 0
                        End If
                        If intRandom = intCardWanted3 Or intRandom = intCardWanted4 Then
                            intCardWanted3 = 0
                        End If
                        If intRandom = intCardWanted5 Or intRandom = intCardWanted6 Then
                            intCardWanted5 = 0
                        End If
                    Next 'End Inner (draws) Loop

                    intRandom = Random.Next(1, 28 - intPlayerTwoBonus) '1st turn draw

                    If intRandom = intCardWanted1 Then
                        intCardWanted1 = 0
                    End If
                    If intRandom = intCardWanted3 Or intRandom = intCardWanted4 Then
                        intCardWanted3 = 0
                    End If
                    If intRandom = intCardWanted5 Or intRandom = intCardWanted6 Then
                        intCardWanted5 = 0
                    End If

                    If intCardWanted1 = 0 AndAlso intCardWanted3 = 0 AndAlso intCardWanted5 = 0 Then
                        intMatches += 1
                    End If
                Next 'End Outer (games) Loop

            Case "17" 'draw 3 (2-2-2) mulligan 0
                For gamesIncrementor = 1 To TOTALGAMES
                    intCardWanted1 = 1
                    intCardWanted2 = 2
                    intCardWanted3 = 3
                    intCardWanted4 = 4
                    intCardWanted5 = 5
                    intCardWanted6 = 6

                    For drawsIncrementor = 0 To intMaxDraws
                        intRandom = Random.Next(1, 31 - drawsIncrementor)

                        If intRandom = intCardWanted1 Or intRandom = intCardWanted2 Then
                            intCardWanted1 = 0
                        End If
                        If intRandom = intCardWanted3 Or intRandom = intCardWanted4 Then
                            intCardWanted3 = 0
                        End If
                        If intRandom = intCardWanted5 Or intRandom = intCardWanted6 Then
                            intCardWanted5 = 0
                        End If
                    Next 'End Inner (draws) Loop

                    If intCardWanted1 = 0 AndAlso intCardWanted3 = 0 AndAlso intCardWanted5 = 0 Then
                        intMatches += 1
                    End If
                Next 'End Outer (games) Loop

            Case "18" 'draw 3 (2-2-2) mulligan 1+
                For gamesIncrementor = 1 To TOTALGAMES
                    intCardWanted1 = 1
                    intCardWanted2 = 2
                    intCardWanted3 = 3
                    intCardWanted4 = 4
                    intCardWanted5 = 5
                    intCardWanted6 = 6

                    For drawsIncrementor = 0 To intMaxDraws
                        intRandom = Random.Next(1, 31 - drawsIncrementor)

                        If intRandom = intCardWanted1 Or intRandom = intCardWanted2 Then
                            intCardWanted1 = 0
                        End If
                        If intRandom = intCardWanted3 Or intRandom = intCardWanted4 Then
                            intCardWanted3 = 0
                        End If
                        If intRandom = intCardWanted5 Or intRandom = intCardWanted6 Then
                            intCardWanted5 = 0
                        End If
                    Next 'End Inner (draws) Loop

                    intRandom = Random.Next(1, 28 - intPlayerTwoBonus) '1st turn draw

                    If intRandom = intCardWanted1 Or intRandom = intCardWanted2 Then
                        intCardWanted1 = 0
                    End If
                    If intRandom = intCardWanted3 Or intRandom = intCardWanted4 Then
                        intCardWanted3 = 0
                    End If
                    If intRandom = intCardWanted5 Or intRandom = intCardWanted6 Then
                        intCardWanted5 = 0
                    End If

                    If intCardWanted1 = 0 AndAlso intCardWanted3 = 0 AndAlso intCardWanted5 = 0 Then
                        intMatches += 1
                    End If
                Next 'End Outer (games) Loop

            Case "19" 'draw 4 (1-1-1-1) mulligan 0
                For gamesIncrementor = 1 To TOTALGAMES
                    intCardWanted1 = 1
                    intCardWanted3 = 3
                    intCardWanted5 = 5
                    intCardWanted7 = 7

                    For drawsIncrementor = 0 To intMaxDraws
                        intRandom = Random.Next(1, 31 - drawsIncrementor)

                        If intRandom = intCardWanted1 Then
                            intCardWanted1 = 0
                        End If
                        If intRandom = intCardWanted3 Then
                            intCardWanted3 = 0
                        End If
                        If intRandom = intCardWanted5 Then
                            intCardWanted5 = 0
                        End If
                        If intRandom = intCardWanted7 Then
                            intCardWanted7 = 0
                        End If
                    Next 'End Inner (draws) Loop

                    If intCardWanted1 = 0 AndAlso intCardWanted3 = 0 AndAlso intCardWanted5 = 0 AndAlso intCardWanted7 = 0 Then
                        intMatches += 1
                    End If
                Next 'End Outer (games) Loop

            Case "20" 'draw 4 (1-1-1-1) mulligan 1+
                For gamesIncrementor = 1 To TOTALGAMES
                    intCardWanted1 = 1
                    intCardWanted3 = 3
                    intCardWanted5 = 5
                    intCardWanted7 = 7

                    For drawsIncrementor = 0 To intMaxDraws
                        intRandom = Random.Next(1, 31 - drawsIncrementor)

                        If intRandom = intCardWanted1 Then
                            intCardWanted1 = 0
                        End If
                        If intRandom = intCardWanted3 Then
                            intCardWanted3 = 0
                        End If
                        If intRandom = intCardWanted5 Then
                            intCardWanted5 = 0
                        End If
                        If intRandom = intCardWanted7 Then
                            intCardWanted7 = 0
                        End If
                    Next 'End Inner (draws) Loop

                    intRandom = Random.Next(1, 28 - intPlayerTwoBonus) '1st turn draw

                    If intRandom = intCardWanted1 Then
                        intCardWanted1 = 0
                    End If
                    If intRandom = intCardWanted3 Then
                        intCardWanted3 = 0
                    End If
                    If intRandom = intCardWanted5 Then
                        intCardWanted5 = 0
                    End If
                    If intRandom = intCardWanted7 Then
                        intCardWanted7 = 0
                    End If

                    If intCardWanted1 = 0 AndAlso intCardWanted3 = 0 AndAlso intCardWanted5 = 0 AndAlso intCardWanted7 = 0 Then
                        intMatches += 1
                    End If
                Next 'End Outer (games) Loop

            Case "21" 'draw 4 (1-1-1-2) mulligan 0
                For gamesIncrementor = 1 To TOTALGAMES
                    intCardWanted1 = 1
                    intCardWanted3 = 3
                    intCardWanted5 = 5
                    intCardWanted7 = 7
                    intCardWanted8 = 8

                    For drawsIncrementor = 0 To intMaxDraws
                        intRandom = Random.Next(1, 31 - drawsIncrementor)

                        If intRandom = intCardWanted1 Then
                            intCardWanted1 = 0
                        End If
                        If intRandom = intCardWanted3 Then
                            intCardWanted3 = 0
                        End If
                        If intRandom = intCardWanted5 Then
                            intCardWanted5 = 0
                        End If
                        If intRandom = intCardWanted7 Or intRandom = intCardWanted8 Then
                            intCardWanted7 = 0
                        End If
                    Next 'End Inner (draws) Loop

                    If intCardWanted1 = 0 AndAlso intCardWanted3 = 0 AndAlso intCardWanted5 = 0 AndAlso intCardWanted7 = 0 Then
                        intMatches += 1
                    End If
                Next 'End Outer (games) Loop

            Case "22" 'draw 4 (1-1-1-2) mulligan 1+
                For gamesIncrementor = 1 To TOTALGAMES
                    intCardWanted1 = 1
                    intCardWanted3 = 3
                    intCardWanted5 = 5
                    intCardWanted7 = 7
                    intCardWanted8 = 8

                    For drawsIncrementor = 0 To intMaxDraws
                        intRandom = Random.Next(1, 31 - drawsIncrementor)

                        If intRandom = intCardWanted1 Then
                            intCardWanted1 = 0
                        End If
                        If intRandom = intCardWanted3 Then
                            intCardWanted3 = 0
                        End If
                        If intRandom = intCardWanted5 Then
                            intCardWanted5 = 0
                        End If
                        If intRandom = intCardWanted7 Or intRandom = intCardWanted8 Then
                            intCardWanted7 = 0
                        End If
                    Next 'End Inner (draws) Loop

                    intRandom = Random.Next(1, 28 - intPlayerTwoBonus) '1st turn draw

                    If intRandom = intCardWanted1 Then
                        intCardWanted1 = 0
                    End If
                    If intRandom = intCardWanted3 Then
                        intCardWanted3 = 0
                    End If
                    If intRandom = intCardWanted5 Then
                        intCardWanted5 = 0
                    End If
                    If intRandom = intCardWanted7 Or intRandom = intCardWanted8 Then
                        intCardWanted7 = 0
                    End If

                    If intCardWanted1 = 0 AndAlso intCardWanted3 = 0 AndAlso intCardWanted5 = 0 AndAlso intCardWanted7 = 0 Then
                        intMatches += 1
                    End If
                Next 'End Outer (games) Loop

            Case "23" 'draw 4 (1-1-2-2) mulligan 0
                For gamesIncrementor = 1 To TOTALGAMES
                    intCardWanted1 = 1
                    intCardWanted3 = 3
                    intCardWanted5 = 5
                    intCardWanted6 = 6
                    intCardWanted7 = 7
                    intCardWanted8 = 8

                    For drawsIncrementor = 0 To intMaxDraws
                        intRandom = Random.Next(1, 31 - drawsIncrementor)

                        If intRandom = intCardWanted1 Then
                            intCardWanted1 = 0
                        End If
                        If intRandom = intCardWanted3 Then
                            intCardWanted3 = 0
                        End If
                        If intRandom = intCardWanted5 Or intRandom = intCardWanted6 Then
                            intCardWanted5 = 0
                        End If
                        If intRandom = intCardWanted7 Or intRandom = intCardWanted8 Then
                            intCardWanted7 = 0
                        End If
                    Next 'End Inner (draws) Loop

                    If intCardWanted1 = 0 AndAlso intCardWanted3 = 0 AndAlso intCardWanted5 = 0 AndAlso intCardWanted7 = 0 Then
                        intMatches += 1
                    End If
                Next 'End Outer (games) Loop

            Case "24" 'draw 4 (1-1-2-2) mulligan 1+
                For gamesIncrementor = 1 To TOTALGAMES
                    intCardWanted1 = 1
                    intCardWanted3 = 3
                    intCardWanted5 = 5
                    intCardWanted6 = 6
                    intCardWanted7 = 7
                    intCardWanted8 = 8

                    For drawsIncrementor = 0 To intMaxDraws
                        intRandom = Random.Next(1, 31 - drawsIncrementor)

                        If intRandom = intCardWanted1 Then
                            intCardWanted1 = 0
                        End If
                        If intRandom = intCardWanted3 Then
                            intCardWanted3 = 0
                        End If
                        If intRandom = intCardWanted5 Or intRandom = intCardWanted6 Then
                            intCardWanted5 = 0
                        End If
                        If intRandom = intCardWanted7 Or intRandom = intCardWanted8 Then
                            intCardWanted7 = 0
                        End If
                    Next 'End Inner (draws) Loop

                    intRandom = Random.Next(1, 28 - intPlayerTwoBonus) '1st turn draw

                    If intRandom = intCardWanted1 Then
                        intCardWanted1 = 0
                    End If
                    If intRandom = intCardWanted3 Then
                        intCardWanted3 = 0
                    End If
                    If intRandom = intCardWanted5 Or intRandom = intCardWanted6 Then
                        intCardWanted5 = 0
                    End If
                    If intRandom = intCardWanted7 Or intRandom = intCardWanted8 Then
                        intCardWanted7 = 0
                    End If

                    If intCardWanted1 = 0 AndAlso intCardWanted3 = 0 AndAlso intCardWanted5 = 0 AndAlso intCardWanted7 = 0 Then
                        intMatches += 1
                    End If
                Next 'End Outer (games) Loop

            Case "25" 'draw 4 (1-2-2-2) mulligan 0
                For gamesIncrementor = 1 To TOTALGAMES
                    intCardWanted1 = 1
                    intCardWanted3 = 3
                    intCardWanted4 = 4
                    intCardWanted5 = 5
                    intCardWanted6 = 6
                    intCardWanted7 = 7
                    intCardWanted8 = 8

                    For drawsIncrementor = 0 To intMaxDraws
                        intRandom = Random.Next(1, 31 - drawsIncrementor)

                        If intRandom = intCardWanted1 Then
                            intCardWanted1 = 0
                        End If
                        If intRandom = intCardWanted3 Or intRandom = intCardWanted4 Then
                            intCardWanted3 = 0
                        End If
                        If intRandom = intCardWanted5 Or intRandom = intCardWanted6 Then
                            intCardWanted5 = 0
                        End If
                        If intRandom = intCardWanted7 Or intRandom = intCardWanted8 Then
                            intCardWanted7 = 0
                        End If
                    Next 'End Inner (draws) Loop

                    If intCardWanted1 = 0 AndAlso intCardWanted3 = 0 AndAlso intCardWanted5 = 0 AndAlso intCardWanted7 = 0 Then
                        intMatches += 1
                    End If
                Next 'End Outer (games) Loop

            Case "26" 'draw 4 (1-2-2-2) mulligan 1+
                For gamesIncrementor = 1 To TOTALGAMES
                    intCardWanted1 = 1
                    intCardWanted3 = 3
                    intCardWanted4 = 4
                    intCardWanted5 = 5
                    intCardWanted6 = 6
                    intCardWanted7 = 7
                    intCardWanted8 = 8

                    For drawsIncrementor = 0 To intMaxDraws
                        intRandom = Random.Next(1, 31 - drawsIncrementor)

                        If intRandom = intCardWanted1 Then
                            intCardWanted1 = 0
                        End If
                        If intRandom = intCardWanted3 Or intRandom = intCardWanted4 Then
                            intCardWanted3 = 0
                        End If
                        If intRandom = intCardWanted5 Or intRandom = intCardWanted6 Then
                            intCardWanted5 = 0
                        End If
                        If intRandom = intCardWanted7 Or intRandom = intCardWanted8 Then
                            intCardWanted7 = 0
                        End If
                    Next 'End Inner (draws) Loop

                    intRandom = Random.Next(1, 28 - intPlayerTwoBonus) '1st turn draw

                    If intRandom = intCardWanted1 Then
                        intCardWanted1 = 0
                    End If
                    If intRandom = intCardWanted3 Or intRandom = intCardWanted4 Then
                        intCardWanted3 = 0
                    End If
                    If intRandom = intCardWanted5 Or intRandom = intCardWanted6 Then
                        intCardWanted5 = 0
                    End If
                    If intRandom = intCardWanted7 Or intRandom = intCardWanted8 Then
                        intCardWanted7 = 0
                    End If

                    If intCardWanted1 = 0 AndAlso intCardWanted3 = 0 AndAlso intCardWanted5 = 0 AndAlso intCardWanted7 = 0 Then
                        intMatches += 1
                    End If
                Next 'End Outer (games) Loop

            Case "27" 'draw 4 (2-2-2-2) mulligan 0
                For gamesIncrementor = 1 To TOTALGAMES
                    intCardWanted1 = 1
                    intCardWanted2 = 2
                    intCardWanted3 = 3
                    intCardWanted4 = 4
                    intCardWanted5 = 5
                    intCardWanted6 = 6
                    intCardWanted7 = 7
                    intCardWanted8 = 8

                    For drawsIncrementor = 0 To intMaxDraws
                        intRandom = Random.Next(1, 31 - drawsIncrementor)

                        If intRandom = intCardWanted1 Or intRandom = intCardWanted2 Then
                            intCardWanted1 = 0
                        End If
                        If intRandom = intCardWanted3 Or intRandom = intCardWanted4 Then
                            intCardWanted3 = 0
                        End If
                        If intRandom = intCardWanted5 Or intRandom = intCardWanted6 Then
                            intCardWanted5 = 0
                        End If
                        If intRandom = intCardWanted7 Or intRandom = intCardWanted8 Then
                            intCardWanted7 = 0
                        End If
                    Next 'End Inner (draws) Loop

                    If intCardWanted1 = 0 AndAlso intCardWanted3 = 0 AndAlso intCardWanted5 = 0 AndAlso intCardWanted7 = 0 Then
                        intMatches += 1
                    End If
                Next 'End Outer (games) Loop

            Case "28" 'draw 4 (2-2-2-2) mulligan 1+
                For gamesIncrementor = 1 To TOTALGAMES
                    intCardWanted1 = 1
                    intCardWanted2 = 2
                    intCardWanted3 = 3
                    intCardWanted4 = 4
                    intCardWanted5 = 5
                    intCardWanted6 = 6
                    intCardWanted7 = 7
                    intCardWanted8 = 8

                    For drawsIncrementor = 0 To intMaxDraws
                        intRandom = Random.Next(1, 31 - drawsIncrementor)

                        If intRandom = intCardWanted1 Or intRandom = intCardWanted2 Then
                            intCardWanted1 = 0
                        End If
                        If intRandom = intCardWanted3 Or intRandom = intCardWanted4 Then
                            intCardWanted3 = 0
                        End If
                        If intRandom = intCardWanted5 Or intRandom = intCardWanted6 Then
                            intCardWanted5 = 0
                        End If
                        If intRandom = intCardWanted7 Or intRandom = intCardWanted8 Then
                            intCardWanted7 = 0
                        End If
                    Next 'End Inner (draws) Loop

                    intRandom = Random.Next(1, 28 - intPlayerTwoBonus) '1st turn draw

                    If intRandom = intCardWanted1 Or intRandom = intCardWanted2 Then
                        intCardWanted1 = 0
                    End If
                    If intRandom = intCardWanted3 Or intRandom = intCardWanted4 Then
                        intCardWanted3 = 0
                    End If
                    If intRandom = intCardWanted5 Or intRandom = intCardWanted6 Then
                        intCardWanted5 = 0
                    End If
                    If intRandom = intCardWanted7 Or intRandom = intCardWanted8 Then
                        intCardWanted7 = 0
                    End If

                    If intCardWanted1 = 0 AndAlso intCardWanted3 = 0 AndAlso intCardWanted5 = 0 AndAlso intCardWanted7 = 0 Then
                        intMatches += 1
                    End If
                Next 'End Outer (games) Loop
        End Select

        Return intMatches
    End Function

    Private Sub btnCalculate_Click(sender As Object, e As EventArgs) Handles btnCalculate.Click
        Dim intCase As Integer = 0
        Dim intPlayer, intCardNumber, intMulligan, intCard1Amount, intCard2Amount, intCard3Amount, intCard4Amount As Integer
        Dim decResults As Decimal
        Dim intMaxDraws, intMatches, intPlayerTwoBonus As Integer

        If radFirst.Checked = True Then
            intPlayer = 1
            intPlayerTwoBonus = 0
        Else
            intPlayer = 2
            intPlayerTwoBonus = 1
        End If

        If radMull0.Checked = True Then
            intMulligan = 0
        ElseIf radMull1.Checked = True Then
            intMulligan = 1
        ElseIf radMull2.Checked = True Then
            intMulligan = 2
        ElseIf radMull3.Checked = True Then
            intMulligan = 3
        Else
            intMulligan = 4
        End If

        If radCard1.Checked = True Then
            intCardNumber = 1
        ElseIf radCard2.Checked = True Then
            intCardNumber = 2
        ElseIf radCard3.Checked = True Then
            intCardNumber = 3
        Else
            intCardNumber = 4
        End If

        If radCard1Amount1.Checked = True Then
            intCard1Amount = 1
        Else
            intCard1Amount = 2
        End If

        If radCard2Amount1.Checked = True Then
            intCard2Amount = 1
        ElseIf radCard2Amount2.Checked = True Then
            intCard2Amount = 2
        Else
            intCard2Amount = 0
        End If

        If radCard3Amount1.Checked = True Then
            intCard3Amount = 1
        ElseIf radCard3Amount2.Checked = True Then
            intCard3Amount = 2
        Else
            intCard3Amount = 0
        End If

        If radCard4Amount1.Checked = True Then
            intCard4Amount = 1
        ElseIf radCard4Amount2.Checked = True Then
            intCard4Amount = 2
        Else
            intCard4Amount = 0
        End If

        If intPlayer = 1 And intMulligan = 0 Then
            intMaxDraws = 3
        ElseIf intPlayer = 1 And intMulligan > 0 Then
            intMaxDraws = intMulligan + 2
        ElseIf intPlayer = 2 And intMulligan = 0 Then
            intMaxDraws = 4
        ElseIf intPlayer = 2 And intMulligan > 0 Then
            intMaxDraws = intMulligan + 3
        End If

        If intCardNumber = 1 AndAlso intCard1Amount = 1 AndAlso intMulligan = 0 Then
            intCase = 1
        End If

        If intCardNumber = 1 AndAlso intCard1Amount = 1 AndAlso intMulligan > 0 Then
            intCase = 2
        End If

        If intCardNumber = 1 AndAlso intCard1Amount = 2 AndAlso intMulligan = 0 Then
            intCase = 3
        End If

        If intCardNumber = 1 AndAlso intCard1Amount = 2 AndAlso intMulligan > 0 Then
            intCase = 4
        End If

        If intCardNumber = 2 AndAlso intCard1Amount = 1 AndAlso intCard2Amount = 1 AndAlso intMulligan = 0 Then
            intCase = 5
        End If

        If intCardNumber = 2 AndAlso intCard1Amount = 1 AndAlso intCard2Amount = 1 AndAlso intMulligan > 0 Then
            intCase = 6
        End If

        If intCardNumber = 2 AndAlso intCard1Amount = 1 AndAlso intCard2Amount = 2 AndAlso intMulligan = 0 Then
            intCase = 7
        End If

        If intCardNumber = 2 AndAlso intCard1Amount = 1 AndAlso intCard2Amount = 2 AndAlso intMulligan > 0 Then
            intCase = 8
        End If

        If intCardNumber = 2 AndAlso intCard1Amount = 2 AndAlso intCard2Amount = 1 AndAlso intMulligan = 0 Then
            intCase = 7
        End If

        If intCardNumber = 2 AndAlso intCard1Amount = 2 AndAlso intCard2Amount = 1 AndAlso intMulligan > 0 Then
            intCase = 8
        End If

        If intCardNumber = 2 AndAlso intCard1Amount = 2 AndAlso intCard2Amount = 2 AndAlso intMulligan = 0 Then
            intCase = 9
        End If

        If intCardNumber = 2 AndAlso intCard1Amount = 2 AndAlso intCard2Amount = 2 AndAlso intMulligan > 0 Then
            intCase = 10
        End If

        If intCardNumber = 3 AndAlso intCard1Amount = 1 AndAlso intCard2Amount = 1 AndAlso intCard3Amount = 1 AndAlso intMulligan = 0 Then
            intCase = 11
        End If

        If intCardNumber = 3 AndAlso intCard1Amount = 1 AndAlso intCard2Amount = 1 AndAlso intCard3Amount = 1 AndAlso intMulligan > 0 Then
            intCase = 12
        End If

        If intCardNumber = 3 AndAlso intCard1Amount = 1 AndAlso intCard2Amount = 1 AndAlso intCard3Amount = 2 AndAlso intMulligan = 0 Then
            intCase = 13
        End If

        If intCardNumber = 3 AndAlso intCard1Amount = 1 AndAlso intCard2Amount = 1 AndAlso intCard3Amount = 2 AndAlso intMulligan > 0 Then
            intCase = 14
        End If

        If intCardNumber = 3 AndAlso intCard1Amount = 1 AndAlso intCard2Amount = 2 AndAlso intCard3Amount = 1 AndAlso intMulligan = 0 Then
            intCase = 13
        End If

        If intCardNumber = 3 AndAlso intCard1Amount = 1 AndAlso intCard2Amount = 2 AndAlso intCard3Amount = 1 AndAlso intMulligan > 0 Then
            intCase = 14
        End If

        If intCardNumber = 3 AndAlso intCard1Amount = 1 AndAlso intCard2Amount = 2 AndAlso intCard3Amount = 2 AndAlso intMulligan = 0 Then
            intCase = 15
        End If

        If intCardNumber = 3 AndAlso intCard1Amount = 1 AndAlso intCard2Amount = 2 AndAlso intCard3Amount = 2 AndAlso intMulligan > 0 Then
            intCase = 16
        End If

        If intCardNumber = 3 AndAlso intCard1Amount = 2 AndAlso intCard2Amount = 1 AndAlso intCard3Amount = 1 AndAlso intMulligan = 0 Then
            intCase = 13
        End If

        If intCardNumber = 3 AndAlso intCard1Amount = 2 AndAlso intCard2Amount = 1 AndAlso intCard3Amount = 1 AndAlso intMulligan > 0 Then
            intCase = 14
        End If

        If intCardNumber = 3 AndAlso intCard1Amount = 2 AndAlso intCard2Amount = 1 AndAlso intCard3Amount = 2 AndAlso intMulligan = 0 Then
            intCase = 15
        End If

        If intCardNumber = 3 AndAlso intCard1Amount = 2 AndAlso intCard2Amount = 1 AndAlso intCard3Amount = 2 AndAlso intMulligan > 0 Then
            intCase = 16
        End If

        If intCardNumber = 3 AndAlso intCard1Amount = 2 AndAlso intCard2Amount = 2 AndAlso intCard3Amount = 1 AndAlso intMulligan = 0 Then
            intCase = 15
        End If

        If intCardNumber = 3 AndAlso intCard1Amount = 2 AndAlso intCard2Amount = 2 AndAlso intCard3Amount = 1 AndAlso intMulligan > 0 Then
            intCase = 16
        End If

        If intCardNumber = 3 AndAlso intCard1Amount = 2 AndAlso intCard2Amount = 2 AndAlso intCard3Amount = 2 AndAlso intMulligan = 0 Then
            intCase = 17
        End If

        If intCardNumber = 3 AndAlso intCard1Amount = 2 AndAlso intCard2Amount = 2 AndAlso intCard3Amount = 2 AndAlso intMulligan > 0 Then
            intCase = 18
        End If

        If intCardNumber = 4 AndAlso intCard1Amount = 1 AndAlso intCard2Amount = 1 AndAlso intCard3Amount = 1 AndAlso intCard4Amount = 1 AndAlso intMulligan = 0 Then
            intCase = 19
        End If

        If intCardNumber = 4 AndAlso intCard1Amount = 1 AndAlso intCard2Amount = 1 AndAlso intCard3Amount = 1 AndAlso intCard4Amount = 1 AndAlso intMulligan > 0 Then
            intCase = 20
        End If

        If intCardNumber = 4 AndAlso intCard1Amount = 1 AndAlso intCard2Amount = 1 AndAlso intCard3Amount = 1 AndAlso intCard4Amount = 2 AndAlso intMulligan = 0 Then
            intCase = 21
        End If

        If intCardNumber = 4 AndAlso intCard1Amount = 1 AndAlso intCard2Amount = 1 AndAlso intCard3Amount = 1 AndAlso intCard4Amount = 2 AndAlso intMulligan > 0 Then
            intCase = 22
        End If

        If intCardNumber = 4 AndAlso intCard1Amount = 1 AndAlso intCard2Amount = 1 AndAlso intCard3Amount = 2 AndAlso intCard4Amount = 1 AndAlso intMulligan = 0 Then
            intCase = 21
        End If

        If intCardNumber = 4 AndAlso intCard1Amount = 1 AndAlso intCard2Amount = 1 AndAlso intCard3Amount = 2 AndAlso intCard4Amount = 1 AndAlso intMulligan > 0 Then
            intCase = 22
        End If

        If intCardNumber = 4 AndAlso intCard1Amount = 1 AndAlso intCard2Amount = 1 AndAlso intCard3Amount = 2 AndAlso intCard4Amount = 2 AndAlso intMulligan = 0 Then
            intCase = 23
        End If

        If intCardNumber = 4 AndAlso intCard1Amount = 1 AndAlso intCard2Amount = 1 AndAlso intCard3Amount = 2 AndAlso intCard4Amount = 2 AndAlso intMulligan > 0 Then
            intCase = 24
        End If

        If intCardNumber = 4 AndAlso intCard1Amount = 1 AndAlso intCard2Amount = 2 AndAlso intCard3Amount = 1 AndAlso intCard4Amount = 1 AndAlso intMulligan = 0 Then
            intCase = 21
        End If

        If intCardNumber = 4 AndAlso intCard1Amount = 1 AndAlso intCard2Amount = 2 AndAlso intCard3Amount = 1 AndAlso intCard4Amount = 1 AndAlso intMulligan > 0 Then
            intCase = 22
        End If

        If intCardNumber = 4 AndAlso intCard1Amount = 1 AndAlso intCard2Amount = 2 AndAlso intCard3Amount = 1 AndAlso intCard4Amount = 2 AndAlso intMulligan = 0 Then
            intCase = 23
        End If

        If intCardNumber = 4 AndAlso intCard1Amount = 1 AndAlso intCard2Amount = 2 AndAlso intCard3Amount = 1 AndAlso intCard4Amount = 2 AndAlso intMulligan > 0 Then
            intCase = 24
        End If

        If intCardNumber = 4 AndAlso intCard1Amount = 1 AndAlso intCard2Amount = 2 AndAlso intCard3Amount = 2 AndAlso intCard4Amount = 1 AndAlso intMulligan = 0 Then
            intCase = 23
        End If

        If intCardNumber = 4 AndAlso intCard1Amount = 1 AndAlso intCard2Amount = 2 AndAlso intCard3Amount = 2 AndAlso intCard4Amount = 1 AndAlso intMulligan > 0 Then
            intCase = 24
        End If

        If intCardNumber = 4 AndAlso intCard1Amount = 1 AndAlso intCard2Amount = 2 AndAlso intCard3Amount = 2 AndAlso intCard4Amount = 2 AndAlso intMulligan = 0 Then
            intCase = 25
        End If

        If intCardNumber = 4 AndAlso intCard1Amount = 1 AndAlso intCard2Amount = 2 AndAlso intCard3Amount = 2 AndAlso intCard4Amount = 2 AndAlso intMulligan > 0 Then
            intCase = 26
        End If

        If intCardNumber = 4 AndAlso intCard1Amount = 2 AndAlso intCard2Amount = 1 AndAlso intCard3Amount = 1 AndAlso intCard4Amount = 1 AndAlso intMulligan = 0 Then
            intCase = 21
        End If

        If intCardNumber = 4 AndAlso intCard1Amount = 2 AndAlso intCard2Amount = 1 AndAlso intCard3Amount = 1 AndAlso intCard4Amount = 1 AndAlso intMulligan > 0 Then
            intCase = 22
        End If

        If intCardNumber = 4 AndAlso intCard1Amount = 2 AndAlso intCard2Amount = 1 AndAlso intCard3Amount = 1 AndAlso intCard4Amount = 2 AndAlso intMulligan = 0 Then
            intCase = 23
        End If

        If intCardNumber = 4 AndAlso intCard1Amount = 2 AndAlso intCard2Amount = 1 AndAlso intCard3Amount = 1 AndAlso intCard4Amount = 2 AndAlso intMulligan > 0 Then
            intCase = 24
        End If

        If intCardNumber = 4 AndAlso intCard1Amount = 2 AndAlso intCard2Amount = 1 AndAlso intCard3Amount = 2 AndAlso intCard4Amount = 1 AndAlso intMulligan = 0 Then
            intCase = 23
        End If

        If intCardNumber = 4 AndAlso intCard1Amount = 2 AndAlso intCard2Amount = 1 AndAlso intCard3Amount = 2 AndAlso intCard4Amount = 1 AndAlso intMulligan > 0 Then
            intCase = 24
        End If

        If intCardNumber = 4 AndAlso intCard1Amount = 2 AndAlso intCard2Amount = 1 AndAlso intCard3Amount = 2 AndAlso intCard4Amount = 2 AndAlso intMulligan = 0 Then
            intCase = 25
        End If

        If intCardNumber = 4 AndAlso intCard1Amount = 2 AndAlso intCard2Amount = 1 AndAlso intCard3Amount = 2 AndAlso intCard4Amount = 2 AndAlso intMulligan > 0 Then
            intCase = 26
        End If

        If intCardNumber = 4 AndAlso intCard1Amount = 2 AndAlso intCard2Amount = 2 AndAlso intCard3Amount = 1 AndAlso intCard4Amount = 1 AndAlso intMulligan = 0 Then
            intCase = 23
        End If

        If intCardNumber = 4 AndAlso intCard1Amount = 2 AndAlso intCard2Amount = 2 AndAlso intCard3Amount = 1 AndAlso intCard4Amount = 1 AndAlso intMulligan > 0 Then
            intCase = 24
        End If

        If intCardNumber = 4 AndAlso intCard1Amount = 2 AndAlso intCard2Amount = 2 AndAlso intCard3Amount = 1 AndAlso intCard4Amount = 2 AndAlso intMulligan = 0 Then
            intCase = 25
        End If

        If intCardNumber = 4 AndAlso intCard1Amount = 2 AndAlso intCard2Amount = 2 AndAlso intCard3Amount = 1 AndAlso intCard4Amount = 2 AndAlso intMulligan > 0 Then
            intCase = 26
        End If

        If intCardNumber = 4 AndAlso intCard1Amount = 2 AndAlso intCard2Amount = 2 AndAlso intCard3Amount = 2 AndAlso intCard4Amount = 1 AndAlso intMulligan = 0 Then
            intCase = 25
        End If

        If intCardNumber = 4 AndAlso intCard1Amount = 2 AndAlso intCard2Amount = 2 AndAlso intCard3Amount = 2 AndAlso intCard4Amount = 1 AndAlso intMulligan > 0 Then
            intCase = 26
        End If

        If intCardNumber = 4 AndAlso intCard1Amount = 2 AndAlso intCard2Amount = 2 AndAlso intCard3Amount = 2 AndAlso intCard4Amount = 2 AndAlso intMulligan = 0 Then
            intCase = 27
        End If

        If intCardNumber = 4 AndAlso intCard1Amount = 2 AndAlso intCard2Amount = 2 AndAlso intCard3Amount = 2 AndAlso intCard4Amount = 2 AndAlso intMulligan > 0 Then
            intCase = 28
        End If

        intMatches = doTheMath(intCase, intMaxDraws, intPlayerTwoBonus)
        decResults = intMatches / TOTALGAMES
        lblResults.Text = decResults.ToString("P2")
    End Sub
End Class
