'Dave Kurtz
'Hearthstone Card Draw Simulator
'Last Revision: 8/17/16
'Currently Working On: Progress Indicator


Public Class frmMain

    Public Random As New Random
    Public Const TOTALGAMES As Integer = 200000

    Private Sub Player_Select_Click_Events(sender As Object, e As EventArgs) Handles mnuOptionsPlayer1.Click, mnuOptionsPlayer2.Click
        'Set lblPlayer to either 1 or 2
        lblPlayer.Text = sender.tag

        'Enable Mulligan Menu
        mnuOptionsMulligan.Visible = True

        'Check if Player 1 or Player 2 was clicked
        If lblPlayer.Text = "1" Then
            'Player 1 was clicked; Hide 4th card menu options
            mnuOptionsMulligan4.Visible = False
            mnuOptionsWanted4.Visible = False
            mnuOptionsAmount4thCard.Visible = False

            'set 4th card labels to appropriate values
            lblCard4Amount.Text = String.Empty
            If lblWantedCards.Text = "4" Then
                lblWantedCards.Text = "3"
            End If
            If lblMulligan.Text = "4" Then
                lblMulligan.Text = "3"
            End If
        ElseIf lblPlayer.Text = "2" Then
            'Player 2 was clicked; Show 4th card menu options
            mnuOptionsMulligan4.Visible = True
            mnuOptionsWanted4.Visible = True
        End If
    End Sub

    Private Sub Mulligan_Select_Click_Events(sender As Object, e As EventArgs) Handles mnuOptionsMulligan0.Click, mnuOptionsMulligan1.Click, mnuOptionsMulligan2.Click,
        mnuOptionsMulligan3.Click, mnuOptionsMulligan4.Click

        'Enable Wanted Cards menu options
        mnuOptionsWanted.Visible = True

        'set lblMulligan to user-selected value
        lblMulligan.Text = sender.text
    End Sub

    Private Sub Wanted_Cards_Select_Click_Events(sender As Object, e As EventArgs) Handles mnuOptionsWanted1.Click, mnuOptionsWanted2.Click, mnuOptionsWanted3.Click,
        mnuOptionsWanted4.Click

        'Enable appropriate Wanted Card Amount menu options
        mnuOptionsAmount.Visible = True
        Select Case sender.text
            Case "1"
                mnuOptionsAmount1stCard.Visible = True
                mnuOptionsAmount2ndCard.Visible = False
                mnuOptionsAmount3rdCard.Visible = False
                mnuOptionsAmount4thCard.Visible = False
                lblCard2Amount.Text = String.Empty
                lblCard3Amount.Text = String.Empty
                lblCard4Amount.Text = String.Empty
            Case "2"
                mnuOptionsAmount1stCard.Visible = True
                mnuOptionsAmount2ndCard.Visible = True
                mnuOptionsAmount3rdCard.Visible = False
                mnuOptionsAmount4thCard.Visible = False
                lblCard3Amount.Text = String.Empty
                lblCard4Amount.Text = String.Empty
            Case "3"
                mnuOptionsAmount1stCard.Visible = True
                mnuOptionsAmount2ndCard.Visible = True
                mnuOptionsAmount3rdCard.Visible = True
                mnuOptionsAmount4thCard.Visible = False
                lblCard4Amount.Text = String.Empty
            Case "4"
                mnuOptionsAmount1stCard.Visible = True
                mnuOptionsAmount2ndCard.Visible = True
                mnuOptionsAmount3rdCard.Visible = True
                mnuOptionsAmount4thCard.Visible = True
        End Select

        'Set lblWantedCards to user-selected value
        lblWantedCards.Text = sender.text
    End Sub

    Private Sub Wanted_Cards_Amounts_Select_Click_Events(sender As Object, e As EventArgs) Handles mnuOptionsAmount1stCard1.Click, mnuOptionsAmount1stCard2.Click, mnuOptionsAmount2ndCard1.Click,
        mnuOptionsAmount2ndCard2.Click, mnuOptionsAmount3rdCard1.Click, mnuOptionsAmount3rdCard2.Click, mnuOptionsAmount4thCard1.Click, mnuOptionsAmount4thCard2.Click

        'Send user-selected value to the appropriate lbl
        Select Case sender.tag
            Case "1"
                lblCard1Amount.Text = sender.text
            Case "2"
                lblCard2Amount.Text = sender.text
            Case "3"
                lblCard3Amount.Text = sender.text
            Case "4"
                lblCard4Amount.Text = sender.text
        End Select

    End Sub

    Private Sub btnCalculate_Click(sender As Object, e As EventArgs) Handles btnCalculate.Click
        'Clear Errors
        ErrorProvider1.Clear()

        'Check to make sure User entered valid info for all required fields
        If lblPlayer.Text = String.Empty Then
            'No player selected
            ErrorProvider1.SetError(lblPlayer, "Please select either First or Second Player from the Options Menu.")
        ElseIf lblMulligan.Text = String.Empty Then
            'No mulligan selected
            ErrorProvider1.SetError(lblMulligan, "Please select a Mulligan amount from the Options Menu.")
        ElseIf lblWantedCards.Text = String.Empty Then
            'No Wanted cards selected
            ErrorProvider1.SetError(lblWantedCards, "Please select the number of Wanted Cards from the Options Menu.")
        ElseIf lblCard1Amount.Text = String.Empty Then
            'No amount entered for Card 1
            ErrorProvider1.SetError(lblCard1Amount, "Please select how many copies of Card 1 is in the deck from the Options Menu. ")
        ElseIf lblCard2Amount.Text = String.Empty AndAlso lblWantedCards.Text > 1 Then
            'User wants at least 2 cards but no copy amount set for card 2
            ErrorProvider1.SetError(lblCard2Amount, "Please select how many copies of Card 2 is in the deck from the Options Menu.")
        ElseIf lblCard3Amount.Text = String.Empty AndAlso lblWantedCards.Text > 2 Then
            'User wants at least 3 cards but no copy amount set for card 3
            ErrorProvider1.SetError(lblCard3Amount, "Please select how many copies of Card 3 is in the deck from the Options Menu.")
        ElseIf lblCard4Amount.Text = String.Empty AndAlso lblWantedCards.Text > 3 Then
            'User wants 4 cards but no copy amount set for card 4
            ErrorProvider1.SetError(lblCard4Amount, "Please select how many copies of Card 4 is in the deck from the Options Menu.")
        Else
            'All required fields have valid info - declare Input-holding variables
            Dim strPlayer, strMulligan, strWantedCards, strCard1Amount, strCard2Amount, strCard3Amount, strCard4Amount As String
            Dim intPlayer, intMulligan, intWantedCards, intCard1Amount, intCard2Amount, intCard3Amount, intCard4Amount As Integer

            'Declare calculations variables
            Dim intWantedSentinels, intTotalHits, intMaxDraws, intGamesCounter, intIncrementor, intDrawsCounter, intDraw As Integer
            Dim decResults As Decimal

            'Get strInputs and parse to intInputs
            strPlayer = lblPlayer.Text
            strMulligan = lblMulligan.Text
            strWantedCards = lblWantedCards.Text
            strCard1Amount = lblCard1Amount.Text
            strCard2Amount = lblCard2Amount.Text
            strCard3Amount = lblCard3Amount.Text
            strCard4Amount = lblCard4Amount.Text
            Integer.TryParse(strPlayer, intPlayer)
            Integer.TryParse(strMulligan, intMulligan)
            Integer.TryParse(strWantedCards, intWantedCards)
            Integer.TryParse(strCard1Amount, intCard1Amount)
            Integer.TryParse(strCard2Amount, intCard2Amount)
            Integer.TryParse(strCard3Amount, intCard3Amount)
            Integer.TryParse(strCard4Amount, intCard4Amount)

            'Convert Player select into "Player Bonus"
            intPlayer -= 1

            'Begin Games Loop
            For intGamesCounter = 1 To TOTALGAMES
                'Create Draws Array
                Dim intDrawsArray(7) As Integer

                'Populate Draws array with values 1-8
                'IntDrawsArray(0) + (1) = Wanted Card 1 Copies 1 and 2, respectively
                'IntDrawsArray(2) + (3) = Card 2 Copies 1 and 2
                '(4) + (5) = Card 3, 6+7 = Card 4
                For intIncrementor = 0 To 7
                    intDrawsArray(intIncrementor) = intIncrementor + 1
                Next

                'Reset WANTED Sentinel counter
                intWantedSentinels = 0

                'Calculate Max Draws
                intMaxDraws = 3 + intPlayer + intMulligan

                'Begin Draws Loop
                For intDrawsCounter = 1 To intMaxDraws
                    'Pick a card, any card
                    intDraw = Random.Next(1, 32 - intDrawsCounter)

                    'See if card matches a Wanted card
                    For intIncrementor = 0 To 7
                        If intDraw = intDrawsArray(intIncrementor) Then
                            'Set matching Card Array to WANTED Sentinel value (0)
                            intDrawsArray(intIncrementor) = 0
                        End If
                    Next
                Next 'End Draws Loop

                'Take Mulligans out of the equation in prep for 1st turn draw
                intMaxDraws -= intMulligan

                '1st turn draw
                intDraw = Random.Next(1, 32 - intMaxDraws)

                'See if card matches a Wanted card
                For intIncrementor = 0 To 7
                    If intDraw = intDrawsArray(intIncrementor) Then
                        'Set matching Card Array to WANTED Sentinel value (0)
                        intDrawsArray(intIncrementor) = 0
                    End If
                Next

                'Check to see if less than 2 copies of Card 1
                If intCard1Amount = 1 Then
                    'Only 1 copy of Card 1 - set 2nd copy to UNWANTED Sentinel value (99)
                    intDrawsArray(1) = 99
                End If

                'Check to see if less than 2 copies of Card 2
                Select Case intCard2Amount
                    Case "0" '0 Copies of Card 2 - take arrays 3 + 4 out
                        intDrawsArray(2) = 99
                        intDrawsArray(3) = 99
                    Case "1" '1 Copy of Card 2 - take array 4 out
                        intDrawsArray(3) = 99
                End Select

                'Repeat ^, but for Card 3
                Select Case intCard3Amount
                    Case "0"
                        intDrawsArray(4) = 99
                        intDrawsArray(5) = 99
                    Case "1"
                        intDrawsArray(5) = 99
                End Select

                'Same as above, but for Card 4
                Select Case intCard4Amount
                    Case "0"
                        intDrawsArray(6) = 99
                        intDrawsArray(7) = 99
                    Case "1"
                        intDrawsArray(7) = 99
                End Select

                'Count the Wanted Sentinels
                For intIncrementor = 0 To 7
                    If intDrawsArray(intIncrementor) = 0 Then
                        intWantedSentinels += 1
                    End If
                Next

                'Check to see if Wanted Sentinels matches Wanted Cards
                If intWantedSentinels = intWantedCards Then
                    'Count as a hit
                    intTotalHits += 1
                End If
            Next 'End Games Loop

            'Calculate and display Results
            decResults = intTotalHits / TOTALGAMES
            lblResults.Text = decResults.ToString("P2")
        End If
    End Sub
End Class
