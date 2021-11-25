#|
************************************************************
* Name:     Nima Naghizadehbaee                            *
* Project:  Project 2 - LISP Mexican Train                 *
* Class:    CMPS 366 - OPL                                 *
* Date:     3/13/21                                        *
************************************************************
|#


#| *********************************************
Source Code to start the Mexican Train game
********************************************* |#

#|
/* ********************************************************************* 
Function Name: startgame 
Purpose: To start up the Mexican Train game
Parameters: None
Return Value: NIL indicating success
Algorithm: 
            1) Ask user whether they would like to start a new game or load a game
       		2)
       			if user wants to start a new game
       				set up the first round
       				play as many rounds as the user wants
       				if user completed all the rounds it started
       					display the game results 
       				end if 
       			end if 
       			if user wants to load a game
       				play loaded game for as many rounds as user wants
       				if user completed all the rounds it started 
       					display the game results
       				end if 
       			end if 

Assistance Received: none 
********************************************************************* */
|#

(defun startgame ()

	;welcome user to the game
	(displaywelcomemsg)

	(let*
		(
			;regarding starting a new game or loading game 
			(userSelection (startmenu) )

			;menu options
			(newGameOption 1)
			(loadGameOption 2)
															)
		(cond 

			;if user wants to start a new game
			( (= userSelection newGameOption)

				(let*
					(
						;play the game
						;returns player scores and the incomplete round indicator
						(gameResult (playgame (setupfirstround) ) )

						;establish the player scores
						(playerScores (first gameResult) )

						;establish the incomplete round indicator
						(incompleteRound (first (rest gameResult) ) )
																				)

					(cond

						;if user did not quit or save game during the round 
						( (not incompleteRound)

							;display the game results
							(displaygameresult playerScores)
						)
					)
				)
			)

			;if user wants to load game
			( (= userSelection loadGameOption)

				(let*
					(
						;play the game
						(gameResult (playgame (loadgame) ) )

						;establish the player scores
						(playerScores (first gameResult) )

						;establish the incomplete round indicator
						(incompleteRound (first (rest gameResult) ) )
																			)

					(cond

						;if user did not quit or save game during the round 
						( (not incompleteRound)

							;display the game results
							(displaygameresult playerScores)
						)
					)
				)
			)
		)
	)

	;display end message
	(displayendmsg)
)



#| *********************************************
Source Code to display messages to user
********************************************* |#

#|
/* ********************************************************************* 
Function Name: displaywelcomemsg 
Purpose: To welcome user to the game 
Parameters: None
Return Value: None
Assistance Received: none 
********************************************************************* */
|#

(defun displaywelcomemsg ()

	(printnewlines 4)
	(printchar #\space 53)
	(princ "WELCOME TO THE MEXICAN TRAIN GAME")
	(printnewlines 4)
	#\space
)


#|
/* ********************************************************************* 
Function Name: printtilereasoning 
Purpose: To inform user about the logistics behind computer's tile selection
Parameters: 
			currentPlayer, a boolean indicating whether the player 
			currently playing is the computer (t) or human (NIL). 
			reasonCode, an integer that will be mapped to the strategy
			used by the computer to select a tile. 
			selectedTile, a list of 2 integers representing the tile
			selected by the computer to play

Return Value: None
Algorithm: 
	`(1) find the reasoning that explains the computer tile selection strategy
	 (2) display this reasoning

Assistance Received: none 
********************************************************************* */
|#

(defun printtilereasoning (currentPlayer reasonCode selectedTile)

	(terpri)

	(cond 

		;if computer is reasoning its own move
		(currentPlayer

			(princ "THE COMPUTER PLAYED")
		)

		;if computer is providing help
		(t
			(princ "THE COMPUTER SUGGESTS PLAYING")

		)
	)

	(princ " THIS TILE BECAUSE IT IS THE ")


	(let*

		;the reasoning codes
		(
			(oneDouble 0)
			(highestDouble 1)
			(oneMatchingDouble 2)
			(mostMatchingDouble 3)
			(mostMatchingDoubleMaxPip 4)
			(oneTile 5)
			(highestTile 6)
			(oneDoublePlayed 7)
			(doublePlayedMaxPip 8)
										)


		(cond 

			( (= reasonCode oneDouble)
				(princ "ONLY PLAYABLE DOUBLE IN HAND")
			)

			( (= reasonCode highestDouble)
				(princ "PLAYABLE DOUBLE WITH THE HIGHEST PIP SUM")
			)

			( (= reasonCode oneMatchingDouble )
				(princ "ONLY PLAYABLE DOUBLE THAT HAS ANOTHER MATCHING TILE IN HAND")
			)

			( (= reasonCode mostMatchingDouble )
				(princ "PLAYABLE DOUBLE WITH THE MOST NUMBER OF TILES IN HAND THAT MATCH IT")
			)

			( (= reasonCode mostMatchingDoubleMaxPip )
				(princ "PLAYABLE DOUBLE WITH THE MOST NUMBER OF MATCHING TILES AND THE HIGHEST PIP SUM")
			)

			( (= reasonCode oneTile )
				(princ "ONLY PLAYABLE TILE")
			)

			( (= reasonCode highestTile )
				(princ "PLAYABLE TILE WITH THE HIGHEST PIP SUM")
			)

			( (= reasonCode oneDoublePlayed )
				(princ "ONLY PLAYABLE NON-DOUBLE WITH ITS DOUBLE(S) PLAYED")
			)

			( (= reasonCode doublePlayedMaxPip )
				(princ "PLAYABLE NON-DOUBLE WITH ITS DOUBLE(S) PLAYED AND THE HIGHEST PIP SUM")
			)
		)

		(princ " --> ")
		(printtile selectedTile)
	)
)


#|
/* ********************************************************************* 
Function Name: printtrainreasoning
Purpose: To inform user about the logistics behind compuetr's train selection
Parameters: 
			currentPlayer, a boolean indicating whether the player 
			currently playing is the computer (t) or human (NIL). 
			reasonCode, an integer that will be mapped to the strategy
			used by the computer to select a train. 
			selectedTrain, an integer indicating the train
			selected by the computer to play a tile on

Return Value: None
Algorithm: 
	 (1) find the reasoning that explains the computer train selection strategy
	 (2) display this reasoning

Assistance Received: none 
********************************************************************* */
|#

(defun printtrainreasoning (currentPlayer reasonCode selectedTrain)

	(terpri)

	(cond 

		;if computer is reasoning its own move
		(currentPlayer

			(princ "THE COMPUTER SELECTED THE")
		)

		;if computer is providing help
		(t
			(princ "THE COMPUTER SUGGESTS SELECTING THE")

		)
	)

	(princ " FOLLOWING TRAIN BECAUSE IT ")

	(let*

		
		(
			;the reasoning codes
			(oneEligibleTrain 0)
			(opponentUnavail 1)
			(personalMexican 2)

			;selection result
			(compSelected 1)
			(humanSelected 2)
			(MexicanSelected 3)
									)

		(cond 

			( (= reasonCode oneEligibleTrain)
				(princ "IS THE ONLY ELIGIBLE TRAIN MATCHING THE SELECTED TILE")
			)

			( (= reasonCode opponentUnavail)
				(princ "COULD BE UNMARKED AND NOT AVAILABLE IN THE NEXT TURN")
			)

			( (= reasonCode personalMexican)
				(princ "IS ALMOST ALWAYS AVAILABLE TO THE OPPONENT")
			)			
		)

		(princ " --> ")

		(cond 

			( (= selectedTrain compSelected)

				(princ "COMPUTER TRAIN")
			)

			( (= selectedTrain humanSelected)

				(princ "HUMAN TRAIN")
			)

			( (= selectedTrain MexicanSelected)

				(princ "MEXICAN TRAIN")
			)
		)

		(printnewlines 2)
	)
)


#|
/* ********************************************************************* 
Function Name: displayendmsg
Purpose: To display a message to user upon termination
Parameters: None
Return Value: None
Assistance Received: none 
********************************************************************* */
|#

(defun displayendmsg ()

	(printnewlines 2)
	(printchar #\space 45)
	(princ "THANK YOU FOR PLAYING THE MEXICAN TRAIN GAME")
	(printnewlines 4)
)


#| *********************************************
Source Code for displaying menus to user
********************************************* |#


#|
/* ********************************************************************* 
Function Name: startmenu
Purpose: To ask user to start a new game or load game
Parameters: None
Return Value: an integer indicating user's selection
				1 --> start a new game 
				2 --> load game
Assistance Received: none 
********************************************************************* */
|#
	
(defun startmenu ()

	(princ "1) START NEW GAME")
	(terpri)
	(princ "2) LOAD GAME")
	(terpri)

	(let*

		(
			(userSelection (read) )

			;menu options
			(newGameOption 1)
			(loadGameOption 2)

									)
		(cond 

			;if user selection is the right type 
			( (numberp userSelection)

				(cond 

					( (= userSelection newGameOption)

						newGameOption
					)

					( (= userSelection loadGameOption)

						loadGameOption
					)

					(t
						(terpri)
						(startmenu)
					)
				)
			)

			;if user selection is not the right type 
			(t
				(terpri)
				(startmenu)
			)
		)
	)
)


#|
/* ********************************************************************* 
Function Name: selectaction
Purpose: To ask user to specify the next action taken in the game
Parameters: 
			currentPlayer, a boolean indicating whether the player 
			currently playing is the computer (t) or human (NIL). 

Return Value: an integer indicating user's selection
				1 --> save game 
				2 --> make move
				3 --> ask for help (human only)
				4 --> quit
Algorithm: 
	 (1) prompt user to select an option
	 (2) 
	 	if user's selection is valid
	 		record the selected option
	 	end if 

	 	else 
	 		prompt user to select again
	 	end else

Assistance Received: none 
********************************************************************* */
|#

(defun selectaction (currentPlayer)

	(printnewlines 2)
	
	(cond 

		;if current player is computer 
		(currentPlayer
			(princ "COMPUTER TURN")
		)

		;if current player is human
		(t
			(princ "YOUR TURN")
		)
	)
	
	(printnewlines 2)

	(princ "SELECT ONE OF THE FOLLOWING OPTIONS:")
	(terpri)
	(princ "1) SAVE THE GAME ")
	
	(terpri)

	(cond 

		;if current player is computer 
		(currentPlayer
			(princ "2) LET COMPUTER MAKE A MOVE")
		)

		;if current player is human
		(t
			(princ "2) MAKE A MOVE")
		)
	)

	(terpri)

	(cond 

		;if current player is computer
		(currentPlayer

			(princ "3) ASK FOR HELP - N/A")
			(terpri)
		)

		(t
			(princ "3) ASK FOR HELP")
			(terpri)
		)
	)	

	(princ "4) QUIT THE GAME")
	(terpri)


	(let*

		(
			(userSelection (read) )
									)

		(cond 

			;if current player is computer
			(currentPlayer

				(cond

					;if user selection is a number
					( (numberp userSelection)

						(cond 

							;if user made a valid selection
							( (or (or (= userSelection 1) (= userSelection 2) ) (= userSelection 4) )

								(printnewlines 3)
								userSelection
							)

							;if user did not make a valid selection
							(t
								;user must select again
								(selectaction currentPlayer)
							)
						)
					)

					;if user selection is not a number
					(t
						;user must select again
						(selectaction currentPlayer)
					)
				)
			)

			;if current player is human
			(t

				(cond

					;if user selection is a number
					( (numberp userSelection)

						(cond 

							;if user made a valid selection
							( (or (or (= userSelection 1) (= userSelection 2) ) (or (= userSelection 3) (= userSelection 4) ) )

								(terpri)
								userSelection
							)

							;if user did not make a valid selection
							(t
								;user must select again
								(selectaction currentPlayer)
							)
						)
					)

					;if user selection is not a number
					(t
						;user must select again
						(selectaction currentPlayer)
					)
				)
			)
		)
	)
)

#|
/* ********************************************************************* 
Function Name: displaytraineligbility
Purpose: To display which trains are eligible to the user to select
Parameters: 
			trainNames, a list of 3 strings displayed as names of 
			game trains to user. 
			indicators, a list of 3 booleans indicating whether 
			each game train is eligible or not.
			num, an integer showing the number corresponding to
			each train

Return Value: None
Algorithm: 
	 (1) list all the game trains
	 (2) indicate which trains are not eligible

Assistance Received: none 
********************************************************************* */
|#

(defun displaytraineligbility (trainNames indicators num)
	
	(cond 

		( (null trainNames)

			()
		)

		(t
			(princ num)
			(princ ") ")
			(princ (first trainNames) )
			(princ " TRAIN" )

			(cond

				;if the indicator is not set
				( (not (first indicators) )

					(princ " - NOT ELIGIBLE")
					(terpri)
					(displaytraineligbility (rest trainNames) (rest indicators) (+ num 1) )
				)

				;if the indicator is set
				(t
					;record this as a valid option
					(terpri)
					(cons 
						num
						(displaytraineligbility (rest trainNames) (rest indicators) (+ num 1) )
					)
				)
			)
		)
	)
)

#|
/* ********************************************************************* 
Function Name: anotherroundmenu 
Purpose: To ask user whether they want to play another round
Parameters: 
			incompleteRound, a boolean indicating whether the user 
			has completed the round it has started or not.
			
Return Value: a boolean indicating user's selection
				t --> play another round
				NIL --> do not play another round

Algorithm: 
	 (1) Ask user whether they would like to play another round
	 (2) 
	 	if user made a valid selection
	 		record the selection
	 	end if 

	 	else 
	 		prompt user to select again
	 	end else 

Assistance Received: none 
********************************************************************* */
|#

(defun anotherroundmenu (incompleteRound)

	(cond 

		;if the user has saved or quit the game
		(incompleteRound

			(printnewlines 2)

			;user does not want to play another round
			NIL
		)

		;if user has not saved or quit the game during the round
		(t
			(terpri)
			(princ "WOULD YOU LIKE TO PLAY ANOTHER ROUND?")
			(terpri)
			(princ "1) YES")
			(terpri)
			(princ "2) NO")
			(terpri)

			(let*

				(
					;the options to select from
					(yesSelected 1)
					(noSlected 2)

					(userInput (read) )
												)

				(cond 

					;if user selection is a number 
					( (numberp userInput)

						(cond 

							;if player wants to play another round
							( (= userInput yesSelected)

								(printnewlines 2)
								t
							)

							;if player does not want to play another round
							( (= userInput noSlected)

								(printnewlines 2)
								NIL
							)

							;if user entered an invalid number
							(t
								(printnewlines 2)
								
								;invalid input
								(anotherroundmenu incompleteRound)
							)
						)
					)

					;if user selection is not a number 
					(t
						(printnewlines 2)
						
						;invalid input
						(anotherroundmenu incompleteRound)
					)
				)
			)
		)
	)
)



#| *********************************************
Source Code to play a game
********************************************* |#

#|
/* ********************************************************************* 
Function Name: setupfirstround
Purpose: To start a game from scratch
Parameters: None			
Return Value: a list of lists consisted of all elements of the game 
Algorithm: 
	 (1) set the initial round number and player scores
	 (2) set up the tiles for the first round

Assistance Received: none 
********************************************************************* */
|#

(defun setupfirstround ()

	(let* 
		( 
			;the starting round
			(roundNumber 1)

			;player's inital scores
			(compScore 0)
			(humanScore 0)
			(playerScores (list compScore humanScore) )

			;the elements of the game
			(gameElem (list roundNumber playerScores) )			
		 													)

		;set up the tiles for the first round
		(setupround gameElem)						
	)
)

#|
/* ********************************************************************* 
Function Name: playgame
Purpose: To continue playing rounds until user quits or saves the game
Parameters: 
			fullGame, a list of lists consisted of all elements of 
			the game 

Return Value: a list consisted of the player socres and a boolean
			  indicating whether user has completed the round 
			  (incomplete round indicator)
Algorithm: 
	 (1) identify the elements of the entire game
	 (2) identify the elements that are specific to this round
	 (3) set up the turn indicators 
	 (4) play a round
	 (5) 
	 	if user completed the round it started 
	 		ask user whether they want to play another round
	 		
	 		if user wants to play another round
	 			set up the elements of the next round
	 			play another round (recursive)
	 		end if

	 		else 
	 			get the final player scores 
	 			note that the game should end now
	 		end else 	
	 	
	 	end if 

	 	else 
	 		get the final player scores 
	 		note that the game should end now
	 	end else
	 

Assistance Received: none 
********************************************************************* */
|#


(defun playgame (fullGame)

	(let*

		;establish the game elements
		(
			;extract the game list
			(gameElem (first fullGame) )

			;extract the round list
			(roundElem (first (rest fullGame) ) )

			;set up the turn indicators
			;a series of booleans and a value that report the status of the round
			
			;set to true when a player exhausts their hand
			;or both players skip their turn because of an empty boneyard
			(roundCompleted NIL)
			
			;set to true when a player picks from boneyard during their turn
			(boneyardDrew NIL)
			
			;set to true when computer skips its turn because of an empty boneyard 
			(computerSkips NIL)
			
			;set to true when human skips its turn because of an empty boneyard 
			(humanSkips NIL)
			
			;the number of doubles played by a player in a "full" turn
			(doublesPlaced 0)

			;set to true when user saves the game or quits during the round
			(incompleteRound NIL)

			;to hold all indicators together
			(turnIndicators (list roundCompleted boneyardDrew computerSkips humanSkips doublesPlaced incompleteRound) )

			;play the round (returns --> game element + incomplete indicator)
			(gameAndMarker (playround gameElem roundElem turnIndicators) )

			;extract the game element
			(newGameElem (first gameAndMarker) )

			;extract the incomplete indicator
			(incompleteRUpdated (first (rest gameAndMarker) ) )

			;prompt user to play for another round (if applicable) - i.e. user has not quit or saved the game
			(anotherRound (anotherroundmenu incompleteRUpdated) )
																														)

		(cond 

			;if user wants to play another round
			(anotherRound

				(let*

					(
						;set up the next round (returns a full game)
						(nextRound (setupround newGameElem) )
																		)

					(playgame nextRound)
				)
			)

			;if user does not want to play another round
			(t

				(let*

					(
						;establish final scores for the round
						(playerScores (getplayerscores newGameElem) )

						;append the incomplete round indicator
						(returnList (list playerScores incompleteRUpdated ) )
																					)
					;note that game is over
					returnList
				)
			)
		)
	)
)



#| *********************************************
Source Code to set up and play a round
********************************************* |#

#|
/* ********************************************************************* 
Function Name: setupround
Purpose: To create the initial elements of a round
Parameters: 
			gameElem, a list consisted of the round number 
			and the player scores (list)

Return Value: a list of lists consisted of all elements of the game 
Algorithm: 
	 (1) create the game deck 
	 (2) identify the round engine
	 (3) remove the engine from the deck
	 (4) shuffle the deck
	 (5) randomly distribute 16 tiles to each player from the deck 
	 (6) place the remaining tiles in the boneyard
	 (7) set up the default train markers (regular and orphan marker)
	 (8) add the default markers to the game trains
	 (9) identify the player starting the round 	 

Assistance Received: none 
********************************************************************* */
|#

(defun setupround (gameElem)

	(let* 
		( 
			;the game elements
			(roundNumber (getroundnumber gameElem) ) 
			(playerScores (getplayerscores gameElem) )
			(compScore (getcompscore playerScores) )
			(humanScore (gethumanscore playerScores) )

			;the smallest pip of a tile 
			(minPip 0)

			(gameDeck (createdeck minPip minPip) )

			(engineVal (identifyengine roundNumber) )

			(roundEngine (list engineVal engineVal) )

			;the game deck without the engine
			(deckNoEngine  (removetile gameDeck roundEngine) )	
			 																	
			;the shuffled deck without the engine
			(finalDeck (shuffle deckNoEngine) ) 

			;the number of tiles to distribute to each player
			(numTiles 16)

			;distribute tiles to computer player
			(compHand (distributehand finalDeck numTiles) )

			;remove the tiles in computer hand from deck
			(deckNoComp (removetiles finalDeck compHand) )

			;distribute tiles to human player
			(humanHand (distributehand deckNoComp numTiles) )

			;the player hands
			(playerHands (list compHand humanHand) )

			;remove the tiles in human hand from deck
			(deckNoHuman (removetiles deckNoComp humanHand) )

			;the remaining tiles make up the boneyard
			(boneyard deckNoHuman)

			;orphan double and marker indicators
			;initially no train is marked nor is orphan double
			;first NIL --> regular marker 
			;second NIL --> orphan double marker
			(trainIndicator (list NIL NIL) )

			;the trains in the game
			(compTrain (addtofront () trainIndicator) )
			(humanTrain (addtofront () trainIndicator) )
			(mexicanTrain (addtofront () trainIndicator) )
			(gameTrains (list compTrain humanTrain mexicanTrain) )

			;the player starting the round
			(firstPlayer (identifyfirstplayer compScore humanScore) )

			;the elements of the round 
			(roundElem (list playerHands boneyard roundEngine gameTrains firstPlayer) )

			;the full game 
			(fullGame (list gameElem roundElem) )
																							)
		fullGame
	)
)

#|
/* ********************************************************************* 
Function Name: playround
Purpose: To play a round 
Parameters: 
			gameElem, a list consisted of the round number 
			and the player scores (list)
			roundElem, a list of lists consisted of the elements of a round 
			(player hands, boneyard, engine, game trains, current player)
			turnIndicators, a list of booleans and a number that ensure
			the rules of the game are followed in each turn

Return Value: a list consisted of the elements of the game (the round 
			  number and player scores) and a boolean indicating whether 
			  user has completed the round (incomplete round indicator)
Algorithm: 
	 (1) 
	 	if the round is over
	 		calculate the pip sum for both player hands
	 		determine and display the winner 	
	 		display the scores for this round
	 	end if 

	 	else if user saved the game or quit during the round
	 		display the goodbye message to user
	 	end if 

	 	else (i.e. the round is not over)
	 		note that current player has not picked from boneyard
	 		note that current player has not played any doubles
	 		establish the orphan doubles (if applicable)
	 		allow the current player to make a move 
	 		switch to the other player
	 		allow this player to make a move (recursive)
	 	end else

Assistance Received: none 
********************************************************************* */
|#

(defun playround (gameElem roundElem turnIndicators)

	(let*

		(
			;establish the player hands
			(playerHands (getplayerhands roundElem))
			(compHand (getcomphand playerHands))
			(humanHand (gethumanhand playerHands))

			;to check whether the round is over
			(roundCompleted (getroundstatus turnIndicators) )

			;to check whether player has saved or quit the game
			(incompleteRound (getincompletestatus turnIndicators) )
																		)

		(cond 

			;if either player has emptied their hand
			;or both players skipped turn because of an empty boneyard  
			(roundCompleted

				(let*

					(
						;calculate pip sum for both hands
						(compScore (getpilepipsum compHand) )
						(humanScore (getpilepipsum humanHand) )
						
						;determine the winner
						(roundOutcome (displayroundresult compHand humanHand) )

						;the updated game (player scores and round number updated)
						(finalRound (endofroundupdate gameElem compHand humanHand) )

						;append the incomplete round indicator
						(returnList (list finalRound incompleteRound ) )
																						)

						;announce the round winner
						(announceeventresult "ROUND" roundOutcome)

						;display the round scores
						(displayeventscores compScore humanScore)

						;the game list + incomplete round indicator
						returnList
				)
			)

			;if user has saved or quit the game
			(incompleteRound

				(let*

					(
						
						;append the incomplete round indicator
						(returnList (list gameElem incompleteRound) )
																			)
						;the game list + incomplete round indicator
						returnList
				)
			)

			;if the round is not over
			(t
				;players keep taking turns

				(let*

					(
						;reset the boneyard indicator
						(boneyardReset (setboneyardmarker turnIndicators NIL) )

						;reset the doubles indicator
						(doublesReset (setdoublesplayed boneyardReset 0))

						;get the game trains
						(gameTrains (gettrains roundElem) )

						;establish orphan doubles
						(orphanTrains (establishorphandoubles gameTrains) )

						;update the round
						(orphanRound (updatetrains roundElem orphanTrains) )
						
						;allow the player to make a move(s)
						(turnTookRound (playermakesmove gameElem orphanRound doublesReset) )

						;extract the turn indicators
						(newTurnIndicators (first (rest turnTookRound) ) )

						;extract the updated round
						(roundUpdated (first turnTookRound) )

						;switch to the next player
						(updatedRound (updatecurrentplayer roundUpdated) )
																									)

					(playround gameElem updatedRound newTurnIndicators)
				)
			)
		)
	)
)

#|
/* ********************************************************************* 
Function Name: playermakesmove
Purpose: To allow a player to make a move
Parameters: 
			gameElem, a list consisted of the round number 
			and the player scores (list)
			roundElem, a list of lists consisted of the elements of a round 
			(player hands, boneyard, engine, game trains, current player)
			turnIndicators, a list of booleans and a number that ensure
			the rules of the game are followed in each turn

Return Value: a list consisted of the elements of the round and the
			  list of turn indicators 
Algorithm: 
	if the round is not over
		prompt user to select an action

		if player wants to save the game
			mark this round as incomplete
			write the game to a text file
		end if 

		else if player wants to quit the game
			mark this round as incomplete
			display goodbye message
		end else 

		else if player wants to get help from computer (human only)
			
			if human has at least one possible move to make
				inform player about which tile to play
				inform player about which train to play the tile on
			end if
			
			else 
				inform player that they have no moves to make
			end else

			this player goes again
		
		end else if

		else if player wants to make a move
			if player has no possible moves
				if player has not yet picked from boneyard
					if boneyard is not empty
						pick a tile from boneyard
						note that this player has picked from boneyard
					end if

					else 
						note that player skipped turn because boneyard is empty
					end else 
				else 
					mark the player's train
				end else
			else 
				prompt player to select a tile 
				prompt player to select a train

				if the tile matches the train
					add the tile to the train
					remove the orphan double marker for this train (if applicable)
					
					if player placed tile on personal train
						unmark player's train (if applicable)
					end if

					if player placed a double
						note that this player has placed a double
						allow this player to pick from boneyard (if needed)
						player goes again since a double is placed
					end if 
				end if 

				else 
					prompt player to select another pair of tile and train
				end else 
			end else
		end else if
	end if

	else 
		note that the round is over
	end else 

Assistance Received: none 
********************************************************************* */
|#

(defun playermakesmove (gameElem roundElem turnIndicators)

	(displaygame gameElem roundElem)

	(let*

		(
			(playerHands (getplayerhands roundElem) )

			;establish the player's hand sizes (for checking round ending conditions)
			(compHandSize (length (getcomphand playerHands) ) )
			(humanHandSize (length (gethumanhand playerHands) ) )

			;estbablish the computer and human skips indicators
			(compSkips (compskippedturn turnIndicators) )
			(humanSkips (humanskippedturn turnIndicators) )

																						)

		(cond

			;if the round is not completed
			( (not (isroundcompleted compHandSize humanHandSize compSkips humanSkips) )

				(let*
					(
						(currentPlayer (getcurrentplayer roundElem) )

						;prompt user to select an action
						(selectedAction (selectaction currentPlayer) )

						(gameTrains (gettrains roundElem) )

						;establish the current player's hand
						(playerHand (getcurrentplayerhand playerHands currentPlayer) )

						(engineVal (first (getroundengine roundElem)) )

						;establish the number of doubles placed
						(doublesPlaced (getnumdoublesplayed turnIndicators) )

						;the possible options of the menu
						(saveGameSelected 1)
						(makeMoveSelected 2)
						(getHelpSelected 3)
						(quitSelected 4)
																						)

					(cond 

						;if player wants to save the game
						( (= selectedAction saveGameSelected)

							(let*

								(
									;establish the full game
									(fullGame (list gameElem roundElem) )

									;indicate that the round will be incomplete
									(newIndicators (setincompleteroundmarker turnIndicators t) )

									;append the turn indicators to the round elements 
									(returnList (list roundElem newIndicators ) )
																									)

								;save the game to a file
								(savegame fullGame)

								;round is unchanged
								returnList
							)
						)

						;if player wants to quit the game
						( (= selectedAction quitSelected)

							(let*

								(
									;indicate that the round will be incomplete
									(newIndicators (setincompleteroundmarker turnIndicators t) )

									;append the turn indicators to the round elements 
									(returnList (list roundElem newIndicators ) )
																									)
								(terpri)
								(princ "YOU HAVE SUCCESSFULLY QUIT THE GAME!")
								(terpri)

								;round is unchanged
								returnList
							)
						)

						;if player wants to get help from computer
						( (= selectedAction getHelpSelected)

							(cond

								;if the human has at least one possible move to make
								( (canplayermove gameTrains playerHand engineVal doublesPlaced currentPlayer) 

									(let*

										(
											;provide the tile hint
											(selectedTile (compselectstile playerHand gameTrains engineVal currentPlayer doublesPlaced) )
																																			)

										;provide the train hint
										(compselectstrain gameTrains currentPlayer engineVal selectedTile)
									)
								)

								;if the human has no possible moves
								(t
									(princ "COMPUTER ADVICE: ")
									(princ "THERE ARE NO PLAYABLE TILES IN YOUR HAND")
									(terpri)
								)
							)

							;player goes again (to try playing with help given)
							;round and indicators are unchanged
							(playermakesmove gameElem roundElem turnIndicators)
						)

						;if player wants to make a move
						( (= selectedAction makeMoveSelected)

							(cond

								;if player cannot move
								( (not (canplayermove gameTrains playerHand engineVal doublesPlaced currentPlayer) )

									(let*

										(
											;establish the boneyard indicator
											(drewFromBoneyard (getboneyardstatus turnIndicators) )
																									)

										(cond 

											;if player has not yet picked from boneyard
											( (not drewFromBoneyard)

												(let*

													(
														;establish the boneyard
														(boneyard (getboneyard roundElem) )
																							)

													(cond 

														;if boneyard is not empty
														( (not (= (length boneyard) 0) )

															;pick a tile from boneyard
															(let*

																(
																	;note that the player has drawn from boneyard (update boneyard indicator)
																	(newIndicators (setboneyardmarker turnIndicators t) )

																	;get the tile on top of the boneyard
																	(tile (getboneyardtop boneyard) )

																	;remove the tile from boneyard
																	(roundUpdatedB (removefromboneyard roundElem) )

																	;add the tile to player's hand
																	(newPlayerHand (addtoend playerHand tile) )

																	;update the current player's hand
																	(newHands (updatecurrentplayerhand playerHands newPlayerHand currentPlayer))

																	;update the player hands
																	(roundUpdatedH (updateplayerhands roundUpdatedB newHands) )

																																					)

																(terpri)
																	
																(cond 

																	;if computer picked from boneyard
																	(currentPlayer
																			
																		(princ "COMPUTER PICKED FROM BONEYARD")
			 
																	)

																	;if human picked from boneyard
																	(t
																		(princ "HUMAN PICKED FROM BONEYARD")
																	)
																)
																	
																(terpri)
																	

																;player goes again (to try playing tile drawn from boneyard)
																(playermakesmove gameElem roundUpdatedH newIndicators)
															)
														)


														;if boneyard is empty
														(t
															(let*

																(
																	;note that player skipped turn because of an empty boneyard
																	(newIndicators (setcurrentplayerskipindicator turnIndicators t currentPlayer) )

																	;append the turn indicators to the round elements 
																	(returnList (list roundElem newIndicators ) )

																																						)

																(terpri)
																	
																(cond 

																	;if computer skipped turn
																	( currentPlayer
																			
																		(princ "BONEYARD IS EMPTY. COMPUTER SKIPS TURN!")
			
																	)

																	;if human skipped turn
																	(t
																		(princ "BONEYARD IS EMPTY. HUMAN SKIPS TURN!")
																	)
																)
																	
																(terpri)
																

																;round is unchanged 
																returnList
															)
														)
													)
												)
											)

											;if player has already picked from boneyard and still can't play
											(t
												(let*

													(
														;mark the player's train

														;get the train number
														(trainNum (getcurrentplayertrainnum currentPlayer) )

														;get a copy of the train
														(personalTrain (gettrainbyindex gameTrains trainNum) )

														;mark the train
														(markedTrain (modifyregularmarker personalTrain t) )

														;update the game train
														(newTrains (updatetrainbyindex gameTrains markedTrain trainNum) )

														;update the round
														(updatedRound (updatetrains roundElem newTrains) )

														;append the turn indicators to the round elements 
														(returnList (list updatedRound turnIndicators ) )
																																
																															)
													(terpri)
													(princ "TILE DRAWN FROM BONEYARD CANNOT BE PLAYED")
													(terpri)

													(cond 

														;if computer train is marked
														( currentPlayer
																			
															(princ "COMPUTER TRAIN IS MARKED")
														)

														;if human train is marked
														(t
															(princ "HUMAN TRAIN IS MARKED")		
														)
													)

													(terpri)

													;the player's train is now marked
													returnList
												)
											)
										)
									)
								)

								;if player can move
								(t

									(let*

										(
											;establish the number of doubles played
											(doublesPlaced (getnumdoublesplayed turnIndicators) )

											;player will select a tile
											(selectedTile (playerselectstile playerHand gameTrains engineVal currentPlayer doublesPlaced) )

											;player will select a train
											(selectedTrain (playerselectstrain gameTrains currentPlayer engineVal selectedTile) )

											;a copy of the selected train
											(selectedTrainCopy (gettrainbyindex gameTrains selectedTrain) )

											;check if tile matches the train
											(matched (doestilematchvalue selectedTile (gettrainendval selectedTrainCopy engineVal) ) )

																																			)

										(cond

											;if the selected tile matches the seleced train
											(matched

												(let*

													(
														;remove the tile from player hand
														(newHand (removetile playerHand selectedTile) )

														;update the player hands with the new hand
														(newPlayerHands (updatecurrentplayerhand playerHands newHand currentPlayer) )

														;update the round with the new hands
														(roundHandUpdated (updateplayerhands roundElem newPlayerHands) )

														;add the tile to the train 
														(newTrain (appendtiletotrain selectedTrainCopy selectedTile engineVal) )

														;update the trains with the new train
														(newGameTrains (updatetrainbyindex gameTrains newTrain selectedTrain) )

														;update the round with the new trains
														(updatedRoundTrain (updatetrains roundHandUpdated newGameTrains) )

														;unmark personal train (if applicable)
														(updatedRoundM (unmarkcurrentplayertrain updatedRoundTrain newTrain selectedTrain) )

														;get the updated trains
														(orphanTrains (gettrains updatedRoundM) )

														;remove orphan double marker for this train
														(trainsWithoutOrphans (removeorphans orphanTrains) )

														;update the round with the new trains
														(orphanLessRound (updatetrains updatedRoundM trainsWithoutOrphans ) )

																																				)
													(cond

														;if player placed a double
														( (isdouble selectedTile)

															(let*

																(
																	;note that this player has placed a double

																	;update the number of doubles played
																	(newIndicators (setdoublesplayed turnIndicators (+ doublesPlaced 1) ) )

																	;reset player's boneyard marker
																	(newIndicatorsB (setboneyardmarker newIndicators NIL) )

																																			)

																;player goes again since a double is played
																(playermakesmove gameElem orphanLessRound newIndicatorsB)
															)
														)

														;if player placed a non-double 
														(t
															(let*

																(
																	;append the turn indicators to the round elements 
																	(returnList (list orphanLessRound turnIndicators) )

																															)
																returnList
															)
														)

													) 
												)
											)

											;if selected tile does not match selected train
											(t
												(terpri)
												(princ "SELECTED TILE DOES NOT MATCH THE SELECTED TRAIN")
												(terpri)
												(princ "PLEASE TRY AGAIN!")
												(terpri)

												;player must select again
												(playermakesmove gameElem roundElem turnIndicators)
											)
										)
									)
								)
							)
						)
					)
				)
			)

			;if the round is completed
			(t

				(let*
					(
						(roundCompleted t)

						;update the turn indicators
						(newTurnIndicators (setroundcompletionmarker turnIndicators roundCompleted) )

						;append the turn indicators to the round elements 
						(returnList (list roundElem newTurnIndicators ) )
																										)

						;noting that the round is completed
						returnList
				)
			)
		)
	)
)

#|
/* ********************************************************************* 
Function Name: isroundcompleted
Purpose: To check whether the round is over
		 i.e. a player has exhausted their hand 
		 or both players have skipped turns because boneyard is empty

Parameters: 
			compHandSize, an integer that indicates the number of tiles
			in computer hand 
			humanHandSize, an integer that indicates the number of tiles
			in human hand
			compSkips, a boolean that indicates whether the computer has
			skipped a turn because boneyard is empty 
			humanSkips, a boolean that indicates whether the human has
			skipped a turn because boneyard is empty 

Return Value: true if round is over and false otherwise
Algorithm: 
	 (1) 
	 	if there is no more tiles in either player's hand
	 		the round is over
	 	end if 
	 	else 
	 		if both players have skipped turns because boneyard is empty
	 			the round is over
	 		end if 
	 		else 
	 			the round is not over
	 		end else 
	 	end else

Assistance Received: none 
********************************************************************* */
|#

(defun isroundcompleted (compHandSize humanHandSize compSkips humanSkips)

	(cond

		;if there is no more tiles in a player hand
		( (or (= compHandSize 0) (= humanHandSize 0) )

			;the round is completed
			t
		)

		;if both players have tiles in hand
		(t
			(cond 

				;if both players skipped turns because of an empty boneyard 
				( (and compSkips humanSkips)

					;the round is completed 
					t
				)

				(t
					;the round is not completed
					NIL

				)
			)
		)
	)
)

#|
/* ********************************************************************* 
Function Name: canplayermove
Purpose: To determine if player has at least one possible move to make
Parameters: 
			trains, a list of lists containing the tiles of each train
			playerHand, a list of the tiles in the current player hand 
			engineVal, the numeric value of this round's engine
			doublesPlaced, the number of doubles placed by this player
			during this turn
			currentPlayer, a boolean indicating whether the player 
			currently playing is the computer (t) or human (NIL). 

Return Value: true if player has a move and false otheriwse
Algorithm: 
	 (1) identify the eligible trains
	 (2) identify the playable tiles
	 (3) 
	 	if there are no playable tiles
	 		player does not have any moves
	 	end if 
	 	else 
	 		player has at least one possible move to make
	 	end else

Assistance Received: none 
********************************************************************* */
|#

(defun canplayermove (trains playerHand engineVal doublesPlaced currentPlayer)
	
	(let*
		(
			;identify the eligble trains
			(eligblityIndicators (identifyeligibletrains trains currentPlayer) )
			(eligibleTrains (collecteligibletrains trains eligblityIndicators) )

			;identify all the playable tiles
			(tileCandidates (identifytilecandidates eligibleTrains playerHand engineVal doublesPlaced currentPlayer playerHand) )
																																	)

		(cond 

			;if there are no candidate tiles
			( (= (length tileCandidates) 0)

				;player does not have any moves
				NIL
			)
			
			;if there is at least one tile candidate
			(t
				;player has at least one possible move
				t
			)
		)
	)
)


#|
/* ********************************************************************* 
Function Name: endofroundupdate
Purpose: To update player scores and round number at the end of round
Parameters: 
			gameElem, a list consisted of the round number 
			and the player scores (list)
			compHand, a list of tiles in computer hand
			humanHand, a list of tiles in human hand  
			
Return Value: an updated list consisted of the round number 
			  and the player scores (list)
Algorithm: 
	 (1) calulate the pip sum for both hands
	 (2) update the player scores
	 (3) update the round number
	 	 
Assistance Received: none 
********************************************************************* */
|#

(defun endofroundupdate (gameElem compHand humanHand)

	(let*

		(
			;calculate pip sum for both hands
			(compScore (getpilepipsum compHand) )
			(humanScore (getpilepipsum humanHand) )

			;update the player scores 
			(playerScores (getplayerscores gameElem) )
			(scoresWithComp (updatecompscore playerScores compScore) )
			(scoreswithHuman (updatehumanscore scoresWithComp humanScore ) )
			(gameWithScores (updateplayerscores gameElem scoreswithHuman) )

			;update the round number
			(gameWithRoundNum (updateroundnumber gameWithScores) )
																			)
			gameWithRoundNum
	)
)



#| ************************************************
Source Code to access and update elements of a game
************************************************ |#


#|
/* ********************************************************************* 
Function Name: getroundnumber 
Purpose: To the get the current round number
Parameters: 
			gameElem, a list consisted of the round number 
			and the player scores (list)

Return Value: an integer indicating the current round number	 	 
Assistance Received: none 
********************************************************************* */
|#

(defun getroundnumber (gameElem)
	
	(first gameElem)
)


#|
/* ********************************************************************* 
Function Name: getplayerscores
Purpose: To get the score of players
Parameters: 
			gameElem, a list consisted of the round number 
			and the player scores (list)

Return Value: the list of player scores 	 	 
Assistance Received: none 
********************************************************************* */
|#

(defun getplayerscores (gameElem)

	(first (rest gameElem) )
)


#|
/* ********************************************************************* 
Function Name: getcompscore
Purpose: To get the game score of computer player
Parameters: 
			playerScores, a list consisted of the computer score and 
			the human score

Return Value: an integer indicating the computer score 	 	 
Assistance Received: none 
********************************************************************* */
|#

(defun getcompscore (playerScores)

	(first playerScores)
)


#|
/* ********************************************************************* 
Function Name: gethumanscore
Purpose: To get the game score of human player
Parameters: 
			playerScores, a list consisted of the computer score and 
			the human score

Return Value: an integer indicating the human score 	 	 
Assistance Received: none 
********************************************************************* */
|#

(defun gethumanscore (playerScores)

	(first (rest playerScores) )
)


#|
/* ********************************************************************* 
Function Name: updateroundnumber
Purpose: To increment round number by 1 for next round
Parameters: 
			gameElem, a list consisted of the round number 
			and the player scores (list)

Return Value: the game list with round number updated	 	 
Assistance Received: none 
********************************************************************* */
|#

(defun updateroundnumber (gameElem)

	(list (+ (getroundnumber gameElem) 1) (getplayerscores gameElem) )
)

#|
/* ********************************************************************* 
Function Name: setroundnumber
Purpose: To overwrite the round number
Parameters: 
			gameElem, a list consisted of the round number 
			and the player scores (list)
			newRoundNumber, an integer indicating the new round number

Return Value: the game list with round number updated	 	 
Assistance Received: none 
********************************************************************* */
|#

(defun setroundnumber (gameElem newRoundNumber)

	(list newRoundNumber (getplayerscores gameElem) )
)


#|
/* ********************************************************************* 
Function Name: updateplayerscores
Purpose: To update the player scores
Parameters: 
			gameElem, a list consisted of the round number 
			and the player scores (list)
			newScores, a list of integers holding player scores

Return Value: the game list with player scores updated	 	 
Assistance Received: none 
********************************************************************* */
|#

(defun updateplayerscores (gameElem newScores)

	(list (getroundnumber gameElem) newScores)
)

#|
/* ********************************************************************* 
Function Name: updatecompscore
Purpose: To update the computer score 
Parameters: 
			playerScores, a list consisted of the computer score and 
			the human score
			newCompScore, an integer indicating the new computer score

Return Value: the player scores with computer score updated	 
Assistance Received: none 
********************************************************************* */
|#

(defun updatecompscore (playerScores newCompScore)

	(list (+ newCompScore (getcompscore playerScores) ) (gethumanscore playerScores) )
)

#|
/* ********************************************************************* 
Function Name: updatehumanscore
Purpose: To update the human score 
Parameters: 
			playerScores, a list consisted of the computer score and 
			the human score
			newHumanScore, an integer indicating the new human score

Return Value: the player scores with human score updated	 
Assistance Received: none 
********************************************************************* */
|#

(defun updatehumanscore (playerScores newHumanScore)

	(list (getcompscore playerScores) (+ newHumanScore (gethumanscore playerScores) ) )
)



#| *************************************************************************
Source Code to generate and work with the deck, the engine, and player hands
**************************************************************************|#

#|
/* ********************************************************************* 
Function Name: createdeck
Purpose: To create the game deck consisting of the 55 tiles
Parameters: 
			front, an integer indicating the front value of the 
			the tile being generated for the deck
			back, an integer indicating the back value of the 
			the tile being generated for the deck

Return Value: a list of tiles (55 tiles)	
Algorithm: 
	 (1) start at the tile (0 0)
	 (2)
	 	for each front value 0-9
	 		for each back value 0-9 (excluding the previous front values)
	 			generate a tile
	 		end for 
	 	end for

Assistance Received: none 
********************************************************************* */
|#

(defun createdeck (front back)

	(let
		(
			;the maximum pip value on a tile 
			(maxPip 9)
											)

		(cond

			;if all deck tiles are generated 
			( (> front maxPip)
				() 
			)

			(t 
				(cond
					( (> back (- maxPip 1) )
						(cons
							(list front back)
							(createdeck (+ front 1) (+ front 1)) 
						)
					)

					(t
						(cons
							(list front back)
							(createdeck front (+ back 1) )	
						)
					)
				)	
			)
		)
	)
)

#|
/* ********************************************************************* 
Function Name: identifyengine
Purpose: To find the engine for the current round
Parameters: 
			roundNumber, an integer indicating the current round number

Return Value: an integer representing the engine value for the round	
Assistance Received: none 
********************************************************************* */
|#

(defun identifyengine (roundNumber)

	(cond

		( (> roundNumber 10)
			
			(identifyengine (- roundNumber 10) ) 
		)

		(t 
			(- 10 roundNumber)
		)
	)
)

#|
/* ********************************************************************* 
Function Name: shuffle
Purpose: To shuffle the deck
Parameters: 
			deck, a list consisted of multiple tiles (lists)

Return Value: a list of shuffled tiles 
Algorithm: 
	 (1) generate a random number between 0 and (size of deck - 1)
	 (2) remove the tile from the deck based on index
	     that matches this random number
	 (3) place this tile in the new deck
	 (4) continue this until the old deck is empty

Assistance Received: none 
********************************************************************* */
|#

(defun shuffle (deck)

	(cond

		( (null deck)

			()
		)

		(t 

			(let*
				(
					;set the seed for randomization
					(randomState (make-random-state t) )

					;the index of tile to be picked from deck
					;a random number between 0 and (decksize - 1)
					( randomNumber (random (length deck) randomState) )

					;the tile to be removed from old deck and added to new deck
					( tile (gettile deck randomNumber) )

					;the deck with the tile removed
					( newDeck (removetile deck tile) )
																				)

				(cons
					;add the tile to new deck being returned
					tile

					;grab the rest of the tiles randomly
					(shuffle newDeck)
				)
			)
		)
	)
)

#|
/* ********************************************************************* 
Function Name: distributehand
Purpose: To distribute tiles to players in the beginning of the round
Parameters: 
			deck, a list consisted of multiple tiles (lists)
			numTiles, an integer indicating the number of tiles
			to be distributed to the player hand from the deck

Return Value: a list of tiles (player hand)
Algorithm: 
	 (1) generate a random number between 0 and (size of deck - 1)
	 (2) remove the tile from the deck based on index
	     that matches this random number
	 (3) place this tile in the player hand
	 (4) continue this until 16 tiles are added to player hand

Assistance Received: none 
********************************************************************* */
|#

(defun distributehand (deck numTiles)

	(let*
		(
			;set the seed for randomization
			(randomState (make-random-state t) )
			
			;the index of tile to be picked from deck
			;a random number between 0 and (decksize - 1)
			( randomNumber (random (length deck) randomState))

			;the tile to be removed from deck and added to player hand
			( tile (gettile deck randomNumber) )

			;the deck with the tile removed
			( newDeck (removetile deck tile) )

																		)
			(cond 
				
				;if 16 tiles are added to player hand
				( (= numTiles 0)

					()
				)

				(t
					(cons
						;add the tile to player hand being returned
						tile

						;grab the rest of the tiles randomly
						(distributehand newDeck (- numTiles 1))
					)
				)
			)
	)
)



#| ******************************************************
Source Code to work with a pile (a collection of tiles)
****************************************************** |#


#|
/* ********************************************************************* 
Function Name: gettile
Purpose: To find a tile in a pile based on its index
Parameters: 
			pile, a list consisted of tiles (lists) 
			index, the index of the tile in the pile

Return Value: a tile (a list)
Assistance Received: none 
********************************************************************* */
|#

(defun gettile (pile index)

	(cond

		( (= index 0)
			
			;the first element is the index now
			(first pile)
		)

		(t 
			(gettile (rest pile) (- index 1) )
		)
	)
)

#|
/* ********************************************************************* 
Function Name: getlastelem
Purpose: To get the last element of a pile
Parameters: 
			pile, a list consisted of tiles (lists) 

Return Value: the last tile of the pile (a list)
Assistance Received: none 
********************************************************************* */
|#

(defun getlastelem (pile)

	(cond 

		;if we are at the last element
		( (= (length pile) 1)
			(first pile)
		)

		(t
			(getlastelem (rest pile) )
		)
	)
)

#|
/* ********************************************************************* 
Function Name: getntiles
Purpose: To get the first n tiles of a pile
Parameters: 
			pile, a list consisted of tiles (lists) 
			n, the number of tiles to get from the pile

Return Value: the first n tiles of a pile as a list 
Assistance Received: none 
********************************************************************* */
|#

(defun getntiles (pile n)

	(cond 

		( (= n 0)
			()
		)

		(t
			(cons
				(first pile)
				(getntiles (rest pile) (- n 1) )
			)
		)
	)
)

#|
/* ********************************************************************* 
Function Name: getpilepipsum
Purpose: To calculate the pip sum of all tiles in a pile
Parameters: 
			pile, a list consisted of tiles (lists) 

Return Value: an integer, indicating the pip sum of all tiles in the pile
Assistance Received: none 
********************************************************************* */
|#

(defun getpilepipsum (pile)

	(cond 

		( (null pile)

			0
		)

		(t
			;add the pip sum for this tile to the total
			(+ (gettilesum (first pile) ) (getpilepipsum (rest pile) ) )
		)
	)
)


#|
/* ********************************************************************* 
Function Name: isinpile
Purpose: To check whether a given tile is in a pile
Parameters: 
			pile, a list consisted of tiles (lists) 
			tile, the tile to check if it exists in the pile

Return Value: true if the tile is in the pile and false otherwise
Assistance Received: none 
********************************************************************* */
|#

(defun isinpile (pile tile)

	(cond

		( (null pile)

			NIL
		)

		;if the tile is found
		( (issametile (first pile) tile)

			t
		)

		(t
			(isinpile (rest pile) tile)
		)
	)
)

#|
/* ********************************************************************* 
Function Name: addtoend
Purpose: To add a tile to the end of a pile
Parameters: 
			pile, a list consisted of tiles
			tile, the tile to be added to the pile

Return Value: a list, the new pile with the tile added
Assistance Received: none 
********************************************************************* */
|#

(defun addtoend (pile tile)
	
	(append pile (list tile))
)

#|
/* ********************************************************************* 
Function Name: addtofront
Purpose: To add a tile to the front of a pile
Parameters: 
			pile, a list consisted of tiles
			tile, the tile to be added to the pile

Return Value: a list, the new pile with the tile added
Assistance Received: none 
********************************************************************* */
|#

(defun addtofront (pile tile)

	(append (list tile) pile )
)

#|
/* ********************************************************************* 
Function Name: removetile
Purpose: To remove a tile from a pile
Parameters: 
			pile, a list consisted of tiles
			tile, the tile to be removed from the pile

Return Value: a list, the new pile with the tile removed
Assistance Received: none 
********************************************************************* */
|#

(defun removetile (pile tile)

	(cond
		
		( (null pile)
			()
		)

		;if the tile that must be removed is found
		( (issametile (first pile) tile)

			;skip this tile and do not add it
			(removetile (rest pile) tile)
		)

		(t 
			(cons
				(first pile)
				(removetile (rest pile) tile)
			)
		)
	)
)

#|
/* ********************************************************************* 
Function Name: removelastelem
Purpose: To remove the last element of a pile
Parameters: 
			pile, a list consisted of tiles

Return Value: a list, the new pile with the last tile removed
Assistance Received: none 
********************************************************************* */
|#

(defun removelastelem (pile)

	(cond 

		;if we are at the last element
		( (= (length pile) 1)
			
			()
		)

		(t
			(cons 
				(first pile)
				(removelastelem (rest pile) )
			)
		)
	)
)

#|
/* ********************************************************************* 
Function Name: removetiles
Purpose: To remove a collection of tiles from a bigger pile
Parameters: 
			bigPile, a list consisted of the greater number of tiles
			pile, a list consisted of the fewer number of tiles

Return Value: a list, the bigger pile with the smaller pile removed from it
Assistance Received: none 
********************************************************************* */
|#

(defun removetiles (bigPile pile)
	 
	 (cond

	 	;if all the tiles have been removed from the bigger pile
		( (null pile)
			bigPile
		)

		(t 

			(let*
				(
					;the big pile with a tile removed
					(newPile (removetile bigPile (first pile)) )

																	)
				;remove this tile from the big pile
				(removetiles newPile (rest pile) )
			)

		)
	)
)


#|
/* ********************************************************************* 
Function Name: nondoubleexists
Purpose: To check whether a pile has any non-double tiles in it
Parameters: 
			pile, a list consisted of tiles
			
Return Value: true if pile has any non-doubles and false otherwise
Assistance Received: none 
********************************************************************* */
|#

(defun nondoubleexists (pile)

	(cond 

		( (null pile)

			;there is no non-double in pile
			NIL
		)

		;if there is a non-dobule in pile
		(  (not (isdouble (first pile) ) )

			t
		)


		(t
			(nondoubleexists (rest pile))
		)
	)
)

#|
/* ********************************************************************* 
Function Name: fliptilesinpile
Purpose: To switch the front and back values of all tiles in a pile
Parameters: 
			pile, a list consisted of tiles
			
Return Value: a list, the pile with all tiles flipped
Assistance Received: none 
********************************************************************* */
|#

(defun fliptilesinpile (pile)

	(cond 

		( (null pile)
			()
		)

		(t
			(cons
				(fliptile (first pile) )
				(fliptilesinpile (rest pile) )
			)
		)
	)
)


#|
/* ********************************************************************* 
Function Name: getmaxpipsumtile
Purpose: To identify the tile with the highest pip sum in a pile
Parameters: 
			pile, a list consisted of tiles
			maxTile, the initial highest pip sum of a tile in a pile
			
Return Value: a tile, with the highest pip sum in the pile
Assistance Received: none 
********************************************************************* */
|#

(defun getmaxpipsumtile (pile maxTile)

	(cond

		( (null pile)

			maxTile
		)

		;if this tile sum is higher than the max tile sum
		( (> (gettilesum (first pile)) (gettilesum maxTile) )

			;this tile is the tile with the highest pip sum now
			(getmaxpipsumtile (rest pile) (first pile) )
		)

		;if this tile sum is not higher than the max tile sum
		(t
			(getmaxpipsumtile (rest pile) maxTile)
		)
	)
)

#|
/* ********************************************************************* 
Function Name: getdoublesinpile
Purpose: To get all the double tiles in a pile
Parameters: 
			pile, a list consisted of tiles
			
Return Value: a list, consisting of double tiles in the pile
Assistance Received: none 
********************************************************************* */
|#

(defun getdoublesinpile (pile)

	(cond 

		( (null pile)

			()
		)

		;if a double is identified
		( (isdouble (first pile) )

			(cons
				(first pile)
				(getdoublesinpile (rest pile) )
			)
		)

		(t
			(getdoublesinpile (rest pile) )
		)
	)
)



#| *********************************************
Source Code to identify player starting the round
********************************************* |#

#|
/* ********************************************************************* 
Function Name: identifyfirstplayer
Purpose: To identify the player starting the round
Parameters: 
			compScore, an integer indicating the current computer score 
			humanScore, an integer indicating the current human score 
			
Return Value: true if computer plays first and false if human plays first
Algorithm: 
	 (1)
	 	compare the player scores
	 	if computer score is lower
	 		computer is the first player
	 	end if

	 	else if human score is lower
	 		human is the first player
	 	end else if

	 	else
	 		coin toss will determine the first player
	 	end else

Assistance Received: none 
********************************************************************* */
|#

(defun identifyfirstplayer (compScore humanScore)

	(let*
		
		(
			;comparison outcomes
			(compLower 0)
			(humanLower 1)

			;the result of score comparison 
			(compResult (compareScores compScore humanScore) ) 
																)
		(terpri)
		(cond
	
			( (= compResult compLower)
				
				;computer plays first
				(princ "COMPUTER SCORE IS LOWER THAN YOUR SCORE...COMPUTER WILL START THE ROUND!")
				(terpri)
				t
			)

			( (= compResult humanLower)
				
				;human plays first
				(princ "YOUR SCORE IS LOWER THAN COMPUTER SCORE...YOU WILL START THE ROUND!")
				(terpri)
				NIL
			)

			;if scores are equal
			(t
				;coin toss will decide which player starts
				(princ "THE GAME IS TIED. COIN IS TOSSED TO DETERMINE THE PLAYER THAT STARTS THE ROUND")
				(terpri)
				(performcointoss)
			)
		)
	)
)

#|
/* ********************************************************************* 
Function Name: compareScores
Purpose: To compare the player scores
Parameters: 
			compScore, an integer indicating the current computer score 
			humanScore, an integer indicating the current human score 
			
Return Value: an integer,
			0 --> computer has lower score
			1 --> human has lower score
            2 --> scores are equal
Assistance Received: none 
********************************************************************* */
|#

(defun compareScores (compScore humanScore)

	(let*
		
		(
			;comparison outcomes
			(compLower 0)
			(humanLower 1)
			(tie 2)
								)

		(cond
	
			( (< compScore humanScore)

				compLower
			)

			( (> compScore humanScore)
				
				humanLower
			)

			;if scores are equal
			(t
				tie
			)
		)
	)
)

#|
/* ********************************************************************* 
Function Name: performcointoss
Purpose: To simulate a real life coin toss
Parameters: None
Return Value: true if human gussed wrong and false otherwise
Algorithm: 
	 (1) generate a random number between 0 and 1
	 (2) ask user to select between 0 and 1
	 (3) 
	 	if human selection matches the random number
	 		human guessed right
	 	end if
	 	else 
	 		human guessed wrong
	 	end else

Assistance Received: none 
********************************************************************* */
|#

(defun performcointoss ()

	(princ "HEADS(0) OR TAILS (1)?")
	(terpri)

	(let*

		(
			;set the seed for randomization
			(randomState (make-random-state t) )

			;ask user to pick between 0 and 1
			(humanCall (read) )

			;generate a random number between 0 and 1
			(tossOutcome (random 2 randomState) )
														)
		(terpri)

		(cond

			;if user selection is a number
			( (numberp humanCall)

				(cond 

					;if user selected either heads or tails 
					( (or (= humanCall 0) (= humanCall 1) )

						(cond

							;if human made the right call
							( (= humanCall tossOutcome)

								(princ "YOU WON THE COIN TOSS")
								(terpri)
								(princ "YOU WILL START THE ROUND!")
								(printnewlines 2)
								
								;human will start the game
								NIL
							)

							;if human made the wrong call
							(t
								(princ "YOU LOST THE COIN TOSS")
								(terpri)
								(princ "COMPUTER WILL START THE ROUND!")
								(printnewlines 2)

								;computer will start the game
								t
							)
						)
					)

					;if user entered an invalid number
					(t
						;user must select again
						(performcointoss)
					)
				)
			)

			;if user selection is not a number
			(t
				(performcointoss)
			)
		)
	)
)



#| *********************************************
Source Code to display the game on the screen
********************************************* |#

#|
/* ********************************************************************* 
Function Name: displaygame
Purpose: To display the current game status on the screen
Parameters: 
			gameElem, a list consisted of the round number 
			and the player scores (list)
			roundElem, a list of lists consisted of the elements of a round 
			(player hands, boneyard, engine, game trains, current player)

Return Value: None
Assistance Received: none 
********************************************************************* */
|#

(defun displaygame (gameElem roundElem)

	(let*

		(
			;the game informtion 
			(roundNumber (getroundnumber gameElem) )
			(playerScores (getplayerscores gameElem) )
			(compScore  (getcompscore playerScores) )
			(humanScore (gethumanscore playerScores) )
			
			;the round information
			(playerHands (getplayerhands roundElem) )
			(compHand (getcomphand playerHands) )
			(humanHand (gethumanhand playerHands) )
			(boneyard (getboneyard roundElem) )
			(boneyardSize (length boneyard) )
			(topBoneyard (getboneyardtop boneyard) )
			(roundEngine (getroundengine roundElem) )
			(gameTrains (gettrains roundElem) )
			(compTrain (getcomptrain gameTrains) )
			(humanTrain (gethumantrain gameTrains) )
			(mexicanTrain (rest (getmexicantrain gameTrains) ) )
																				)

		
		(displayround roundNumber)
		(displayscores compScore humanScore)
		(displayplayerhands compHand humanHand)
		(displaytopboneyard topBoneyard boneyardSize)
		(displayregulartrain compTrain humanTrain roundEngine )
		(displaymexicantrain mexicanTrain)
	)
)

#|
/* ********************************************************************* 
Function Name: displayround
Purpose: To display the current round number
Parameters: 
			roundNumber, an integer indicating the current round number

Return Value: None
Assistance Received: none 
********************************************************************* */
|#

(defun displayround (roundNumber)

	(printnewlines 3)
	(printchar #\space 65)
	
	(princ "ROUND:")
	(princ roundNumber)
	
	(terpri)
	(printchar #\space 65)
	(printchar '-' 7)
	(printnewlines 2)
)

#|
/* ********************************************************************* 
Function Name: displayscores
Purpose: To display the current score of the players
Parameters: 
			compScore, an integer indicating the current computer score 
			humanScore, an integer indicating the current human score 

Return Value: None
Assistance Received: none 
********************************************************************* */
|#

(defun displayscores (compScore humanScore)

	;add the top of the box
	(princ #\space)
	(printchar '_' 65)
	(printchar #\space 6)
	(printchar '_' 65)

	(terpri)

	;display the computer score 
	(princ "|")
	(princ #\space)
	(printscore "COMPUTER" compScore 48)
	(princ #\space)
	(princ "|")

	(printchar #\space 4)

	;display the human score
	(princ "|")
	(princ #\space)
	(printscore "HUMAN" humanScore 51)
	(princ #\space)
	(princ "|")

	(terpri)
)

#|
/* ********************************************************************* 
Function Name: printscore
Purpose: To display the score of a player
Parameters: 
			playerName, a string holding the name of the player whose
			score is being printed
			playerScore, an integer indicating the player score
			maxSpace, the maximum number of spaces between player
			name and their score

Return Value: None
Assistance Received: none 
********************************************************************* */
|#

(defun printscore (playerName playerScore maxSpace)
	
	(princ playerName)

	(let*

		(
			( digitScore  (finddigits playerScore) )
			( spaces (- maxSpace digitScore)  )
													)
			
			(printchar #\space spaces)
			(princ "SCORE: ")
			(princ playerScore)
	)
)

#|
/* ********************************************************************* 
Function Name: displayplayerhands
Purpose: To display the tiles in player hands
Parameters: 
			compHand, the tiles in computer hand
			humanHand, the tiles in human hand
			
Return Value: None
Algorithm: 
	 (1)
	 	if both players have 16 or less tiles in hand
	 		display all tiles in computer and human hand
	 	end if 
	 	else 
	 		if computer has more than 16 tiles and human has 16 or less
	 			break the computer hand into 2 piles 
	 			display the first 16 tiles in computer and human hand
	 			display the remaining tiles in computer hand
	 		end if 
	 		else if human has more than 16 tiles and computer has 16 or less
	 			break the human hand into 2 piles 
	 			display the first 16 tiles in computer and human hand
	 			display the remaining tiles in human hand
	 		end if 
	 		else 
	 			break the computer hand into 2 piles 
	 			break the human hand into 2 piles
	 			display the first 16 tiles in computer and human hand
	 			display the remaining tiles in compurer and human hand
	 		end else
	 	end else

Assistance Received: none 
********************************************************************* */
|#

(defun displayplayerhands (compHand humanHand)

	
	(let*
		(
			;the initial number of tiles in each player's hand
			(numTiles 16)

			;the number of tiles in computer hand 
			(compSize (length compHand) )

			;the number of tiles in human hand
			(humanSize (length humanHand) )

																)
		(cond 

			;if both players have 16 or less tiles in hand
			( (and (>= numTiles compSize) (>= numTiles humanSize) )

				;display the first 3 rows for computer and human
				(displaythreerows compHand humanHand)

				;close the box for both player hands
				(princ #\space)
				(printchar "-" 65)
				(printchar #\space 6)
				(printchar "-" 65)
			)

			;if at least one player has more than 16 tiles 
			(t
				(cond 

					;if computer has more than 16 tiles and human has 16 or less
					( (and (< numTiles compSize) (>= numTiles humanSize) )

						(let*
							(
								;break computer hand into 2 piles
								(firstComp (getntiles compHand numTiles) )
								(secondComp (removetiles compHand firstComp) )

																					)

								;display the first 3 rows for computer and human
								(displaythreerows firstComp humanHand)

								;display the first row of computer (t --> outer)
								(displayhandrow secondComp t)

								;close the human box
								(printchar #\space 5)
								(printchar "-" 65)

								(terpri)

								;display the second row of computer (NIL --> inner)
								(displayhandrow secondComp NIL)

								(terpri)

								;display the third row of computer (t --> outer)
								(displayhandrow secondComp t)

								(terpri)

								;close the computer box
								(princ #\space)
								(printchar "-" 65)
						)
					)

					;if human has more than 16 tiles and computer has 16 or less
					( (and (< numTiles humanSize) (>= numTiles compSize) )

						(let*
							(
								;break human hand into 2 piles
								( firstHuman (getntiles humanHand numTiles) )
								( secondHuman (removetiles humanHand firstHuman) )

																					)

								;display the first 3 rows for computer and human
								(displaythreerows compHand firstHuman)

								;close the computer box
								(princ #\space)
								(printchar "-" 65)

								(printchar #\space 5)

								;display first row of human (t --> outer)
								(displayhandrow secondHuman t)

								(terpri)
								
								(printchar #\space 71)
								
								;display second row of human (NIL --> inner)
								(displayhandrow secondHuman NIL)

								(terpri)

								(printchar #\space 71)

								;display third row of human (t --> outer)
								(displayhandrow secondHuman t)

								(terpri)

								(printchar #\space 72)

								(printchar "-" 65)
						)
					)

					;if both players have more than 16 tiles
					(t

						(let*
							(
								;break computer hand into 2 piles
								(firstComp (getntiles compHand numTiles) )
								(secondComp (removetiles compHand firstComp) )

								;break human hand into 2 piles
								(firstHuman (getntiles humanHand numTiles) )
								(secondHuman (removetiles humanHand firstHuman) )

																					)

							;display the first 3 rows for computer and human (part 1)
							(displaythreerows firstComp firstHuman)

							;display the first 3 rows for computer and human (part 2)
							(displaythreerows secondComp secondHuman)

							;close the box for both player hands
							(princ #\space)
							(printchar "-" 65)
							(printchar #\space 6)
							(printchar "-" 65)
						)
					)
				)
			)
		)
	)
)

#|
/* ********************************************************************* 
Function Name: displaythreerows
Purpose: To display 3 rows of player hands (computer and human)
Parameters: 
			compHand, the tiles in computer hand
			humanHand, the tiles in human hand

Return Value: None
Assistance Received: none 
********************************************************************* */
|#

(defun displaythreerows (compHand humanHand)

	;display the first row
	(displayrowhands compHand humanHand t)

	;display the second row 
	(displayrowhands compHand humanHand NIL)

	;display the third row
	(displayrowhands compHand humanHand t)
)

#|
/* ********************************************************************* 
Function Name: displayrowhands
Purpose: To display one row of player hands (computer and human)
Parameters: 
			compHand, the tiles in computer hand
			humanHand, the tiles in human hand
			location, a boolean indicating whether the inner or outer
			part of the hand is displayed

Return Value: None
Assistance Received: none 
********************************************************************* */
|#

(defun displayrowhands (compHand humanHand location)

	;display the computer part of row
	(displayhandrow compHand location)

	;add necessary spaces
	(printchar #\space 4)

	;display the human part of row
	(displayhandrow humanHand location)

	(terpri)
)


#|
/* ********************************************************************* 
Function Name: displayhandrow
Purpose: To display one row of tiles in player hand
Parameters: 
			playerHand, the tiles in the player hand
			location, a boolean indicating whether the inner or outer
			part of the hand is displayed

Return Value: None
Assistance Received: none 
********************************************************************* */
|#

(defun displayhandrow (playerHand location)

	(let*

		(
			;the number of tiles in player's hand
			(handSize (length playerHand) )

			;the maximum number of tiles in one row
			(maxRowTiles 16)

			;the number of spots for empty tiles
			(emptySpots (- maxRowTiles handSize) )

			;the space a tile takes
			(tileSpaceSize 4)

			(endChar "|")
														)

			;print a row of player hand
			(princ endChar)
			(printrowtile playerHand location t)
			(printchar #\space (* emptySpots tileSpaceSize))
			(princ endChar)
	)
)

#|
/* ********************************************************************* 
Function Name: displaytopboneyard
Purpose: To display the top tile on the boneyard
Parameters: 
			topBoneyard, the tile on top of the boneyard
			boneyardSize, the number of tiles in the boneyard

Return Value: None
Algorithm: 
	 (1) 
	 	if boneyard is empty
	 		display nothing
	 	end if 
	 	else 
	 		display the tile on top of the boneyard
	 	end else

Assistance Received: none 
********************************************************************* */
|#

(defun displaytopboneyard (topBoneyard boneyardSize)

	(printnewlines 2)

	;add the top box
	(printchar #\space 65)
	(printchar "_" 8)
	(terpri)

	;print the title
	(printchar #\space 64)
	(princ "|BONEYARD|")
	(terpri)

	;add the first row of tile (t --> outer)
	(printchar #\space 64)
	(princ "|  ")	

	(cond

		;if boneyard is empty
		( (= boneyardSize 0)
			(printchar #\space 3)
		) 

		;if boneyard is not empty
		(t
			(displaytile topBoneyard t)
		)
	)

	(princ "   |")
	(terpri)

	;add the second row of tile (NIL --> inner)
	(printchar #\space 64)
	(princ "|  ")

	(cond

		;if boneyard is empty
		( (= boneyardSize 0)
			(printchar #\space 3)
		) 

		;if boneyard is not empty
		(t
			(displaytile topBoneyard NIL)
		)
	)
	
	(princ "   |")
	(terpri)

	;add the third row of tile (t --> outer)
	(printchar #\space 64)
	(princ "|  ")

	(cond

		;if boneyard is empty
		( (= boneyardSize 0)
			(printchar #\space 3)
		) 

		;if boneyard is not empty
		(t
			(displaytile topBoneyard t)
		)
	)
	
	(princ "   |")
	(terpri)

	;close the box
	(printchar #\space 65)
	(printchar "-" 8)

	(printnewlines 3)
)

#|
/* ********************************************************************* 
Function Name: displaytile
Purpose: To display a row of a tile
Parameters: 
			tile, the tile to be displayed
			location, a boolean indicating whether the inner or outer
			part of the tile is displayed

Return Value: None
Assistance Received: none 
********************************************************************* */
|#

(defun displaytile (tile location)

	#| t --> outer AND NIL --> inner |#
	
	(cond 

		;if outer tile is being printed
		(location

			(cond 

				( (isdouble tile)
					
					(princ #\space)
					(princ (getfrontvalue tile))
					(princ #\space)
				)

				; if the tile is a non-double
				(t
					(printchar #\space 3)
				)
			)
		)

		;if inner tile is being printed
		(t
			(cond 
				
				( (isdouble tile)

					(princ #\space)
					(princ "|")
					(princ #\space)
				)

				; if the tile is a non-double
				(t
					(princ (getfrontvalue tile))
					(princ "-")
					(princ (getbackvalue tile))
				)
			)
		)
	)
)

#|
/* ********************************************************************* 
Function Name: displayregulartrain
Purpose: To display the computer train and human train
Parameters: 
			compTrain, the list of tiles in computer train
			humanTrain, the list of tiles in human train
			roundEngine, the engine tile of the round

Return Value: None
Assistance Received: none 
********************************************************************* */
|#

(defun displayregulartrain (compTrain humanTrain roundEngine)

	(princ " REGULAR TRAIN")
	(printnewlines 2)

	;display the first row of the train (t --> outer)
	(displayregulartrainrow compTrain humanTrain roundEngine t)
	(terpri)

	;display the second row of the train (NIL --> inner)
	(displayregulartrainrow compTrain humanTrain roundEngine NIL)
	(terpri)

	;display the third row of the train (t --> outer)
	(displayregulartrainrow compTrain humanTrain roundEngine t)
	(terpri)
)

#|
/* ********************************************************************* 
Function Name: displayregulartrainrow
Purpose: To display a row of computer train and human train
Parameters: 
			compTrain, the list of tiles in computer train
			humanTrain, the list of tiles in human train
			roundEngine, the engine tile of the round
			location, a boolean indicating whether the inner or outer
			part of the train is displayed

Return Value: None
Assistance Received: none 
********************************************************************* */
|#

(defun displayregulartrainrow (compTrain humanTrain roundEngine location)

	(let*

		(
			;the regular marker for ecah train
			(compMarker (istrainmarked compTrain) )
			(humanMarker (istrainmarked humanTrain) )

			;the train without the indicators
			(compTrainNoM (reverse (rest compTrain) ) )
			(humanTrainNoM (rest humanTrain) )
															)

		;display possible computer marker
		(displaymarker compMarker location)

		;display the row of the computer train (t: outer) (NIL: reversed) 
		(printrowtile compTrainNoM location NIL)

		;display the row of the engine (t --> outer)
		(displaytile roundEngine location)

		;display the row of the human train (t: outer) (t: normal) 
		(printrowtile humanTrainNoM location t)

		;display possible human marker
		(displaymarker humanMarker location)
	)
)

#|
/* ********************************************************************* 
Function Name: displaymexicantrain
Purpose: To display the mexican train
Parameters: 
			mexicanTrain, the list of tiles in mexican train

Return Value: None
Assistance Received: none 
********************************************************************* */
|#

(defun displaymexicantrain (mexicanTrain)

	(printnewlines 3)
	(princ " MEXICAN TRAIN")
	(printnewlines 2)

	;print the first row of the mexican train (t: outer) (t: forward)
	(printrowtile mexicanTrain t t)
	(terpri)

	;print the second row of the mexican train (NIL: inner) (t: forward)
	(printrowtile mexicanTrain NIL t)
	(terpri)

	;print the third row of the mexican train (t: outer) (t: forward)
	(printrowtile mexicanTrain t t)
	(terpri)

	(printnewlines 2)
)


#|
/* ********************************************************************* 
Function Name: displaymarker
Purpose: To display the regular marker of train
Parameters: 
			marked, a boolean indicating whether a train is marked or not
			location, a boolean indicating whether the inner or outer
			part of the marker is displayed

Return Value: None
Assistance Received: none 
********************************************************************* */
|#

(defun displaymarker (marked location)

	(cond 

		;if train is marked
		(marked 

			(cond

				;if outer part of marker is being printed
				( location
					(printchar #\space 3)
				)

				;if inner part of marker is being printed
				(t
					(princ #\space)
					(princ "M")
					(princ #\space)
				)
			)
		)

		;if train is not marked
		(t
			NIL
		)
	)
)

#|
/* ********************************************************************* 
Function Name: printrowtile
Purpose: To print a pile (in row formatting)
Parameters: 
			pile, a list of tiles
			location, a boolean indicating whether the inner or outer
			part of the marker is displayed
			direction, a boolean indicating whether the pile should be
			printed straight or in reverse

Return Value: None
Algorithm:
			(1) 
				if outer row is being printed
					if the tile being processed is a double
						display space front space
					end if 
					else 
						display space space space
					end else 
				end if 

				else 
					if this tile is a double
						display space '|' space
					end if

					else
						if the direction is forward
							display front '-' back
						end if 
						else
							display back '-' front
						end else
					end else
				end else

Assistance Received: none 
********************************************************************* */
|#

(defun printrowtile (pile location direction)

	(princ #\space)

	(cond

		(  (null pile) 
			()
		)
		
		(t 
			(cond 

				;if outer row is being printed
				(location 

					(cond 

						( (isdouble (first pile))

							(princ #\space)

							;display the front end of the tile
							(princ (getfrontvalue (first pile)))
							
							(princ #\space)

							(printrowtile (rest pile) location direction)
						)

						(t
							(printchar #\space 3)
							(printrowtile (rest pile) location direction)
						)
					)
				)

				;if the inner row is being printed
				(t
					(cond 

						( (isdouble (first pile))

							(princ #\space)
							(princ "|")
							(princ #\space)
							(printrowtile (rest pile) location direction)
						)

						;if the tile is a non-double
						(t
							(cond 

								;if the direction is forward
								(direction

									;display the front end of the tile
									(princ (getfrontvalue (first pile) ) )

									(princ "-")

									;display the back value of the tile
									(princ (getbackvalue (first pile) ) )

									(printrowtile (rest pile) location direction)
								)

								;if the direction is backwards
								(t
									;display the end value of the tile
									(princ (getbackvalue (first pile) ) )

									(princ "-")

									;display the front value of the tile
									(princ (getfrontvalue (first pile) ) )

									(printrowtile (rest pile) location direction)
								)
							)
						)
					)
				)
			)
		)
	)
)



#| *************************************************
Source Code to access and update elements of a round 
************************************************* |#


#|
/* ********************************************************************* 
Function Name: getplayerhands
Purpose: To get the player hands
Parameters: 
			roundElem, a list of lists consisted of the elements of a round 
			(player hands, boneyard, engine, game trains, current player)

Return Value: the list of player hands
Assistance Received: none 
********************************************************************* */
|#

(defun getplayerhands (roundElem)

	(first roundElem)
)


#|
/* ********************************************************************* 
Function Name: getcomphand
Purpose: To get the computer hand
Parameters: 
			playerHands, a list consisted of the computer hand and the
			human hand

Return Value: the list of computer hand
Assistance Received: none 
********************************************************************* */
|#

(defun getcomphand (playerHands)

	(first playerHands)
)

#|
/* ********************************************************************* 
Function Name: gethumanhand
Purpose: To get the human hand
Parameters: 
			playerHands, a list consisted of the computer hand and the
			human hand

Return Value: the list of human hand
Assistance Received: none 
********************************************************************* */
|#

(defun gethumanhand (playerHands)

	(first (rest playerHands) )
)

#|
/* ********************************************************************* 
Function Name: getboneyard
Purpose: To get the boneyard
Parameters: 
			roundElem, a list of lists consisted of the elements of a round 
			(player hands, boneyard, engine, game trains, current player)

Return Value: the list of tiles in boneyard
Assistance Received: none 
********************************************************************* */
|#

(defun getboneyard (roundElem)

	(first (rest roundElem) ) 
)


#|
/* ********************************************************************* 
Function Name: getboneyardtop
Purpose: To get the tile on top of the boneyard
Parameters: 
			boneyard, a list of tiles in the boneyard

Return Value: the tile on the top of the boneyard
Assistance Received: none 
********************************************************************* */
|#

(defun getboneyardtop (boneyard)

	(first boneyard)
)


#|
/* ********************************************************************* 
Function Name: getroundengine
Purpose: To get the engine of the round
Parameters: 
			roundElem, a list of lists consisted of the elements of a round 
			(player hands, boneyard, engine, game trains, current player)

Return Value: the engine tile
Assistance Received: none 
********************************************************************* */
|#

(defun getroundengine (roundElem)
	(first (rest (rest roundElem) ) ) 
)


#|
/* ********************************************************************* 
Function Name: gettrains
Purpose: To get all the trains in the round
Parameters: 
			roundElem, a list of lists consisted of the elements of a round 
			(player hands, boneyard, engine, game trains, current player)

Return Value: the list of all game trains
Assistance Received: none 
********************************************************************* */
|#

(defun gettrains (roundElem)

	(first (rest (rest (rest roundElem) ) ) )
)


#|
/* ********************************************************************* 
Function Name: getcomptrain
Purpose: To get the computer train
Parameters: 
			gameTrains, a list consisted of the computer train,  
			human train and mexican train

Return Value: the computer train list
Assistance Received: none 
********************************************************************* */
|#

(defun getcomptrain (gameTrains)

	(first gameTrains)
)


#|
/* ********************************************************************* 
Function Name: gethumantrain
Purpose: To get the human train
Parameters: 
			gameTrains, a list consisted of the computer train, 
			human train and mexican train

Return Value: the human train list
Assistance Received: none 
********************************************************************* */
|#

(defun gethumantrain (gameTrains)

	(first (rest gameTrains ) )
)

#|
/* ********************************************************************* 
Function Name: getmexicantrain
Purpose: To get the mexican train
Parameters: 
			gameTrains, a list consisted of the computer train, 
			human train and mexican train

Return Value: the mexican train list
Assistance Received: none 
********************************************************************* */
|#

(defun getmexicantrain (gameTrains)

	(first (rest (rest gameTrains ) ) ) 
)


#|
/* ********************************************************************* 
Function Name: getcurrentplayer
Purpose: To identify the player who has to take a turn now
Parameters: 
			roundElem, a list of lists consisted of the elements of a round 
			(player hands, boneyard, engine, game trains, current player)

Return Value: true if it's computer turn and false otherwise
Assistance Received: none 
********************************************************************* */
|#

(defun getcurrentplayer (roundElem)

	(first (rest (rest (rest (rest roundElem) ) ) ) )
)


#|
/* ********************************************************************* 
Function Name: updateplayerhands
Purpose: To update the player hands after they play a tile
Parameters: 
			roundElem, a list of lists consisted of the elements of a round 
			(player hands, boneyard, engine, game trains, current player)
			newHands, a list consisted of the new computer hand and the
			new human hand

Return Value: the round list with player hands updated
Assistance Received: none 
********************************************************************* */
|#

(defun updateplayerhands (roundElem newHands)

	(list newHands (getboneyard roundElem) (getroundengine roundElem) (gettrains roundElem) (getcurrentplayer roundElem) )
)

#|
/* ********************************************************************* 
Function Name: updatecomphand
Purpose: To update the computer hand after it plays a tile
Parameters: 
			playerHands, a list consisted of the computer hand and the
			human hand
			newHand, a list consisted of the new computer hand

Return Value: the player hands list with computer hand updated
Assistance Received: none 
********************************************************************* */
|#

(defun updatecomphand (playerHands newHand)
		
	(list newHand (gethumanhand playerHands) ) 
)



#|
/* ********************************************************************* 
Function Name: updatehumanhand
Purpose: To update the human hand after it plays a tile
Parameters: 
			playerHands, a list consisted of the computer hand and the
			human hand
			newHand, a list consisted of the new human hand

Return Value: the player hands list with human hand updated
Assistance Received: none 
********************************************************************* */
|#

(defun updatehumanhand (playerHands newHand)

	(list (getcomphand playerHands) newHand)
)


#|
/* ********************************************************************* 
Function Name: removefromboneyard
Purpose: To remove the top tile of the boneyard
Parameters: 
			roundElem, a list of lists consisted of the elements of a round 
			(player hands, boneyard, engine, game trains, current player)

Return Value: the round list with boneyard updated
Assistance Received: none 
********************************************************************* */
|#
  
(defun removefromboneyard (roundElem)

	(list (getplayerhands roundElem) (rest(getboneyard roundElem)) (getroundengine roundElem) (gettrains roundElem) 
		(getcurrentplayer roundElem) )
)

#|
/* ********************************************************************* 
Function Name: updateboneyard
Purpose: To overwrite the round boneyard
Parameters: 
			roundElem, a list of lists consisted of the elements of a round 
			(player hands, boneyard, engine, game trains, current player)
			newBoneyard, a list of tiles in the new boneyard

Return Value: the round list with boneyard updated
Assistance Received: none 
********************************************************************* */
|#
 
(defun updateboneyard (roundElem newBoneyard)

	(list (getplayerhands roundElem) newBoneyard (getroundengine roundElem) (gettrains roundElem) (getcurrentplayer roundElem) )
)


#|
/* ********************************************************************* 
Function Name: updateroundengine
Purpose: To update the engine of the round
Parameters: 
			roundElem, a list of lists consisted of the elements of a round 
			(player hands, boneyard, engine, game trains, current player)
			newEngine, the new round engine tile

Return Value: the round list with engine round updated
Assistance Received: none 
********************************************************************* */
|#

(defun updateroundengine (roundElem newEngine)
	
	(list (getplayerhands roundElem) (getboneyard roundElem) newEngine (gettrains roundElem) (getcurrentplayer roundElem) )
)


#|
/* ********************************************************************* 
Function Name: updatetrains
Purpose: To update the round trains
Parameters: 
			roundElem, a list of lists consisted of the elements of a round 
			(player hands, boneyard, engine, game trains, current player)
			newTrains, a list consisted of the new computer train,  
			new human train and new mexican train

Return Value: the round list with trains updated
Assistance Received: none 
********************************************************************* */
|#

(defun updatetrains (roundElem newTrains)

	(list (getplayerhands roundElem) (getboneyard roundElem) (getroundengine roundElem) newTrains (getcurrentplayer roundElem) )
)


#|
/* ********************************************************************* 
Function Name: updatecomptrain
Purpose: To update the computer train in the round trains
Parameters: 
			gameTrains, a list consisted of the computer train,  
			human train and mexican train
			newTrain, a list consisted of the new computer train

Return Value: the trains list with computer train updated
Assistance Received: none 
********************************************************************* */
|#

(defun updatecomptrain (gameTrains newTrain)

	(list newTrain (gethumantrain gameTrains) (getmexicantrain gameTrains) ) 													
)


#|
/* ********************************************************************* 
Function Name: updatehumantrain
Purpose: To update the human train in the round trains
Parameters: 
			gameTrains, a list consisted of the computer train,  
			human train and mexican train
			newTrain, a list consisted of the new human train

Return Value: the trains list with human train updated
Assistance Received: none 
********************************************************************* */
|#

(defun updatehumantrain (gameTrains newTrain)

	(list (getcomptrain gameTrains) newTrain (getmexicantrain gameTrains) )  			
)


#|
/* ********************************************************************* 
Function Name: updatemexicantrain
Purpose: To update the mexican train in the round trains
Parameters: 
			gameTrains, a list consisted of the computer train,  
			human train and mexican train
			newTrain, a list consisted of the new mexican train

Return Value: the trains list with mexican train updated
Assistance Received: none 
********************************************************************* */
|#

(defun updatemexicantrain (gameTrains newTrain)

	(list (getcomptrain gameTrains) (gethumantrain gameTrains) newTrain ) 
)


#|
/* ********************************************************************* 
Function Name: updatecurrentplayer
Purpose: To switch the player making a move
Parameters: 
			roundElem, a list of lists consisted of the elements of a round 
			(player hands, boneyard, engine, game trains, current player)

Return Value: the round list with current player updated
Assistance Received: none 
********************************************************************* */
|#

(defun updatecurrentplayer (roundElem)

	(list (getplayerhands roundElem) (getboneyard roundElem) (getroundengine roundElem) (gettrains roundElem) (not (getcurrentplayer roundElem) ) )
)


#|
/* ********************************************************************* 
Function Name: setcurrentplayer
Purpose: To overwrite the player making a move
Parameters: 
			roundElem, a list of lists consisted of the elements of a round 
			(player hands, boneyard, engine, game trains, current player)
			currentPlayer, a boolean indicating whether the player 
			currently playing is the computer (t) or human (NIL). 

Return Value: the round list with current player updated
Assistance Received: none 
********************************************************************* */
|#

(defun setcurrentplayer (roundElem currentPlayer)

	(list (getplayerhands roundElem) (getboneyard roundElem) (getroundengine roundElem) (gettrains roundElem) currentPlayer )
)



#| *************************************************
Source Code to access and update the turn indicators
************************************************* |#

#|
/* ********************************************************************* 
Function Name: getroundstatus
Purpose: To check whether a round is completed
Parameters: 
			turnIndicators, a list of booleans and a number that ensure
			the rules of the game are followed in each turn

Return Value: true if round is completed and false otherwise
Assistance Received: none 
********************************************************************* */
|#

(defun getroundstatus (turnIndicators)

	(first turnIndicators)
)


#|
/* ********************************************************************* 
Function Name: getboneyardstatus
Purpose: To check whether a player has drawn from boneyard
Parameters: 
			turnIndicators, a list of booleans and a number that ensure
			the rules of the game are followed in each turn

Return Value: true if player has drawn from boneyard and false otherwise 
Assistance Received: none 
********************************************************************* */
|#

(defun getboneyardstatus (turnIndicators)

	(first (rest turnIndicators) )
)

#|
/* ********************************************************************* 
Function Name: compskippedturn
Purpose: To check whether computer skipped turn because boneyard is empty
Parameters: 
			turnIndicators, a list of booleans and a number that ensure
			the rules of the game are followed in each turn

Return Value: true if computer skipped and false otherwise
Assistance Received: none 
********************************************************************* */
|#

(defun compskippedturn (turnIndicators)

	(first (rest (rest turnIndicators) ) )
)

#|
/* ********************************************************************* 
Function Name: humanskippedturn
Purpose: To check whether human skipped turn because boneyard is empty
Parameters: 
			turnIndicators, a list of booleans and a number that ensure
			the rules of the game are followed in each turn

Return Value: true if human skipped and false otherwise
Assistance Received: none 
********************************************************************* */
|#

(defun humanskippedturn (turnIndicators)

	(first (rest (rest (rest turnIndicators) ) ) )
)


#|
/* ********************************************************************* 
Function Name: getnumdoublesplayed
Purpose: To get the number of doubles played by a player in a "full" turn
Parameters: 
			turnIndicators, a list of booleans and a number that ensure
			the rules of the game are followed in each turn

Return Value: an integer, the number of doubles played
Assistance Received: none 
********************************************************************* */
|#

(defun getnumdoublesplayed (turnIndicators)

	(first (rest (rest (rest (rest turnIndicators) ) ) ) )
)

#|
/* ********************************************************************* 
Function Name: getincompletestatus
Purpose: To check whether a player has saved or quit the game during the round
Parameters: 
			turnIndicators, a list of booleans and a number that ensure
			the rules of the game are followed in each turn

Return Value: true if player has saved or quit and false otherwise
Assistance Received: none 
********************************************************************* */
|#

(defun getincompletestatus (turnIndicators)

	(first (rest (rest (rest (rest (rest turnIndicators) ) ) ) ) )
)


#|
/* ********************************************************************* 
Function Name: setroundcompletionmarker
Purpose: To indicate that a round is completed
Parameters: 
			turnIndicators, a list of booleans and a number that ensure
			the rules of the game are followed in each turn

Return Value: the updated turnIndicators list
Assistance Received: none 
********************************************************************* */
|#

(defun setroundcompletionmarker (turnIndicators marker)

	(list marker (getboneyardstatus turnIndicators) (compskippedturn turnIndicators) 
		(humanskippedturn turnIndicators) (getnumdoublesplayed turnIndicators) (getincompletestatus turnIndicators) )
)

#|
/* ********************************************************************* 
Function Name: setboneyardmarker
Purpose: To allow/prevent players from picking from boneyard
Parameters: 
			turnIndicators, a list of booleans and a number that ensure
			the rules of the game are followed in each turn
			marker, a boolean to indicate whether player has picked
			from boneyard or not

Return Value: the updated turnIndicators list
Assistance Received: none 
********************************************************************* */
|#

(defun setboneyardmarker (turnIndicators marker)

		(list (getroundstatus turnIndicators) marker (compskippedturn turnIndicators) 
		(humanskippedturn turnIndicators) (getnumdoublesplayed turnIndicators) (getincompletestatus turnIndicators) )
)


#|
/* ********************************************************************* 
Function Name: setcompskipmarker
Purpose: To note that computer has skipped its turn because boneyard is empty
Parameters: 
			turnIndicators, a list of booleans and a number that ensure
			the rules of the game are followed in each turn
			marker, a boolean to indicate whether computer has skipped
			turn because boneyard is empty

Return Value: the updated turnIndicators list
Assistance Received: none 
********************************************************************* */
|#

(defun setcompskipmarker (turnIndicators marker)

	(list (getroundstatus turnIndicators) (getboneyardstatus turnIndicators) marker 
		(humanskippedturn turnIndicators) (getnumdoublesplayed turnIndicators) (getincompletestatus turnIndicators) )
)


#|
/* ********************************************************************* 
Function Name: sethumanskipmarker
Purpose: To note that human has skipped its turn because boneyard is empty
Parameters: 
			turnIndicators, a list of booleans and a number that ensure
			the rules of the game are followed in each turn
			marker, a boolean to indicate whether human has skipped
			turn because boneyard is empty

Return Value: the updated turnIndicators list
Assistance Received: none 
********************************************************************* */
|#

(defun sethumanskipmarker (turnIndicators marker)

	(list (getroundstatus turnIndicators) (getboneyardstatus turnIndicators) (compskippedturn turnIndicators) 
		marker (getnumdoublesplayed turnIndicators) (getincompletestatus turnIndicators) )
)


#|
/* ********************************************************************* 
Function Name: setdoublesplayed
Purpose: To update the number of doubles played by a player
Parameters: 
			turnIndicators, a list of booleans and a number that ensure
			the rules of the game are followed in each turn
			doublesPlayed, the number of doubles placed by player
			during this turn
			

Return Value: the updated turnIndicators list
Assistance Received: none 
********************************************************************* */
|#

(defun setdoublesplayed (turnIndicators doublesPlayed)

	(list (getroundstatus turnIndicators) (getboneyardstatus turnIndicators) (compskippedturn turnIndicators) 
	(humanskippedturn turnIndicators) doublesPlayed (getincompletestatus turnIndicators))
)


#|
/* ********************************************************************* 
Function Name: setincompleteroundmarker
Purpose: To indicate that player has saved the game or quit during the round
Parameters: 
			turnIndicators, a list of booleans and a number that ensure
			the rules of the game are followed in each turn
			marker, a boolean to indicate whether player has saved 
			the game or quit during the round

Return Value: the updated turnIndicators list
Assistance Received: none 
********************************************************************* */
|#

(defun setincompleteroundmarker (turnIndicators marker)

	(list (getroundstatus turnIndicators) (getboneyardstatus turnIndicators) (compskippedturn turnIndicators) 
	(humanskippedturn turnIndicators) (getnumdoublesplayed turnIndicators)  marker)
)



#| *********************************************
Source Code to work with game trains
********************************************* |#

#|
/* ********************************************************************* 
Function Name: gettrainendval
Purpose: To get the tail value of a train
Parameters: 
			train, the list of all tiles in a train
			engineVal, the pip value of the engine tile
			
Return Value: an integer, the tail value of the train
Assistance Received: none 
********************************************************************* */
|#

(defun gettrainendval (train engineVal)

	(let*

		(
			;number of tiles in the train
			(trainSize (length train) )
													)

		(cond 

			;if the size of the train equals 1 (i.e it only has the indicator)
			( (= trainSize 1)

				;the engine is the tail value
				engineVal
			)

			;if there is at least one tile in the train
			(t
				;get the back value of the last tile 
				(getbackvalue (gettile train (- trainSize 1) ) )
			)
		)
	)
)

#|
/* ********************************************************************* 
Function Name: trainendswithdouble
Purpose: To check whether a train ends with a double
Parameters: 
			train, the list of all tiles in a train
			
Return Value: true if a train ends with a double and false otherwise 
Assistance Received: none 
********************************************************************* */
|#

(defun trainendswithdouble (train)

	(cond

		( (null train)

			;train does not end with a double
			NIL
		)

		;if train is non-empty
		(t
			(let*

				(
					;the tail of train
					(lastTile (getlastelem train) )
														)
				(cond 

					( (isdouble lastTile)

						t
					)

					(t
						NIL
					)
				)
			)
		)
	)
)


#|
/* ********************************************************************* 
Function Name: establishorphandoubles
Purpose: To mark the trains that end with a double as orphan double
Parameters: 
			gameTrains, a list consisted of the computer train,  
			human train and mexican train
			
Return Value: the list of modified trains
Assistance Received: none 
********************************************************************* */
|#

(defun establishorphandoubles (gameTrains)

	(cond 

		( (null gameTrains)

			()
		)

		(t
			(cond 

				;if the train ends with a double 
				(  (trainendswithdouble (rest (first gameTrains) ) ) 

					(let*

						(
							;mark the train
							(markedTrain (modifyorphanmarker (first gameTrains) t) )
																						)
						(cons
							markedTrain
							(establishorphandoubles (rest gameTrains) )
						)
					)
				)

				;if train does not end with a double				
				(t
					;do not modify train
					(cons
						(first gameTrains)
						(establishorphandoubles (rest gameTrains) )
					)
				)
			)
		)
	)
)

#|
/* ********************************************************************* 
Function Name: removeorphans
Purpose: To remove orphan double markers after every placement
Parameters: 
			gameTrains, a list consisted of the computer train,  
			human train and mexican train
			
Return Value: the list of modified trains
Assistance Received: none 
********************************************************************* */
|#

(defun removeorphans (gameTrains)

	(cond 

		( (null gameTrains)

			()
		)

		(t
			(cond 

				;if the train does not end with a double
				(  (not (trainendswithdouble (rest (first gameTrains) ) ) )

					(let*

						(
							;unmark the train
							(unmarkedTrain (modifyorphanmarker (first gameTrains) NIL) )
																								)
						(cons
							unmarkedTrain
							(removeorphans (rest gameTrains) )
						)
					)
				)

				;if train ends with a double				
				(t
					;do not modify train
					(cons
						(first gameTrains)
						(removeorphans (rest gameTrains) )
					)
				)
			)
		)
	)
)


#|
/* ********************************************************************* 
Function Name: istrainmarked
Purpose: To check whether a train is marked
Parameters: 
			train, the list of all tiles in a train
			
Return Value: true if train is marked and false otherwise
Assistance Received: none 
********************************************************************* */
|#

(defun istrainmarked (train)

	(first (first train) )
)

#|
/* ********************************************************************* 
Function Name: modifyregularmarker
Purpose: To mark/unmark a train
Parameters: 
			train, the list of all tiles in a train
			marker, a boolean indicating whether the train is marked
			or not
			marker = t   --> mark 
			marker = NIL --> unmark
			
Return Value: the list of marked/unmarked train
Assistance Received: none 
********************************************************************* */
|#

(defun modifyregularmarker (train marker)

	(let*
		(
			;save the orphan double marker
			(orphanMarker (istrainorphan train) )

			;remove the indicator from the train
			(newTrain (rest train) )

			;the new indicator
			(newIndicator (list marker orphanMarker) )

			;add the new indicator to the train
			(markedTrain (addtofront newTrain newIndicator) )

																)

			;return the train that is now marked/unmarked
			markedTrain
	)
)

#|
/* ********************************************************************* 
Function Name: istrainorphan
Purpose: To check whether a train is marked as orphan double
Parameters: 
			train, the list of all tiles in a train
			
Return Value: true if train is marked as orphan double and false otherwise
Assistance Received: none 
********************************************************************* */
|#

(defun istrainorphan (train)

	(first (rest (first train) ) )
)


#|
/* ********************************************************************* 
Function Name: modifyorphanmarker
Purpose: To mark/unmark a train as orphan double
Parameters: 
			train, the list of all tiles in a train
			marker, a boolean indicating whether the train is marked
			as orphan double or not
			marker = t   --> mark 
			marker = NIL --> unmark
			
Return Value: the list of marked/unmarked train
Assistance Received: none 
********************************************************************* */
|#

(defun modifyorphanmarker (train marker)

	(let*
		(
			;save the regular marker
			(regularMarker (istrainmarked train) )

			;remove the indicator from the train
			(newTrain (rest train) )

			;the new indicator
			(newIndicator (list regularMarker marker) )

			;add the new indicator to the train
			(markedTrain (addtofront newTrain newIndicator) )

																)

			;return the train that is now marked/unmarked as orphan double
			markedTrain
	)
)



#| *********************************************
Source Code to access and update trains by index
********************************************* |#

#|
/* ********************************************************************* 
Function Name: gettrainbyindex
Purpose: To get a copy of game trains by index
Parameters: 
			gameTrains, a list consisted of the computer train,  
			human train and mexican train
			index, an integer indicating the train to get a copy of
			
Return Value: a copy of a game train (list)
Assistance Received: none 
********************************************************************* */
|#

(defun gettrainbyindex (gameTrains index)

	(let*
		(
			;the number corresponding to computer train
			(compSelected 1)

			;the number corresponding to human train
			(humanSelected 2)

			;the number corresponding to mexican train
			(MexicanSelected 3)
														)

		(cond 
		
			( (= index compSelected)

				(getcomptrain gameTrains)

			)

			( (= index humanSelected)

				(gethumantrain gameTrains)
			)

			( (= index MexicanSelected)

				(getmexicantrain gameTrains)
			)
		)
	)
)


#|
/* ********************************************************************* 
Function Name: updatetrainbyindex
Purpose: To update a game train by index
Parameters: 
			gameTrains, a list consisted of the computer train,  
			human train and mexican train
			newTrain, the list of all tiles in the new train
			index, an integer indicating the train to get a copy of
			
Return Value: the updated trains of the round 
Assistance Received: none 
********************************************************************* */
|#

(defun updatetrainbyindex (gameTrains newTrain index)
	(let*
		(
			;the number corresponding to computer train
			(compSelected 1)

			;the number corresponding to human train
			(humanSelected 2)

			;the number corresponding to mexican train
			(MexicanSelected 3)
														)

		(cond 
		
			( (= index compSelected)
				(updatecomptrain gameTrains newTrain)
			)

			( (= index humanSelected)
				(updatehumantrain gameTrains newTrain)
			)

			( (= index MexicanSelected)
				(updatemexicantrain gameTrains newTrain)
			)
		)
	)
)



#| *********************************************
Source Code to work with a tile
********************************************* |#


#|
/* ********************************************************************* 
Function Name: getfrontvalue
Purpose: To get the front value of a tile
Parameters: 
			tile, the tile to get information about
			
Return Value: an integer, the front value of the tile
Assistance Received: none 
********************************************************************* */
|#

(defun getfrontvalue (tile)
	(first tile)
)

#|
/* ********************************************************************* 
Function Name: getbackvalue
Purpose: To get the back value of a tile
Parameters: 
			tile, the tile to get information about
			
Return Value: an integer, the back value of the tile
Assistance Received: none 
********************************************************************* */
|#

(defun getbackvalue (tile)
	(first (rest tile ) )
)


#|
/* ********************************************************************* 
Function Name: gettilesum
Purpose: To calculate the pip sum of a tile
Parameters: 
			tile, the tile to get information about
			
Return Value: an integer, the pip sum of the tile
Assistance Received: none 
********************************************************************* */
|#

(defun gettilesum (tile)

	(+ (getfrontvalue tile) (getbackvalue tile) )
)


#|
/* ********************************************************************* 
Function Name: fliptile
Purpose: To switch the front and back value of a tile
Parameters: 
			tile, the tile to get information about
			
Return Value: the flipped version of the tile
Assistance Received: none 
********************************************************************* */
|#

(defun fliptile (tile)

	(let*

		(
			(frontVal (getfrontvalue tile) )

			(backVal (getbackvalue tile) )

			;switch the front and back 
			(flippedTile (list backVal frontVal) )
														)
		flippedTile
	)
)

#|
/* ********************************************************************* 
Function Name: isdouble
Purpose: To check whether a tile is a double
Parameters: 
			tile, the tile to get information about
			
Return Value: true if tile is a double and false otherwise
Assistance Received: none 
********************************************************************* */
|#

(defun isdouble (tile)

	(cond
			
		;if front and back values match
		( (= (getfrontvalue tile) (getbackvalue tile) ) 

			t
		)

		(t 
			nil
		)
	)
)


#|
/* ********************************************************************* 
Function Name: issametile
Purpose: To check whether 2 tiles are the same tiles
Parameters: 
			tile, the tile to get information about
			
Return Value: true if they are the same tiles and false otherwise
Assistance Received: none 
********************************************************************* */
|#

(defun issametile (firstTile secondTile)

	(let*
		(
			;the front and back values of 1st tile
			(firstFront (getfrontvalue firstTile) )
			(firstBack (getbackvalue firstTile) )

			;the front and back values of 2nd tile
			(secondFront (getfrontvalue secondTile) )
			(secondBack (getbackvalue secondTile)  )
														)

		(cond
			
			;case 1 matching tiles
			( (and (= firstFront secondFront) (= firstBack secondBack) )

				t
			)

			;case 2 matching tiles
			( (and (= firstFront secondBack) (= firstBack secondFront) )

				t
			)

			(t 
				nil
			)
		)
	)
)


#| *********************************************
Source Code to allow players to select a tile
********************************************* |#


#|
/* ********************************************************************* 
Function Name: playerselectstile
Purpose: To allow players to select a tile
Parameters: 
			playerHand, the list of tiles in the player hand
			gameTrains, a list consisted of the computer train,  
			human train and mexican train
			engineVal, an integer indicating the pip value of the engine
			currentPlayer, a boolean indicating whether the player 
			currently playing is the computer (t) or human (NIL)
			doublesPlaced, an integer indicating the number of doubles
			placed by the player in this turn			
			
Return Value: the tile selected by the player
Assistance Received: none 
********************************************************************* */
|#

(defun playerselectstile (playerHand gameTrains engineVal currentPlayer doublesPlaced)

	(cond 

		;if the current player is computer
		(currentPlayer

			;computer will select a tile 
			(compselectstile playerHand gameTrains engineVal currentPlayer doublesPlaced)
		)

		;if current player is human
		(t	
			;human will select a tile
			(humanselectstile playerHand gameTrains engineVal currentPlayer doublesPlaced)
		)
	)
)


#|
/* ********************************************************************* 
Function Name: humanselectstile
Purpose: To allow human player to select a tile from their hand
Parameters: 
			humanHand, the list of tiles in human hand
			gameTrains, a list consisted of the computer train,  
			human train and mexican train
			engineVal, an integer indicating the pip value of the engine
			currentPlayer, a boolean indicating whether the player 
			currently playing is the computer (t) or human (NIL)
			doublesPlaced, an integer indicating the number of doubles
			placed by the player in this turn			
			
Return Value: the tile selected by human
Algorithm:
		(1) prompt user to select a tile from hand
		(2) 
			if the selected tile is in the hand
				if the tile is a playable tile
					record the selected tile
				end if
				else 
					player must select again
				end else 
			end if 
			else 
				player must select again
			end else 

Assistance Received: none 
********************************************************************* */
|#

(defun humanselectstile (humanHand gameTrains engineVal currentPlayer doublesPlaced)

	(terpri)
	(princ "SELECT THE TILE YOU WOULD LIKE TO PLACE")
	(terpri)

	(let*

		(

			;the tile selected by the human
			(selectedTile  (read) )
											)

		(cond 

			;if user has entered a list
			( (listp selectedTile)

				(cond 

					;if the list has only 2 elements
					( (= (length selectedTile) 2)

						(cond 

							;if both elements are numbers
							( (and (numberp (first selectedTile) ) (numberp (first (rest selectedTile) ) ) )

								(cond 

									;if the selected tile is in the human hand
									( (isinpile humanHand selectedTile)

										;make sure the tile is an eligible tile
										(let*
											(
												;identify eligible trains 
												(eligibleTrains (identifyeligibletrains gameTrains currentPlayer) )
												(collectedTrains (collecteligibletrains gameTrains eligibleTrains) ) 

												;identify the playable tiles
												(eligibleTiles (identifytilecandidates collectedTrains humanHand engineVal doublesPlaced currentPlayer humanHand) )
																																										)

											(cond

												;if the tile is a playable tile
												( (isinpile eligibleTiles selectedTile)

													;player made a valid selection
													selectedTile
												)

												;if tile is not playable
												(t
													(terpri)
													(princ "INVALID TILE SELECTION. PLEASE TRY AGAIN!")
													(printnewlines 2)

													;player must select again
													(humanselectstile humanHand gameTrains engineVal currentPlayer doublesPlaced)
												)
											)
										)
									)

									;if the tile is not in the humand hand
									(t
										(terpri)
										(princ "INVALID TILE SELECTION. PLEASE TRY AGAIN!")
										(printnewlines 2)

										;human must select again
										(humanselectstile humanHand gameTrains engineVal currentPlayer doublesPlaced)
									)
								)
							) 

							;if an element is not a number
							(t
								(terpri)
								(princ "INVALID TILE SELECTION. PLEASE TRY AGAIN!")
								(printnewlines 2)

								;human must select again
								(humanselectstile humanHand gameTrains engineVal currentPlayer doublesPlaced)
							)
						)
					)
					
					;if the list has a different size
					(t
						(terpri)
						(princ "INVALID TILE SELECTION. PLEASE TRY AGAIN!")
						(printnewlines 2)

						;human must select again
						(humanselectstile humanHand gameTrains engineVal currentPlayer doublesPlaced)
					)
				)
			)

			;if user has entered an invalid form of input
			(t
				(terpri)
				(princ "INVALID TILE SELECTION. PLEASE TRY AGAIN!")
				(printnewlines 2)

				;human must select again
				(humanselectstile humanHand gameTrains engineVal currentPlayer doublesPlaced)
			)
		)
	)
)


#|
/* ********************************************************************* 
Function Name: compselectstile
Purpose: To allow computer to select a tile from its hand
Parameters: 
			playerHand, the list of tiles in the player hand
			gameTrains, a list consisted of the computer train,  
			human train and mexican train
			engineVal, an integer indicating the pip value of the engine
			currentPlayer, a boolean indicating whether the player 
			currently playing is the computer (t) or human (NIL)
			doublesPlaced, an integer indicating the number of doubles
			placed by the player in this turn			
			
Return Value: the tile selected by computer
Algorithm:
		(1) identify the eligible trains
		(2) identify the playable tiles
		(3) identify the playable double tiles (if applicable)
		(4)
			if there is at least one double
				if there is more than one double
					identify all doubles that there is a matching tile for them in hand
					if there is at least one double with a matching tile
						if there is more than one double with matching tile(s)
							identify the max number of matching tiles for the doubles
							identify all doubles with matching tiles = max 
							if there is more than one double with matching tiles = max
								select the double with the highest pip sum
								justify selection
							end if 

							else 
								select the one double with matching tiles = max 
							end else
						end if 

						else 
							select the one double with matching tile(s)
							justify selection
						end else
					end if 
					else 
						select this one double with the highest pip sum
						justify selection
					end else
				end if 
				else 
					select the one playable double in hand
				end else
			end if 
			else 
				if there is more than one non-double
					establish the doubles that have been played in the game
					establish the non-doubles that at least one of their doubles is played
					if there is at least one non-double for which its double(s) is played 
						if there is more than one non-double for which one of its doubles is played
							select the non-double with the highest pip sum
							justify selection
						end if 
						else
							select the one non-double that its double(s) is played
						end else
					end if 
					else 
						select the non-double with the highest pip sum
					end else 
				end if 
				else 
					select the one playable tile
				end else 
			end else

		

Assistance Received: none 
********************************************************************* */
|#

(defun compselectstile (playerHand gameTrains engineVal currentPlayer doublesPlaced)

	(let*

		(
			;the selection reasning codes
			(oneDouble 0)
			(highestDouble 1)
			(oneMatchingDouble 2)
			(mostMatchingDouble 3)
			(mostMatchingDoubleMaxPip 4)
			(oneTile 5)
			(highestTile 6)
			(oneDoublePlayed 7)
			(doublePlayedMaxPip 8)

			(eligibleTrainIndicators (identifyeligibletrains gameTrains currentPlayer) )
			(collectedTrains (collecteligibletrains gameTrains eligibleTrainIndicators) ) 

			;identify all tile candidates
			(tileCands (identifytilecandidates collectedTrains playerHand engineVal doublesPlaced currentPlayer playerHand) )

			;there is at least one candidate because if there were no cadidates...
			;the computer would not be prompted to select a tile

			;identify double candidates
			(doubleCands (getdoublesinpile tileCands) )
																																	)


		(cond

			;if there is at least one double
			( (not (null doubleCands) )

				(cond 

					;if there is more than one double
					( (> (length doubleCands) 1)

						(let*

							(
								;identify all doubles that there is a matching tile for them in hand
								(doublesMatching (getmatchingdoubles playerHand doubleCands) )
																										)

							(cond 

								;if there is at least one double with a matching tile
								( (not (null doublesMatching) )

									(cond

										;if there is more than one double with matching tile(s)
										( (> (length doublesMatching) 1)

											(let*

												(
													(initialMax 0)

													;identify the max number of matching tiles for the doubles
													(maxMatches (identifymostmatchingdoublenum playerHand doublesMatching initialMax) )

													;identify all doubles with matching tiles = max 
													(maxMatchingDoubles (getmostmatchingdoubles playerHand doublesMatching maxMatches) )
																																			)

												(cond

													;if there is more than one double with matching tiles = max
													( (> (length maxMatchingDoubles) 1)

														(let*

															(
																(selectedTile (getmaxpipsumtile maxMatchingDoubles (first maxMatchingDoubles) )   )
																																						)
															
															;select the double with the highest pip sum
															;Code 4: playable double with the most number of matching tiles in hand and highest pip sum
															(printtilereasoning currentPlayer mostMatchingDoubleMaxPip selectedTile)

															selectedTile
														)
													)

													;if there is only one double with matching tiles = max
													(t
														;select this one double with matching tiles = max 
														;Code 3: playable double with the most number of tiles in hand that could match it
														(printtilereasoning currentPlayer mostMatchingDouble (first maxMatchingDoubles) )

														(first maxMatchingDoubles) 
													)
												)
											)
										)

										;if there is only one double with matching tile(s)
										(t
											;select this one double with matching tile(s)
											;Code 2: only playable double that has one or more matching tiles in hand
											(printtilereasoning currentPlayer oneMatchingDouble (first doublesMatching) )

											(first doublesMatching)
										)
									)
								)

								;if there is no double with a matching tile
								(t

									(let*

										(
											(selectedTile (getmaxpipsumtile doubleCands (first doubleCands) )  )
																													)

									;select this one double with the highest pip sum
									;Code 1: playable double with the highest pip sum in hand
									(printtilereasoning currentPlayer highestDouble selectedTile)

									selectedTile

									)
								)
							)
						)
					)

					;if there is only one double
					(t
						;select this one and only playable double in hand
						;Code 0: only playable double in hand
						(printtilereasoning currentPlayer oneDouble (first doubleCands) )
						
						(first doubleCands)
					)
				)
			)

			;if there are no doubles (candiates are all non-doubles)
			(t

				(cond 

					;if there is more than one non-double
					( (> (length tileCands) 1)

						(let*

							(
								;establish the doubles that have been played in the game (including the engine)
								(playedDoubles (identifydoublesplayed gameTrains engineVal) )

								;the non-doubles that at least one of their doubles is played
								(nonDoubles (getnondoublesplayed tileCands playedDoubles) )
																														)

							(cond 

								;if there is at least one non-double for which its double(s) is played 
								( (not (null nonDoubles) )

									(cond 

										;if there is more than one non-double for which one of its doubles is played
										( (> (length nonDoubles) 1)

											(let*

												(
													(selectedTile (getmaxpipsumtile nonDoubles (first nonDoubles) ) )
																															)

												;select the non-double with the highest pip sum
												;Code 8: playable non-double with its double(s) played and highest pip sum
												(printtilereasoning currentPlayer doublePlayedMaxPip selectedTile )

												selectedTile
											)
										)

										;if there is only one non-double for which one of its doubles is played
										(t
											;select this one non-double that its double(s) is played
											;Code 7: only playable non-double with its double(s) played
											(printtilereasoning currentPlayer oneDoublePlayed (first nonDoubles) )

											(first nonDoubles)
										)
									)
								)

								;if there is no non-double for which one of its doubles is played
								(t


									(let*

										(
											(selectedTile (getmaxpipsumtile tileCands (first tileCands) ) )
																												)

										;select the non-double with the highest pip sum
										;Code 6: playable tile with the highest pip sum
										(printtilereasoning currentPlayer highestTile selectedTile)

										selectedTile
									)
								)
							)
						)
					)

					;if there is only one non-double
					(t
						;select this one and only playable tile
						;Code 5: only playable tile
						(printtilereasoning currentPlayer oneTile (first tileCands) )

						(first tileCands)
					)
				)
			)
		)
	)
)


#|
/* ********************************************************************* 
Function Name: getnondoublesplayed
Purpose: To identify the non-doubles that "their doubles" has been played
Parameters: 
			nonDoubleTiles, the list of non-doubles to check whether 
			their double(s) has been played 
			playedDoubles, the list of played doubles so far in the round
			including the engine
			
Return Value: list of non-doubles that "their doubles" has been played
Assistance Received: none 
********************************************************************* */
|#

(defun getnondoublesplayed (nonDoubleTiles playedDoubles)

	(cond 

		( (null nonDoubleTiles)
			()
		)

		;if the double(s) of this non-double is played
		( (doestilematchanydoubles playedDoubles (first nonDoubleTiles) )

			;record this non-double
			(cons
				(first nonDoubleTiles)
				(getnondoublesplayed (rest nonDoubleTiles) playedDoubles)
			)
		)

		(t
			(getnondoublesplayed (rest nonDoubleTiles) playedDoubles)
		)
	)
)

#|
/* ********************************************************************* 
Function Name: doestilematchanydoubles
Purpose: To check whether one of the doubles of a non-double is played
Parameters: 
			playedDoubles, the list of played doubles so far in the 
			round including the engine
			nonDoubleTile, the non-double tile to check whether 
			its double(s) is played 
			
Return Value: true if condition is met and false otherwise
Assistance Received: none 
********************************************************************* */
|#

(defun doestilematchanydoubles (playedDoubles nonDoubleTile)

	(cond 

		( (null playedDoubles)

			NIL
		)

		;if the non-double matches this double
		( (doestilematchvalue nonDoubleTile (first (first playedDoubles) ) )

			;one of the doubles of this non-double is played
			t
		)

		(t
			(doestilematchanydoubles (rest playedDoubles) nonDoubleTile)
		)
	)
)


#|
/* ********************************************************************* 
Function Name: identifydoublesplayed
Purpose: To identify doubles that have been played on the trains
Parameters: 
			gameTrains, a list consisted of the computer train,  
			human train and mexican train
			engineVal, an integer indicating the pip value of the engine
			
Return Value: the list of all played doubles
Assistance Received: none 
********************************************************************* */
|#

(defun identifydoublesplayed (gameTrains engineVal)

	
	(let*

		(
			;establish the game trains (rest --> remove train indicators)
			(compTrain (rest (getcomptrain gameTrains) ) )
			(humanTrain (rest (gethumantrain gameTrains) ) )
			(mexicanTrain (rest (getmexicantrain gameTrains) ) )

			;establish the engine
			(engine (list engineVal engineVal) )

			;identify all doubles placed on the trains 
			(compDoubles (getdoublesinpile compTrain) )
			(humanDoubles (getdoublesinpile humanTrain) )
			(mexicanDoubles (getdoublesinpile mexicanTrain) )
			(placedDoublesTrains (append (append compDoubles humanDoubles) mexicanDoubles) )
			
			;all placed doubles
			(placedDoubles (cons engine placedDoublesTrains) )

																							)
		placedDoubles
	)
)


#|
/* ********************************************************************* 
Function Name: getmostmatchingdoubles
Purpose: To identify all doubles that their number of matching tiles = max
Parameters: 
			playerHand, the list of tiles in player hand 
			doubleTiles, the list of double tiles to select the ones
			with matching tiles = max from 
			maxMatches, the most number of matching tiles for a given
			double
			
Return Value: a list of doubles with matching tiles = max
Assistance Received: none 
********************************************************************* */
|#

(defun getmostmatchingdoubles (playerHand doubleTiles maxMatches)

	(cond 

		( (null doubleTiles)
			()
		)

		;if this double's matching tiles = max
		( (= (getnummatchingtiles playerHand (first doubleTiles) ) maxMatches )

			;record this double
			(cons
				(first doubleTiles)
				(getmostmatchingdoubles playerHand (rest doubleTiles) maxMatches)
			)
		)

		(t
			(getmostmatchingdoubles playerHand (rest doubleTiles) maxMatches)
		)
	)
)


#|
/* ********************************************************************* 
Function Name: identifymostmatchingdoublenum
Purpose: To identify the most number of tiles in hand that match a double
Parameters: 
			playerHand, the list of tiles in player hand 
			doubleTiles, the list of double tiles to find the maximum 
			number of matching tiles from
			mostMatches, the most number of matching tiles of a double
			in the player hand
			
Return Value: an integer, the max number of tiles in hand that match a double
Assistance Received: none 
********************************************************************* */
|#

(defun identifymostmatchingdoublenum (playerHand doubleTiles mostMatches)

	(cond 

		( (null doubleTiles)
			mostMatches
		)

		;if this double has more matching tiles than the maximum matching tiles
		( (> (getnummatchingtiles playerHand (first doubleTiles)) mostMatches )

			;this is the max matching tiles now
			(identifymostmatchingdoublenum playerHand (rest doubleTiles) (getnummatchingtiles playerHand (first doubleTiles)) )
		)

		(t
			(identifymostmatchingdoublenum playerHand (rest doubleTiles) mostMatches)
		)
	)
)


#|
/* ********************************************************************* 
Function Name: getmatchingdoubles
Purpose: To identify all doubles that there is a matching tile for them in hand
Parameters: 
			playerHand, the list of tiles in player hand 
			doubles, the list of double tiles to find the ones with matching 
			tile(s) from
			
Return Value: the list of doubles that have at least one matching tile
Assistance Received: none 
********************************************************************* */
|#

(defun getmatchingdoubles (playerHand doubles)

	(cond 

		( (null doubles)
			()
		)

		;if this double has a matching tile in hand 
		( (> (getnummatchingtiles playerHand (first doubles) ) 0) 

			(cons
				(first doubles)
				(getmatchingdoubles playerHand (rest doubles) )
			)
		)

		(t
			(getmatchingdoubles playerHand (rest doubles) )
		)
	)
)


#|
/* ********************************************************************* 
Function Name: getnummatchingtiles
Purpose: To find the number of tiles in hand that match a given double tile
Parameters: 
			playerHand, the list of tiles in player hand 
			doubleTile, the double to check how many tiles match it
			in the player hand
			
Return Value: an integer, the number of matching tiles
Assistance Received: none 
********************************************************************* */
|#

(defun getnummatchingtiles (playerHand doubleTile)

	(cond 

		( (null playerHand)
			0
		)

		;if this tile matches the double and it's not the double itself
		( (and (doestilematchvalue (first playerHand) (first doubleTile) ) (not (issametile (first playerHand) doubleTile) ) )

			;there is a matching tile 
			(+ 1 (getnummatchingtiles (rest playerHand) doubleTile) )
		)

		(t
			(getnummatchingtiles (rest playerHand) doubleTile)
		)
	)
)


#| *********************************************
Source Code to allow players to select a train
********************************************* |#


#|
/* ********************************************************************* 
Function Name: playerselectstrain
Purpose: To allow players to select a train
Parameters: 
			gameTrains, a list consisted of the computer train,  
			human train and mexican train
			currentPlayer, a boolean indicating whether the player 
			currently playing is the computer (t) or human (NIL)
			engineVal, an integer indicating the pip value of the engine
			selectedTile, the tile selected by the player from hand
			
Return Value: an integer, the selected train number
Assistance Received: none 
********************************************************************* */
|#

(defun playerselectstrain (gameTrains currentPlayer engineVal selectedTile)

	(cond 

		;if the current player is computer
		(currentPlayer

			;computer will select a train
			(compselectstrain gameTrains currentPlayer engineVal selectedTile)
		)

		;if current player is human
		(t
			;human will select a train
			(humanselectstrain gameTrains currentPlayer)
		)
	)
)


#|
/* ********************************************************************* 
Function Name: humanselectstrain
Purpose: To allow human player to select a train to place the tile on
Parameters: 
			gameTrains, a list consisted of the computer train,  
			human train and mexican train
			currentPlayer, a boolean indicating whether the player 
			currently playing is the computer (t) or human (NIL)
			
Return Value: an integer, the selected train number
Algorithm:
		(1) identify the eligible trains
		(2) prompt user to select an eligible train
		(3)
			if user did not select an eligible train
				user must select again
			end if
			else
				record the selected train 
			end else

Assistance Received: none 
********************************************************************* */
|#

(defun humanselectstrain (gameTrains currentPlayer)

	(terpri)
	(princ "SELECT THE TRAIN YOU WOULD LIKE TO PLACE THE TILE ON")
	(terpri)

	(let*

		(
			;the start of the menu
			(startNum 1)

			;eligibleTrains is now a boolean list (t/NIL t/NIL t/NIL)
			(eligibleTrains (identifyeligibletrains gameTrains currentPlayer) )

			;the name of the trains
			(trainNames (list "COMPUTER" "YOUR" "MEXICAN") )

			;display which trains are eligible to the user
			(eligibleNums (displaytraineligbility trainNames eligibleTrains startNum) )

			;the user input
			(selectedTrain  (read) )

			;the option corresponding to computer train
			(compSelected 1)

			;the option corresponding to human train
			(humanSelected 2)

			;the option corresponding to mexican train
			(MexicanSelected 3)
																							)

			(cond

				;if user input is a number
				( (numberp selectedTrain)

					(cond 

						;if user did not select an eligible train
						( (not (isvalinlist eligibleNums selectedTrain) )

							(terpri)
							(princ "INVALID TRAIN SELECTION. PLEASE TRY AGAIN!")
							(terpri)

							;user must select again
							(humanselectstrain gameTrains currentPlayer)
						)

						(t
							;user made a valid selection
							(cond 

								( (= selectedTrain compSelected)
									compSelected
								)


								( (= selectedTrain humanSelected)
									humanSelected
								)

								;if mexican was selected
								(t
									MexicanSelected
								)

							)
						)
					)
				)

				;if user input is not a number
				(t
					(terpri)
					(princ "INVALID TRAIN SELECTION. PLEASE TRY AGAIN!")
					(terpri)

					;user must select again
					(humanselectstrain gameTrains currentPlayer)
				)
			)
	)
)


#|
/* ********************************************************************* 
Function Name: compselectstrain
Purpose: To allow computer to select a train
Parameters: 
			gameTrains, a list consisted of the computer train,  
			human train and mexican train
			currentPlayer, a boolean indicating whether the player 
			currently playing is the computer (t) or human (NIL)
			engineVal, an integer indicating the pip value of the engine
			selectedTile, the tile selected by the player from hand
			
Return Value: an integer, the selected train number
Algorithm:
	(1) identify the eligible matching trains
	(2) 
		if there is only one matching eligible train
			justify selection
			select this train
		end if 

		else 
			if there is 2 eligible matching trains
				
				if personal and mexican trains are eligible matching
					justify selection
					select mexican train
				end if

				else if personal and opponent trains are eligible matching
					justify selection
					select opponent train
				end else if

				else if mexican and opponent trains are eligible matching
					justify selection
					select opponent train
				end else if
			end if 

			else 
				justify selection
				select opponent train
			end else
		end else

Assistance Received: none 
********************************************************************* */
|#

(defun compselectstrain (gameTrains currentPlayer engineVal selectedTile)

	(let*

		(
			;eligibleTrains is now a boolean list (t/NIL t/NIL t/NIL)
			(eligibleIndicators (identifyeligibletrains gameTrains currentPlayer) )

			;identify mathcing eligible trains
			(matchingEligibles (identifymatchingelibletrains gameTrains eligibleIndicators engineVal selectedTile) )

			;number of eligible matching trains
			(numTrains (getnumeligibletrains matchingEligibles) )

			;establish the indicators
			(compEligible (first matchingEligibles) )
			(humanEligible (first (rest matchingEligibles) ) )
			(mexicanEligible (first (rest (rest matchingEligibles) ) ) ) 

			;at least one train is always is eligbile otherwise computer would
			;not be prompted to select a train

			;the reasoning codes
			(oneEligibleTrain 0)
			(opponentUnavail 1)
			(personalMexican 2)

			;selection result
			(compSelected 1)
			(humanSelected 2)
			(MexicanSelected 3)
																															)


		(cond 

			;if there is only one matching eligible train
			( (= numTrains 1)

				;select the only eligible train
				(cond 

					(compEligible

						;Code 0: ONLY ELIGIBLE TRAIN THAT MATCHES THE SELECTED TILE
						(printtrainreasoning currentPlayer oneEligibleTrain compSelected)
						
						compSelected
					)

					(humanEligible

						;Code 0: ONLY ELIGIBLE TRAIN THAT MATCHES THE SELECTED TILE
						(printtrainreasoning currentPlayer oneEligibleTrain humanSelected)

						humanSelected
					)

					(mexicanEligible

						;Code 0: ONLY ELIGIBLE TRAIN THAT MATCHES THE SELECTED TILE
						(printtrainreasoning currentPlayer oneEligibleTrain MexicanSelected)

						MexicanSelected 
					)
				)
			)

			;if there is more than one eligble matching train
			(t

				(let*

					(
						;opponent information
						(opponent (not currentPlayer) )
						(opponentTrainNum (getcurrentplayertrainnum opponent) )

						(personalEligible (getpersonaltraineligiblity compEligible humanEligible currentPlayer) )
						(opponentEligible (getpersonaltraineligiblity compEligible humanEligible opponent ) )
						
																													)
					(cond

						;if there is 2 eligble trains
						( (= numTrains 2)

							(cond 

								;if personal and mexican trains are eligible
								( (and personalEligible mexicanEligible)
									
									;Code 2: MEXICAN IS ALMOST ALWAYS AVAILABLE TO THE OPPONENT
									(printtrainreasoning currentPlayer personalMexican MexicanSelected)

									MexicanSelected
								)

								;if personal and opponent trains are eligible
								( (and personalEligible opponentEligible)

									;Code 1: OPPONENT TRAIN COULD BE UNMARKED AND NOT AVAILABLE IN THE NEXT TURN
									(printtrainreasoning currentPlayer opponentUnavail opponentTrainNum)

									;opponent train would be the best option
									opponentTrainNum

								)

								;if mexican and opponent trains are eligible
								( (and opponentEligible mexicanEligible)

									;Code 1: OPPONENT TRAIN COULD BE UNMARKED AND NOT AVAILABLE IN THE NEXT TURN
									(printtrainreasoning currentPlayer opponentUnavail opponentTrainNum)

									;opponent train would be the best option
									opponentTrainNum
								)
							)
						)

						;if all trains are eligible
						(t
							;Code 1: OPPONENT TRAIN COULD BE UNMARKED AND NOT AVAILABLE IN THE NEXT TURN
							(printtrainreasoning currentPlayer opponentUnavail opponentTrainNum)

							;opponent train would be the best option
							opponentTrainNum
						)
					)
				)
			)
		)
	)	
)

#|
/* ********************************************************************* 
Function Name: identifymatchingelibletrains
Purpose: To check which eligible trains match the selected tile 
Parameters: 
			trains, a list of all trains
			trainIndicators, a list of booleans that indicate whether 
			a train is eligible or not
			engineVal, an integer indicating the pip value of the engine
			selectedTile, the tile selected by the player from hand
			
Return Value: the indicator list of trains with eligible matching trains set
Assistance Received: none 
********************************************************************* */
|#

(defun identifymatchingelibletrains (trains trainIndicators engineVal selectedTile)

	(cond

		( (null trainIndicators)
			()
		) 

		;if this train is eligble and matches the selected tile
		( (and (doestilematchvalue selectedTile (gettrainendval (first trains) engineVal) ) (first trainIndicators) )

			(cons
				t
				(identifymatchingelibletrains (rest trains) (rest trainIndicators) engineVal selectedTile)
			)
		)

		(t
			(cons
				NIL
				(identifymatchingelibletrains (rest trains) (rest trainIndicators) engineVal selectedTile)
			)
		)
	)
)

#|
/* ********************************************************************* 
Function Name: getnumeligibletrains
Purpose: To identify the number of eligble matching trains
Parameters: 
			eligibleIndicators, a list of booleans that indicate whether 
			a train is eligible or not
			
Return Value: the number of eligbile matching trains
Assistance Received: none 
********************************************************************* */
|#

(defun getnumeligibletrains (eligibleIndicators)

	(cond 

		( (null eligibleIndicators)
			0
		)

		;if this train is eligbile
		( (first eligibleIndicators)

			(+ 1 (getnumeligibletrains (rest eligibleIndicators) ) )
		)

		(t
			(getnumeligibletrains (rest eligibleIndicators) )
		)
	)
)



#| *********************************************
Source Code to match tiles to trains
********************************************* |#


#|
/* ********************************************************************* 
Function Name: identifyeligibletrains
Purpose: To check which trains are eligible to play on
Parameters: 
			gameTrains, a list consisted of the computer train,  
			human train and mexican train
			currentPlayer, a boolean indicating whether the player 
			currently playing is the computer (t) or human (NIL)
			
Return Value: a list of booleans to indicate which trains are eligible
Algorithm:
	(1) 
		if any of the trains is orphan double
			these are the only eligible trains
		end if 
		else 
			personal train is eligible 
			mexican train is eligible
			if opponent train is marked
				opponent train is also eligible
			end if 
		end else

Assistance Received: none 
********************************************************************* */
|#

(defun identifyeligibletrains (gameTrains currentPlayer)

	(let*

		(
			;establish the trains
			(compTrain (getcomptrain gameTrains) )
			(humanTrain (gethumantrain gameTrains) )
			(mexicanTrain (getmexicantrain gameTrains) )

			;to check if computer train is orphan double
			(compEligiblity (istrainorphan compTrain) )

			;to check if human train is orphan double
			(humanEligiblity (istrainorphan humanTrain) )

			;to check if mexican train is orphan double
			(mexicanEligiblity (istrainorphan mexicanTrain) )
																)

		(cond 

			;if any of the trains is orphan double
			(  (or (or compEligiblity humanEligiblity) mexicanEligiblity )

				;return the eligible trains because there is at least one orphan double
				(list compEligiblity humanEligiblity mexicanEligiblity)
			)

			;if there are no orphan doubles
			(t

				(cond 

					;if current player is computer 
					(currentPlayer

						(let*

							(
								;computer train is eligible (personal)
								(compEligiblity t)

								;to check whether opponent train is marked
								(humanEligiblity (istrainmarked humanTrain) )

								;mexican train is eligible
								(mexicanEligiblity t)
																				)

							(list compEligiblity humanEligiblity mexicanEligiblity)
						)
					)

					;if current player is human
					(t
						(let*

							(
								;to check whether opponent train is marked
								(compEligiblity (istrainmarked compTrain) )

								;human train is eligible (personal)
								(humanEligiblity t)

								;mexican train is eligible
								(mexicanEligiblity t)
																				)

							(list compEligiblity humanEligiblity mexicanEligiblity)
						)
					)
				)
			)
		)
	)
)


#|
/* ********************************************************************* 
Function Name: collecteligibletrains
Purpose: To get a copy of all the eligible trains
Parameters: 
			trains, a list consisted of the computer train,  
			human train and mexican train
			indicators, a list of booleans that indicate whether 
			a train is eligible or not
			
Return Value: a collection of trains (eligible)
Assistance Received: none 
********************************************************************* */
|#

(defun collecteligibletrains (trains indicators)

	(cond 

		( (null trains)

			()
		)

		;if the eligblity indicator is set
		( (first indicators)
			
			(cons 
				(first trains)
				(collecteligibletrains (rest trains) (rest indicators) )
			)
		)

		;if the eligblity indicator is not set
		(t
			(collecteligibletrains (rest trains) (rest indicators) )
		)
	)
)

#|
/* ********************************************************************* 
Function Name: playingnondoubleexists
Purpose: To check whether a non-double in player hand is playable 
Parameters: 
			playerHand, the tiles in the player hand
			trains, a list consisted of the computer train,  
			human train and mexican train
			currentPlayer, a boolean indicating whether the player 
			currently playing is the computer (t) or human (NIL)
			engineVal, the pip value of the engine
			
Return Value: true if a non-double is playable and false otheriwse
Assistance Received: none 
********************************************************************* */
|#

(defun playingnondoubleexists (playerHand trains currentPlayer engineVal)

	(let*

		(
			;identify the eligble trains
			(eligblityIndicators (identifyeligibletrains trains currentPlayer) )

			;get eligible trains
			(eligibleTrains (collecteligibletrains trains eligblityIndicators) )
																						)
		(cond 

			( (null playerHand)

				;there is no playing non-double in hand 
				NIL
			)

			;if there is a non-double that matches an eligible train
			( (and (not (isdouble (first playerHand) ) ) (doestilematchanytrain eligibleTrains (first playerHand) engineVal) )

				;there is a playing non-double in hand
				t
			)

			(t
				(playingnondoubleexists (rest playerHand) trains currentPlayer engineVal)
			)
		)
	)
)

#|
/* ********************************************************************* 
Function Name: identifytilecandidates
Purpose: To identify all the tiles that are playable in the turn
Parameters: 
			trains, a list consisted of the computer train,  
			human train and mexican train
			playerHand, the tiles in the player hand
			engineVal, the pip value of the engine
			doublesPlaced, the number of doubls placed by the player 
			in this turn
			currentPlayer, a boolean indicating whether the player 
			currently playing is the computer (t) or human (NIL)
			originalHand, a copy of the tiles in the player hand
			used to check for playing non-doubles since the original
			tiles in player hand will be modified inside this function
			
Return Value: all the playable tiles as a list
Algorithm:
	(1)
		if the tile being checked macthes an eligible train
			if 1 double is placed, the tile being checked is a double and there is no playing non-double in hand
				if this is not the last tile in hand
					this tile is not a candidate
				end if 
				else 
					this tile is a candidate
				end else
			end if
			
			else if 2 doubles are placed already and this tile is a double
				this tile is not a candidate
			end else if

			else 
				this tile is a candidate
			end else
		end if 

		else 
			this tile is not a candidate
		end else

Assistance Received: none 
********************************************************************* */
|#

(defun identifytilecandidates (trains playerHand engineVal doublesPlaced currentPlayer originalHand)

	(cond 
		
		( (null playerHand)

			()
		)

		;if the tile being checked matches an eligble train
		( (doestilematchanytrain trains (first playerHand) engineVal)

			(cond 

				;if 1 double is placed, the tile being checked is a double and there is no playing non-double in hand
				( (and (and (= doublesPlaced 1) (isdouble (first playerHand) ) ) (not (playingnondoubleexists originalHand trains currentPlayer engineVal) ) )

					(cond 

						;if this is not the last tile in hand
						( (not (= (length originalHand) 1) )

							;skip this tile because double candidates are only eligible if there is a playing non-double in hand
							;unless it is the last the tile in hand which does not apply here
							(identifytilecandidates trains (rest playerHand) engineVal doublesPlaced currentPlayer originalHand)
						)

						;if this is the last tile in hand
						(t
							;record tile as a candidate
							(cons
								(first playerHand)
								(identifytilecandidates trains (rest playerHand) engineVal doublesPlaced currentPlayer originalHand)
							)
						)
					)
				)

				;if 2 doubles are placed already and this tile is a double
				( (and (= doublesPlaced 2) (isdouble (first playerHand) ) )
				
					;skip this tile because player must now play a non-double
					;all doubles are ineligible
					(identifytilecandidates trains (rest playerHand) engineVal doublesPlaced currentPlayer originalHand)
				)

				;if no doubles are placed yet
				(t
					;record tile as a candidate
					(cons
						(first playerHand)
						(identifytilecandidates trains (rest playerHand) engineVal doublesPlaced currentPlayer originalHand)
					)
				)
			)
		)

		;if the tile does not match any of the trains
		(t
			(identifytilecandidates trains (rest playerHand) engineVal doublesPlaced currentPlayer originalHand)
		)
	)
)


#|
/* ********************************************************************* 
Function Name: doestilematchanytrain
Purpose: To check whether a tile matches the end of any eligible game trains
Parameters: 
			trains, a list consisted of eligible trains
			tile, the tile to check 
			engineVal, the pip value of the engine
			
Return Value: true if the tile matches at least one eligbile train and false otherwise
Assistance Received: none 
********************************************************************* */
|#

(defun doestilematchanytrain (trains tile engineVal)

	(cond

		( (null trains)

			NIL
		)

		(t

			(let*
				(
					;the tail value of the train being checked
					(trainTailVal (gettrainendval (first trains) engineVal) )
																				)

				(cond 

					;if front or back of tile matches the tail of the train
					( (doestilematchvalue tile trainTailVal)

						t
					)

					;if the tile does not match this train
					(t
						;check if it matches the other trains
						(doestilematchanytrain (rest trains) tile engineVal )
					)
				)
			)

		)
	)
)

#|
/* ********************************************************************* 
Function Name: doestilematchvalue
Purpose: To check whether a tile matches a given pip value
Parameters: 
			tile, the tile to check 
			pipVal, the value to check the tile with
			
Return Value: true if the tile matches the value and false otherwise
Assistance Received: none 
********************************************************************* */
|#

(defun doestilematchvalue (tile pipVal)

	(cond

		;if the front or back of tile matches the pip value
		( (or (= (getfrontvalue tile) pipVal) (= (getbackvalue tile) pipVal) )
			
			;the tile matches this pip value
			t
		)

		(t
			;tile does not match this pip value
			NIL
		)
	)
)

#|
/* ********************************************************************* 
Function Name: appendtiletotrain
Purpose: To add a tile to the end of a train
Parameters: 
			train, the list of tiles to add the tile to
			tile, the tile to be added 
			engineVal, the pip value of the engine
			
Return Value: the train list with tile added 
Assistance Received: none 
********************************************************************* */
|#

(defun appendtiletotrain (train tile engineVal)

	(let*
		(
			;the tail value of the train
			(trainTailVal (gettrainendval train engineVal) )
																)
		(cond

			;if the tile is a non-double and the tile's back matches the train
			( (and (not (isdouble tile) ) (= (getbackvalue tile) trainTailVal) )

				(let*

					(
						;flip the tile to match train tail
						(flippedTile (fliptile tile) )
															)
					(addtoend train flippedTile)
				)
			)

			;no flipping required
			(t
				(addtoend train tile)
			)
		)
	)
)



#| ************************************************************************
Source Code to access and update the round elements based on current player 
************************************************************************ |#

#|
/* ********************************************************************* 
Function Name: getcurrentplayerhand 
Purpose: To get the hand of the player who is a taking turn now
Parameters: 
			playerHands, the list of tiles in the player hands
			currentPlayer, a boolean indicating whether the player 
			currently playing is the computer (t) or human (NIL)
			
Return Value: the list of current player hand 
Assistance Received: none 
********************************************************************* */
|#

(defun getcurrentplayerhand (playerHands currentPlayer)

	(cond 

		;if the current player is computer
		(currentPlayer
			(getcomphand playerHands)
		)

		;if current player is human
		(t
			(gethumanhand playerHands)
		)
	)
)


#|
/* ********************************************************************* 
Function Name: updatecurrentplayerhand 
Purpose: To update the hand of the player who is a taking turn now
Parameters: 
			playerHands, the list of tiles in the player hands
			newHand, the list of tiles to update the current player's 
			hand with
			currentPlayer, a boolean indicating whether the player 
			currently playing is the computer (t) or human (NIL)
			
Return Value: the modified player hands list
Assistance Received: none 
********************************************************************* */
|#

(defun updatecurrentplayerhand (playerHands newHand currentPlayer)

	(cond 

		;if the current player is computer
		( currentPlayer
			(updatecomphand playerHands newHand)
		)

		;if current player is human
		(t
			(updatehumanhand playerHands newHand)
		)
	)
)


#|
/* ********************************************************************* 
Function Name: setcurrentplayerskipindicator 
Purpose: To indicate that a the current palayer has skipped its turn 
Parameters: 
			turnIndicators, a list of booleans and a number that ensure
			the rules of the game are followed in each turn
			marker, a boolean to indicate whether the current player
			has skipped its turn because boneyard is empty or not
			currentPlayer, a boolean indicating whether the player 
			currently playing is the computer (t) or human (NIL)
			
Return Value: the updated turn indicators
Assistance Received: none 
********************************************************************* */
|#

(defun setcurrentplayerskipindicator (turnIndicators marker currentPlayer)

	(cond 

		;if the current player is computer
		( currentPlayer

			(setcompskipmarker turnIndicators marker)
		)

		;if current player is human
		(t
			(sethumanskipmarker turnIndicators marker)
			
		)
	)
)

#|
/* ********************************************************************* 
Function Name: getcurrentplayertrainnum
Purpose: To identify the personal train number of the current player 
Parameters: 
			currentPlayer, a boolean indicating whether the player 
			currently playing is the computer (t) or human (NIL)
			
Return Value: an integer, the personal train number of the current player
Assistance Received: none 
********************************************************************* */
|#

(defun getcurrentplayertrainnum (currentPlayer)

	(let*
		(
			;the number corresponding to computer train
			(compTrainNum 1)

			;the number corresponding to human train
			(humanTrainNum 2)
															)

		(cond 

			;if the current player is computer
			(currentPlayer
				compTrainNum
			)

			;if current player is human
			(t
				humanTrainNum
			)
		)
	)
)


#|
/* ********************************************************************* 
Function Name: getpersonaltraineligiblity
Purpose: To identify the eligblity of the personal train of the current player 
Parameters: 
			compEligiblity, a boolean indicating whether the computer 
			train is eligible to play on 
			humanEligiblity, a boolean indicating whether the human
			train is eligible to play on 
			currentPlayer, a boolean indicating whether the player 
			currently playing is the computer (t) or human (NIL)
			
Return Value: true if personal train is eligible and false otherwise
Assistance Received: none 
********************************************************************* */
|#

(defun getpersonaltraineligiblity (compEligiblity humanEligiblity currentPlayer)


	(cond
		;if current player is computer
		(currentPlayer
			compEligiblity
		)

		;if current player is human
		(t
			humanEligiblity
		)
	)
)


#|
/* ********************************************************************* 
Function Name: unmarkcurrentplayertrain
Purpose: To unmark the current player's train if player plays on their own train
Parameters: 
			roundElem, a list of lists consisted of the elements of a round 
			(player hands, boneyard, engine, game trains, current player)
			train, the personal train list of the current player
			selectedTrainNum, an integer indicating the selected train
			to play a tile on
			
Return Value: the updated round list
Assistance Received: none 
********************************************************************* */
|#

(defun unmarkcurrentplayertrain (roundElem train selectedTrainNum)

	(let*

		(
			(currentPlayer (getcurrentplayer roundElem) )
															)


		(cond 

			;if tile is being placed on personal train
			( (= selectedTrainNum (getcurrentplayertrainnum currentPlayer) )

				(let*

					(
						;the current game trains
						(gameTrains (gettrains roundElem) )

						;unmark player train (NIL)
						(markerLessTrain (modifyregularmarker train NIL) )

						;update the trains with the new train
						(updatedTrains (updatetrainbyindex gameTrains markerLessTrain selectedTrainNum) )

						;update the round with the new trains
						(roundUpdate (updatetrains roundElem updatedTrains) )	

																											)
					roundUpdate									
				)
			)

			;if tile is being placed on opponent or mexican train
			(t
				;do not unmark ther player's train
				roundElem
			)
		)
	)
)



#| *********************************************
Source Code to help with formatting the displays
********************************************* |#

#|
/* ********************************************************************* 
Function Name: printchar
Purpose: To display a charater multiple times consecutively
Parameters: 
			ch, the character to be displayed
			num, the number of times it should be displayed
			
Return Value: None
Assistance Received: none 
********************************************************************* */
|#

(defun printchar (ch num)

	(cond
		
		( (= num 0) 
			()
		)

		
		(t 
			(princ ch)
			(printchar ch (- num 1) )
		)
	)
)


#|
/* ********************************************************************* 
Function Name: finddigits
Purpose: To find the number of digits in a number
Parameters: 
			num, the integer to find its number of digits
			
Return Value: an integer, the number of digits in the number
Assistance Received: none 
********************************************************************* */
|#

(defun finddigits (num)

	(cond 
          
       ( (> 0 (- num 10)) 

       		1
       )

       (t 
       		(+ (finddigits (/ num 10) ) 1 )
       )
    )
)

#|
/* ********************************************************************* 
Function Name: printnewlines
Purpose: To print multiple new lines
Parameters: 
			numLines, the number of new lines to print
Return Value: None
Assistance Received: none 
********************************************************************* */
|#

(defun printnewlines (numLines)

	(cond 

		( (= numLines 0)

			()
		)


		(t
			(terpri)
			(printnewlines (- numLines 1) )
		)
	)
)


#|
/* ********************************************************************* 
Function Name: addproperspaces
Purpose: To add appropriate spacing for printing event results
Parameters: 
			score, the player score being displayed
			maxSpace, the maximum number of spaces between score and 
			the winner of the round/game

Return Value: None
Assistance Received: none 
********************************************************************* */
|#

(defun addproperspaces (score maxSpace)

	(let*
		(
			;number of digits in score
			(numDigits (finddigits score) )

			;spaces to display
			(spacesToAdd (- maxSpace numDigits) )
													)
		(printchar #\space spacesToAdd)
		(princ "|")
	)
)



#| *********************************************
Source Code for miscellaneous functionalities
********************************************* |#

#|
/* ********************************************************************* 
Function Name: printtile
Purpose: To print a tile on the screen 
Parameters: 
			tile, the tile to be displayed
			
Return Value: None
Assistance Received: none 
********************************************************************* */
|#

(defun printtile (tile)

	(princ (getfrontvalue tile) )
	(princ "-")
	(princ (getbackvalue tile) )
)

#|
/* ********************************************************************* 
Function Name: isvalinlist
Purpose: To check whether a number exists in a list of numbers 
Parameters: 
			numbers, the list of numbers to search through
			num, the number to look for in the list
			
Return Value: true if the number exists in the list and false otherwise
Assistance Received: none 
********************************************************************* */
|#

(defun isvalinlist (numbers num)
	
	(cond

		( (null numbers)

			NIL
		)

		;if the number is in the list
		( (= (first numbers) num)

			t
		)

		(t
			(isvalinlist (rest numbers) num)
		)
	)
)



#| *********************************************
Source Code to write to a file
********************************************* |#


#|
/* ********************************************************************* 
Function Name: savegame
Purpose: To write the current game status to a file
Parameters: 
			fullGame, a list of lists consisted of all elements of 
			the game 
			
Return Value: None
Algorithm:
	(1) prompt user to enter the filename
	(2) extract the elements of the game and the round
	(3) write the round number to the file 
	(4) write the computer score to the file 
	(5) write the computer hand to the file 
	(6) write the computer train to the file
	(7) write the human score to the file 
	(8) write the human hand to the file 
	(9) write the human train to the file
	(10) write the mexican train to the file 
	(11) write the boneyard to the file 
	(12) write the next player to the file

Assistance Received: none 
********************************************************************* */
|#

(defun savegame (fullGame)

	(terpri)
	(princ "ENTER A FILENAME")
	(terpri)

	(let*

		(
			;filename given by user
			(filename (write-to-string (read) ) )

			;the maximum number of characters in the filename
			(nameSize 10)
																	)

		(cond 

			;if filename is too long
			( (> (length filename) nameSize)

				(terpri)
				(princ "FILENAME MUST BE AT MOST 10 CHARACTERS!")
				
				;user must provide filename again 
				(savegame fullGame)
			)

			;if user input is a string
			(t
				(let*
					(

						;extract the game list
						(gameElem (first fullGame) )

						;extract the round list
						(roundElem (first (rest fullGame) ) )

						;extract round number
						(roundNumber (getroundnumber gameElem) )

						;extract player scores
						(playerScores (getplayerscores gameElem) )
						(compScore (getcompscore playerScores) )
						(humanScore (gethumanscore playerScores) )

						;extract player hands
						(playerHands (getplayerhands roundElem) )
						(compHand (getcomphand playerHands) )
						(humanHand (gethumanhand playerHands) )

						;extract the boneyard
						(boneyard (getboneyard roundElem) )

						;extract the engine
						(engine (getroundengine roundElem) )

						;extract the game trains
						(gameTrains (gettrains roundElem) )
						(compTrain (getcomptrain gameTrains) )
						(humanTrain (gethumantrain gameTrains) )
						(mexicanTrain (getmexicantrain gameTrains) )

						;extract the next player
						(nextPlayer (getcurrentplayer roundElem) )

																		)
					
					;supersede --> overwrite file if it already exists
					(with-open-file (stream filename :direction :output :if-exists :supersede)

					   (format stream "( ")
					   (terpri stream)

					   ;write the round number
					   (format stream "; Round:")
					   (terpri stream)
					   (format stream (write-to-string roundNumber) )
					  
					   (terpri stream) 
					   (terpri stream)

					   ;write the computer score
					   (writescoretofile stream "Computer" compScore)

					   (terpri stream)

					   ;write the computer hand
					   (writehandtofile stream "Computer" compHand)

					   (terpri stream)

					   ;write the computer train
					   (writecomptraintofile stream compTrain engine)

					   (terpri stream)
					   (terpri stream)
					   (terpri stream)

					   ;write the human score
					   (writescoretofile stream "Human" humanScore)

					   (terpri stream)

					   ;write the human hand
					   (writehandtofile stream "Human" humanHand)

					   (terpri stream)

					   ;write the human train
					   (writehumantraintofile stream humanTrain engine)

					   (terpri stream)
					   (terpri stream)
					   (terpri stream)

					   ;write the mexican train
					   (format stream "; Mexican Train")
					   (terpri stream)
					   (format stream "( ")
					   (writepiletofile stream (rest mexicanTrain) )
					   (format stream ")")

					   (terpri stream)
					   (terpri stream)
					   
					   ;write the boneyard
					   (format stream "; Boneyard")
					   (terpri stream)
					   (format stream "( ")
					   (writepiletofile stream boneyard)
					   (format stream ")")

					   (terpri stream)
					   (terpri stream)

					   ;write the next player
					   (format stream "; Next Player")
					   (terpri stream)
					   (writenextplayer stream nextPlayer)
					   (terpri stream)

					   (format stream ")")
					   
					)
				)

				(princ "GAME SUCCESSFULLY SAVED!")
				(terpri)
			)
		)
	)	
)

#| *********************************************
Source Code to write to a file
********************************************* |#


#|
/* ********************************************************************* 
Function Name: writescoretofile
Purpose: To write a player score to a file
Parameters: 
			stream, the file object to write to
			player, the player name in string format
			score, an integer indicating the player score

Return Value: None
Assistance Received: none 
********************************************************************* */
|#

(defun writescoretofile (stream player score)

	(format stream "; ")
	(format stream player)
	(format stream " Score")
	(terpri stream)
	(format stream (write-to-string score) )
)

#|
/* ********************************************************************* 
Function Name: writehandtofile
Purpose: To write a player hand to a file
Parameters: 
			stream, the file object to write to
			player, the player name in string format
			hand, the list of tiles in the player hand

Return Value: None
Assistance Received: none 
********************************************************************* */
|#

(defun writehandtofile (stream player hand)
	
	(format stream "; ")
	(format stream player)
	(format stream " Hand")
	(terpri stream)
	(format stream "( ")
	(writepiletofile stream hand)
	(format stream ")")
)


#|
/* ********************************************************************* 
Function Name: writecomptraintofile
Purpose: To write the computer train to a file
Parameters: 
			stream, the file object to write to
			compTrain, the tiles in the computer train
			engine, the engine of the round (tile)

Return Value: None
Algorithm:
	(1) add engine to the train
	(2) add marker to the train (if applicable)
	(3) reverse the train
	(4) flip all the tiles in the train
	(5) write the train to the file

Assistance Received: none 
********************************************************************* */
|#

(defun writecomptraintofile (stream compTrain engine)

	(format stream "; Computer Train")
	(terpri stream)
	(format stream "( ")

	(let*

		(
			;establish the regular marker
			(trainMarked (istrainmarked compTrain) )

			;remove the marker
			(trainNoMarker (rest compTrain) )

			;add engine to the beginning of train
			(trainWithEngine (cons engine trainNoMarker) )

			;reverse the train
			(reversedTrain (reverse trainWithEngine) )

			;flip all tiles
			(flippedTrain (fliptilesinpile reversedTrain) )

																)

		(cond

			;if the train is marked
			( trainMarked

				(let*

					(
						(markedTrain (cons 'M flippedTrain) )
																	)

					(writepiletofile stream markedTrain)
				)
			)

			;if train is not marked
			(t
				(writepiletofile stream flippedTrain)
			)
		)
	)


	(format stream ")")
)


#|
/* ********************************************************************* 
Function Name: writehumantraintofile
Purpose: To write the human train to a file
Parameters: 
			stream, the file object to write to
			humanTrain, the tiles in the human train
			engine, the engine of the round (tile)

Return Value: None
Algorithm:
	(1) add engine to the train
	(2) add marker to the train (if applicable)
	(3) write the train to the file
	
Assistance Received: none 
********************************************************************* */
|#

(defun writehumantraintofile (stream humanTrain engine)

	(format stream "; Human Train")
	(terpri stream)
	(format stream "( ")

	(let*

		(
			;establish the regular marker
			(trainMarked (istrainmarked humanTrain) )

			;remove the marker
			(trainNoMarker (rest humanTrain) )

			;add engine to the beginning of train
			(trainWithEngine (cons engine trainNoMarker) )

																)

		(cond

			;if the train is marked
			( trainMarked

				(let*

					(
						(markedTrain (append trainWithEngine (list 'M) ) )
																				)

					(writepiletofile stream markedTrain)
				)
			)

			;if train is not marked
			(t
				(writepiletofile stream trainWithEngine)
			)
		)
	)

	(format stream ")")
)


#|
/* ********************************************************************* 
Function Name: writenextplayer
Purpose: To write the next player to a file
Parameters: 
			stream, the file object to write to
			nextPlayer, a boolean indicating whether the player 
			currently playing is the computer (t) or human (NIL)

Return Value: None	
Assistance Received: none 
********************************************************************* */
|#

(defun writenextplayer (stream nextPlayer)

	(cond

		;if next player is computer
		(nextPlayer

			(format stream "Computer")
		)

		(t

			(format stream "Human")
		)
	)
)


#|
/* ********************************************************************* 
Function Name: writepiletofile
Purpose: To write a pile to a file
Parameters: 
			stream, the file object to write to
			pile, the list of tiles to write to file

Return Value: None	
Assistance Received: none 
********************************************************************* */
|#

(defun writepiletofile (stream pile)

	(cond

		( (null pile)
			()
		)


		(t 
			(format stream (write-to-string (first pile) ) )
			(format stream " ")

			(writepiletofile stream (rest pile) )
		)
	)
)



#| *********************************************
Source Code to read from a file
********************************************* |#

#|
/* ********************************************************************* 
Function Name: loadgame
Purpose: To load the game from a file
Parameters: None
Return Value: a list of lists consisted of all elements of the game 
Assistance Received: none 
********************************************************************* */
|#

(defun loadgame ()

	(let*

		(
			;get file object
			(file (getfileobj) )

			;read the full game
			(loadedGame (readallfiledata file) )
													)
		loadedGame
	)
)

#|
/* ********************************************************************* 
Function Name: getfileobj
Purpose: To get the file object to read from
Parameters: None
Return Value: the file object to read from
Algorithm:
	(1) prompt user to enter the filename
	(2) 
		if user input is invalid or file is not found
			user must enter filename again
		end if
		else
			record the file object
		end else

Assistance Received: none 
********************************************************************* */
|#

(defun getfileobj ()

	(terpri)
	(princ "ENTER THE FILENAME")
	(terpri)

	(let*

		(
			;filename given by user
			(filename (write-to-string (read) ) )

			;the maximum number of characters in the filename
			(nameSize 10)
																) 

		(cond 

			;if filename is too long
			( (> (length filename) nameSize)

				(terpri)
				(princ "FILENAME MUST BE AT MOST 10 CHARACTERS!")

				;user must select again
				(getfileobj) 
			)

			;if filename is a string
			(t
				(let*

					(
						;the file object
						(file (open filename :if-does-not-exist nil) )
																			)

					(cond 

						;if file is not found or cannot be opened 
						( (not file)

							(princ "FILE NOT FOUND!")
							(terpri)
							(getfileobj)
						)

						;file is found
						(t
							file				
						)
					)
				)
			)
		)
	)	
)

#|
/* ********************************************************************* 
Function Name: readallfiledata
Purpose: To read a a previosuly saved game status from a file
Parameters: 
			file, the file object to read from

Return Value: a list of lists consisted of all elements of the game 
Assistance Received: none 
********************************************************************* */
|#

(defun readallfiledata (file)

	(let*

		(
			;the player hands
			(compHand () )
			(humanHand () )
			(playerHands (list compHand humanHand) )

			(boneyard () )

			(roundEngine  () )

			;the trains
			(compTrain () )
			(humanTrain () )
			(mexicanTrain () )
			(gameTrains (list compTrain humanTrain mexicanTrain) )

			;default player to start
			(currentPlayer t)

			;the elements of the round 
			(roundElem (list playerHands boneyard roundEngine gameTrains currentPlayer) )

			;default scores 
			(compScore 0)
			(humanScore 0)
			(playerScores (list compScore humanScore) )

			;default round number
			(roundNumber 1)

			;the default elements of the game
			(gameElem (list roundNumber playerScores) )

			;the default entire game information
			(fullGame (list gameElem roundElem) )

			;read the entire data from file
			(fileContent (read file nil) )

			;the number of the element being read from the big list of the file
			(elemNum 1)

			;record the content of each line properly
			(loadedGame (recordgameinfo fullGame fileContent elemNum) )

																							)
		
		(close file)
		loadedGame
	)	
)


#|
/* ********************************************************************* 
Function Name: recordgameinfo
Purpose: To record the game/round elements from file
Parameters: 
			fullGame, a list consisted of all elements of the game
			fileContent, a list of the elements of the file list
			elemNum, the number of the element being read from the 
			big list of the file
			
Return Value: a list of lists consisted of all elements of the game 
			  read from file
Algorithm:
	(1) 
		if element 1 is being read
			record the round number
		end if

		else if element 2 or element 5 is being read
			record the player score
		end else if 

		else if element 3 or element 6 is being read
			record the player hand tiles
		end else if 

		else if element 4 or element 8 is being read
			record the train tiles 
		end else if

		else if element 7 is being read
			record the engine
			record the human train tiles
		end else if

		else if element 9 is being read
			record the boneyard tiles
		end else if

		else
			record the next player info
		end else
		
Assistance Received: none 
********************************************************************* */
|#

(defun recordgameinfo (fullGame fileContent elemNum)


	(let*

		(
			;the game information
			(gameElem (first fullGame) )

			;the round information
			(roundElem (first(rest fullGame)) )
													)

		
		(cond

			( (null fileContent)
				fullGame
			)

			;if the element one of the file list is being read
			( (= elemNum 1)

				(let*
					
					(
						;update the round number
						(updatedGameElem (setroundnumber gameElem (first fileContent) ) )

						;update the full game
						(updatedFullGame (list updatedGameElem roundElem ) )
																					)

					(recordgameinfo updatedFullGame (rest fileContent) (+ elemNum 1) )
				)
			)

			;if the element two or five of the file list is being read
			( (or (= elemNum 2) (= elemNum 5) )

				(let*
					
					(
						;get the player scores
						(playerScores (getplayerscores gameElem) )

						;update the player scores 
						(updatedScores (readscoreline playerScores (first fileContent) elemNum) )

						;update the game
						(updatedGameElem (updateplayerscores gameElem updatedScores) )

						;update the full game
						(updatedFullGame (list updatedGameElem roundElem) )
																								)

					(recordgameinfo updatedFullGame (rest fileContent) (+ elemNum 1) )
				)
			)

			;if the element three or six of the file list is being read
			( (or (= elemNum 3) (= elemNum 6) )

				(let*
					
					(
						;get the player hands
						(playerHands (getplayerhands roundElem) )

						;update the player hands
						(updatedHands (readhandline playerHands (first fileContent) elemNum) )

						;update the round
						(updatedRound (updateplayerhands roundElem updatedHands) )

						;update the full game
						(updatedFullGame (list gameElem updatedRound) )
																						)

					(recordgameinfo updatedFullGame (rest fileContent) (+ elemNum 1) )
				)
			)

			;if the element four or eight of the file list is being read
			( (or (= elemNum 8) (= elemNum 4) )

				(let*
					
					(
						;get the game trains
						(gameTrains (gettrains roundElem) )

						;update the trains
						(updatedTrains (readtrainline gameTrains (first fileContent) elemNum) )

						;update the round
						(updatedRound (updatetrains roundElem updatedTrains) )

						;update the full game
						(updatedFullGame (list gameElem updatedRound) )
																								)

					(recordgameinfo updatedFullGame (rest fileContent) (+ elemNum 1) )
				)
			)

			;if the element seven of the file list is being read
			( (= elemNum 7)

				(let*

					(
						;record the engine
						(roundWithEngine (readengineline roundElem (first fileContent) ) )	

						;get the game trains
						(gameTrains (gettrains roundWithEngine) )

						;update the trains
						(updatedTrains (readtrainline gameTrains (first fileContent) elemNum) )

						;update the round
						(updatedRound (updatetrains roundWithEngine updatedTrains) )

						;update the full game
						(updatedFullGame (list gameElem updatedRound) )
																							)

					(recordgameinfo updatedFullGame (rest fileContent) (+ elemNum 1) )
				)
			)

			;if the element nine of the file list is being read
			( (= elemNum 9)
				
				(let*

					(
						;update the round with the boneyard
						(updatedBoneyard (updateboneyard roundElem (first fileContent) ) )

						;;update the full game
						(updatedFullGame (list gameElem updatedBoneyard) )
																									)

					(recordgameinfo updatedFullGame (rest fileContent) (+ elemNum 1) )
				)
			)

			;if the element ten of the file list is being read
			(t

				(let*

					(
						;update round with current player
						(updatedRound (readcurrentplayerline roundElem (first fileContent) ) )

						;update the full game
						(updatedFullGame (list gameElem updatedRound) )
																								)

					(recordgameinfo updatedFullGame (rest fileContent) (+ elemNum 1) )
				)
			)
		)
	)
)


#|
/* ********************************************************************* 
Function Name: readscoreline
Purpose: To read player scores from file
Parameters: 
			playerScores, the list of player scores
			newScore, the player score read from file
			elemNum, the number of the element being read from the 
			big list of the file
			
Return Value: the updated player scores list
Assistance Received: none 
********************************************************************* */
|#

(defun readscoreline (playerScores newScore elemNum)

	(cond 

		;if computer score is being updated
		( (= elemNum 2)

			(updatecompscore playerScores newScore)
		)

		;if human score is being updated
		(t
			(updatehumanscore playerScores newScore)
		)
	)
)


#|
/* ********************************************************************* 
Function Name: readengineline
Purpose: To read engine from file
Parameters: 
			roundElem, a list of lists consisted of the elements of a round 
			(player hands, boneyard, engine, game trains, current player)
			line, the content of the current line being read
			
Return Value: the updated round list 
Assistance Received: none 
********************************************************************* */
|#

(defun readengineline (roundElem line)

	(let*

		(
			;round engine
			(roundEngine (first line) )

			;update the round with engine
			(roundWithEngine (updateroundengine roundElem roundEngine) )

																				)
		roundWithEngine
	)
)

#|
/* ********************************************************************* 
Function Name: readhandline
Purpose: To update the player hand from file
Parameters: 
			playerHands, the list of tiles in the player hands
			newHand, the list of player hand read from file
			elemNum, the number of the element being read from the 
			big list of the file
			
Return Value: the updated player hands list 
Assistance Received: none 
********************************************************************* */
|#

(defun readhandline (playerHands newHand elemNum)

	(cond 

		;if computer hand is being updated
		( (= elemNum 3)

			(updatecomphand playerHands newHand)
		)

		;if human hand is being updated
		(t
			(updatehumanhand playerHands newHand)
		)
	)
)

#|
/* ********************************************************************* 
Function Name: readtrainline
Purpose: To update a train from file
Parameters: 
			gameTrains, a list consisted of the computer train,  
			human train and mexican train
			newTrain, the list of train read from file
			elemNum, the number of the element being read from the 
			big list of the file
			
Return Value: the updated trains list
Algorithm:
	(1) identify the train being read from file
	(2) update the default train with this train

Assistance Received: none 
********************************************************************* */
|#

(defun readtrainline (gameTrains newTrain elemNum)
	
	(cond 

		;if computer train is being updated 
		( (= elemNum 4)

			(let*

				(
					;modify the computer train
					(updatedCompTrain (readcomptrainline newTrain) )

					;update the computer train
					(updatedTrains (updatecomptrain gameTrains updatedCompTrain) )
																					)
				updatedTrains
			)
		)

		;if human train is being updated
		( (= elemNum 7)

			(let*

				(
					;modify the human train
					(updatedHumanTrain (readhumantrainline newTrain) )

					;update the human train
					(updatedTrains (updatehumantrain gameTrains updatedHumanTrain) )
																							)

				updatedTrains
			)
		)

		;if mexican train is being updated
		(t

			(let*

				(
					;modify the mexican train
					(updatedMexicanTrain (readmexicantrainline newTrain) )

					;update the mexican train
					(updatedTrains (updatemexicantrain gameTrains updatedMexicanTrain) )
																								)

				updatedTrains
			)
		)
	)																
)


#|
/* ********************************************************************* 
Function Name: readcomptrainline
Purpose: To read the computer train from file
Parameters: 
			compTrain, the default list of computer train
			
Return Value: the computer train list read from file
Assistance Received: none 
********************************************************************* */
|#

(defun readcomptrainline (compTrain)

	(let*
		(
			;check if train is marked
			(regularMarker (istrainfilemarked (first compTrain) ) )

			;remove the marker from file
			(markerLessTrain (getcompwithoutmarker compTrain regularMarker) )

			;reverse the train
			(reversedTrain (reverse markerLessTrain) )

			;remove the engine
			(engineLessTrain (rest reversedTrain) )

			;flip all the tiles in the train
			(flippedTrain (fliptilesinpile engineLessTrain) )

			;check if train is orphan double
			(orphanMarker (trainendswithdouble flippedTrain) )

			;establish the indicators
			(indicators (list regularMarker orphanMarker) )

			;add indicators to the train
			(completeTrain (cons indicators flippedTrain) )
																				)
			
			;now the train markers are established
			;engine is removed
			;train is reversed
			completeTrain
	)
)


#|
/* ********************************************************************* 
Function Name: readhumantrainline
Purpose: To read the human train from file
Parameters: 
			humanTrain, the default list of human train
			
Return Value: the human train list read from file
Assistance Received: none 
********************************************************************* */
|#

(defun readhumantrainline (humanTrain)
	
	(let*
		(
			;remove the engine
			(engineLessTrain (rest humanTrain) )

			;check whether train is marked 
			(regularMarker (istrainfilemarked (getlastelem engineLessTrain) ) )

			;remove the marker from file
			(markerLessTrain (gethumantrainwithoutmarker engineLessTrain regularMarker) )

			;check if train is orphan double
			(orphanMarker (trainendswithdouble markerLessTrain) )

			;establish the indicators
			(indicators (list regularMarker orphanMarker) )

			;add indicators to the train
			(completeTrain (cons indicators markerLessTrain) )
																							)
		completeTrain
	)
)

#|
/* ********************************************************************* 
Function Name: readmexicantrainline
Purpose: To read the mexican train from file
Parameters: 
			mexicanTrain, the default list of mexican train
			
Return Value: the mexican train list read from file
Assistance Received: none 
********************************************************************* */
|#

(defun readmexicantrainline (mexicanTrain)

	(let*
		(
			;check if train is orphan double
			(orphanMarker (trainendswithdouble mexicanTrain) )

			;identify the indicators
			(indicators (list NIL orphanMarker) )

			;mark train as orphan if it is
			(updatedOrphanTrain (cons indicators mexicanTrain) )
																	)

		updatedOrphanTrain
	)
)

#|
/* ********************************************************************* 
Function Name: getcompwithoutmarker
Purpose: To remove computer marker read from file
Parameters: 
			compTrain, the computer train read from file
			marked, a boolean indicating whether computer train
			read from file is marked
			
Return Value: the computer train list (marker removed)
Assistance Received: none 
********************************************************************* */
|#

(defun getcompwithoutmarker (compTrain marked)

	(cond 

		;if train is marked 
		( marked

			;remove the first element (marker)
			(rest compTrain)
		)

		;if train is not marked
		(t
			compTrain
		)
	)
)


#|
/* ********************************************************************* 
Function Name: gethumantrainwithoutmarker
Purpose: To remove human marker read from file
Parameters: 
			humanTrain, the human train read from file
			marked, a boolean indicating whether human train
			read from file is marked
			
Return Value: the human train list (marker removed)
Assistance Received: none 
********************************************************************* */
|#

(defun gethumantrainwithoutmarker (humanTrain marked)

	(cond 
		;if train is marked 
		( marked

			;remove the last element (marker)
			(removelastelem humanTrain)
		)

		;if train is not marked
		(t
			humanTrain
		)
	)
)

#|
/* ********************************************************************* 
Function Name: istrainfilemarked
Purpose: To check whether a train read from file is marked
Parameters: 
			trainEnd, the last element of the train read from file
			
Return Value: true if train is marked and false otherwise
Assistance Received: none 
********************************************************************* */
|#

(defun istrainfilemarked (trainEnd)

	(cond

		;if last element is a symbol
		( (symbolp trainEnd)

			;the train is marked
			t
		)

		(t
			NIL
		)
	)
)

#|
/* ********************************************************************* 
Function Name: readcurrentplayerline
Purpose: To read the current player from file
Parameters: 
			roundElem, a list of lists consisted of the elements of a round 
			(player hands, boneyard, engine, game trains, current player)
			currentPlayer, a boolean indicating whether the player 
			currently playing is the computer (t) or human (NIL)
			
Return Value: the updated round with current player
Assistance Received: none 
********************************************************************* */
|#

(defun readcurrentplayerline (roundElem currentPlayer)

	(cond 

		;if current player is computer
		( (string= currentPlayer "COMPUTER")

			(setcurrentplayer roundElem t)
		)

		;if current player is human
		(t
			(setcurrentplayer roundElem NIL)
		)
	)
)



#| ***************************************************
Source Code to determine and display round/game result
*************************************************** |#

#|
/* ********************************************************************* 
Function Name: displaygameresult
Purpose: To determine and display the winner of the game (or a tie)
Parameters: 
			playerScores, the list of final player scores
			
Return Value: none
Assistance Received: none 
********************************************************************* */
|#

(defun displaygameresult (playerScores)

	(let*
		
		(

			;extract player scores
			(compScore (getcompscore playerScores) )
			(humanScore (gethumanscore playerScores) )

			;idenitfy the result of game
			(roundOutcome (compareScores compScore humanScore) )
																	)

		;announce the game winner
		(announceeventresult "GAME" roundOutcome)

		;display the game scores
		(displayeventscores compScore humanScore)
	)
)


#|
/* ********************************************************************* 
Function Name: displayroundresult
Purpose: To determine the winner of round (or a tie)
Parameters: 
			compHand, a list of tiles in computer hand
			humanHand, a list of tiles in human hand  

Return Value: an integer,
			0 --> computer has lower score
			1 --> human has lower score
            2 --> scores are equal
Algorithm:
	(1) 
		if computer hand is empty
			computer is the winner
		end if 

		if human hand is empty
			human is the winner
		end if

		else 
			calculate the pip sum for each player hand 
			the player with the lower pip sum wins
		end else

Assistance Received: none 
********************************************************************* */
|#

(defun displayroundresult (compHand humanHand)

	(let*
		
		(
			;outcome
			(compWins 0)
			(humanWins 1)
							)

		(cond

			;if computer hand is empty
			( (= (length compHand) 0 )

				compWins
			)

			;if human hand is empty
			( (= (length humanHand) 0 )

				humanWins
			)

			;both players still have tiles
			(t

				;the player with lower pip sum wins (if applicable)
				(let*

					(
						;calculate pip sum for both hands
						(compSum (getpilepipsum compHand) )
						(humanSum (getpilepipsum humanHand) )
																)

					;will determine winner based on pip sum 
					(compareScores compSum humanSum)
				)
			)
		)
	)
)


#|
/* ********************************************************************* 
Function Name: announceeventresult
Purpose: To display the winner of round/game (or a tie)
Parameters: 
			event, the event in string format of which the result 
			is being displayed for
			result, an integer indicating the outcome of the event

Return Value: none
Assistance Received: none 
********************************************************************* */
|#

(defun announceeventresult (event result)

	(let*
		
		(
			;outcome
			(compWins 0)
			(humanWins 1)
							)

		(princ #\space)
		(printchar '-' 24)
		(terpri)
		(princ "| ")
		(princ event)
		(princ " RESULT")


		(cond

			( (string= event "ROUND")

				(printchar #\space 11)
			)

			;if game result is being printed
			(t
				(printchar #\space 12)
			)
		)

		(princ "|")
		(terpri)
		(princ "| WINNER: ")

		(cond

			( (= result compWins)
				
				(princ "COMPUTER")
				(printchar #\space 7)
				(princ "|")
				(terpri)
			)

			( (= result humanWins)

				(princ "HUMAN")
				(printchar #\space 10)
				(princ "|")
				(terpri)
			)

			;if tie is the result
			(t
				(princ "TIE")
				(printchar #\space 12)
				(princ "|")
				(terpri)
			)
		)
	)
)

#|
/* ********************************************************************* 
Function Name: displayeventscores
Purpose: To display the player scores at the end of round/game
Parameters: 
			compScore, an integer indicating the computer score 
			humanScore, an integer indicating the human score 

Return Value: none
Assistance Received: none 
********************************************************************* */
|#

(defun displayeventscores (compScore humanScore)

	(princ "| COMPUTER SCORE: ")
	(princ compScore)
	(addproperspaces compScore 7)
	(terpri)

	(princ "| HUMAN SCORE: ")
	(princ humanScore)
	(addproperspaces humanScore 10)
	(terpri)

	(princ #\space)
	(printchar '-' 24)
	(printnewlines 3)
)
