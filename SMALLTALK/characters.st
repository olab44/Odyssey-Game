Object subclass: NPC [
    | name availableDialogues allDialogues |

    NPC class >> new [
        ^ super new init
    ]

    init [
        name := ''.
        availableDialogues := Dictionary new.
        allDialogues := Dictionary new.
    ]

    NPC >> name: aName dialogues: alDialogues [
        name := aName.
        allDialogues := alDialogues.
    ]
    setAvailableDialogue: aKey atLocation: aLocation [
        availableDialogues at: aLocation put: (allDialogues at: aKey ifAbsent: [nil])
    ]

    talk: gameState [
        | dialogue |
        dialogue := availableDialogues at: (gameState getCurrentLocation getName) ifAbsent: [nil].

        (dialogue isNil) ifTrue: [
            ^ #('It`s not a time nor place for a talk with someone who`s busy - or someone who`s not even there.')
        ].
        ^ dialogue value: gameState
    ]
]

Object subclass: CharacterPopulation [
    CharacterPopulation class >> createCharacters [
        | characters |
        characters := Dictionary new.
        characters at: 'crew' put: self createCrew.
        characters at: 'circe' put: self createCirce.
        characters at: 'hermes' put: self createHermes.
        characters at: 'charon' put: self createCharon.
        characters at: 'tiresias' put: self createTiresias.
        characters at: 'mother' put: self createMother.
        characters at: 'agamemnon' put: self createAgamemnon.
        characters at: 'achilles' put: self createAchilles.
        ^characters
    ]

    CharacterPopulation class >> createCrew [
        | character dialogues |

        dialogues := Dictionary new.
        dialogues at: 'post_circe_confrontation' put: [ :gameState |
            (gameState getCharacter: 'circe') setAvailableDialogue: 'underworld_advice' atLocation: 'circe_island'.

            #('Your crew gathers, their expressions serious. "Captain," they say, "it`s time we resume our journey to Ithaca."'
            'They remind you of the goal that has driven you across the seas, and their loyalty fills you with resolve.'
            'You should now talk to Circe to seek further guidance from her.')
        ].
        dialogues at: 'hangry' put: [ :gameState |
            | choice message |

            Transcript show: 'Your crew gathers around, their faces pale with hunger.'; cr.
            Transcript show: 'One of them speaks up: "Captain, we cannot last without food. These cattle are our only chance."'; cr.
            Transcript show: 'You remember Tiresias`s warning not to eat the sacred cattle of Helios.'; cr.
            Transcript show: 'Make your choice: "do not eat cattle" to let your crew starve, or "eat cattle" to eat, but risk the wrath of the gods.'; cr.

            Transcript show: '|> '.
            choice := FileStream stdin nextLine.

            (choice ~= 'eat cattle' and: [choice ~= 'do not eat cattle']) ifTrue: [
                message := #('You hesitate too long and so the crew decides for you, led by their hunger.').
                choice := 'eat cattle'
            ].

            choice = 'eat cattle' ifTrue: [
                message := #('Reluctantly, you allow the crew to slaughter the sacred cattle of Helios.'
                    'The feast restores their strength, but you sense a dark omen in the air.').

                    (gameState hasItem: 'potion_recipe') ifTrue: [
                        gameState addItemToEq: 'helios_blood'.
                        message := message, #('As you consume the cattle, you find a special vial containing the blood of Helios`s sacred beast.'
                            'This is the final ingredient needed to complete the elixir of strength, you realize.'
                            'You may want to find some time to "complete the elixir".')
                    ].
                message := message, #('However, for now, the storm miraculously ceases. You may take your chance to embark and sail onwards.').
                (gameState getLocation: 'sun_god_storm') setAccess: true.
            ].

            choice = 'do not eat cattle' ifTrue: [
                message := #('You stand firm in your decision: "We will not eat the cattle of the Sun God."'
                    'The crew protests but ultimately obeys. Over the next days, hunger claims the lives of tens of your men'
                    'You eventually have no choice, but to talk to them again - and make a different, damned, choice.').
                message := message, (gameState killCrew: 50).
            ].

            message
        ].
        character := NPC new.
        character name: 'crew' dialogues: dialogues.
        character setAvailableDialogue: 'hangry' atLocation: 'sun_god_island'.

        ^character
    ]

    CharacterPopulation class >> createCirce [
        | character dialogues |

        dialogues := Dictionary new.
        dialogues at: 'first_confrontation' put: [ :gameState |
            (gameState hasItem: 'magic_herb') ifTrue: [
                | choice message |
                Transcript show: 'Holding the magical herb, you feel its protective aura as you step into Circe`s palace.'; cr.
                Transcript show: 'You sense her spells failing against you.'; cr.
                Transcript show: 'You now have a choice: will you "spare" her life or "kill" her?'; cr.

                Transcript show: '|> '.
                choice := FileStream stdin nextLine.

                choice = 'kill' ifTrue: [
                    message := #('You kill Circe, and in doing so, you lose the chance to undo the curse on your crew. You are left at the mercy of her Nymphs, and your journey ends in failure.').
                    gameState setIsGameOver: true
                ].
                choice = 'spare' ifTrue: [
                    (gameState getCharacter: 'crew') setAvailableDialogue: 'post_circe_confrontation' atLocation: 'circe_island'.
                    message := #('You decide to spare Circe, who restores your crew to human form. They are relieved, and gratitude fills the air.'
                        'Time passes; a full year slips by as Circe becomes your ally and lover. The comforts of the island nearly make you forget your quest.'
                        'One day, your crew approaches, urging you to remember Ithaca. You should talk to them to discuss the journey ahead.').
                ].
                (choice ~= 'kill' and: [choice ~= 'spare']) ifTrue: [
                    message := #('You hesitate, and Circe takes advantage of your indecision. She casts a spell, and you and your crew are lost forever.').
                    gameState setIsGameOver: true
                ].

                message
            ] ifFalse: [
                gameState setIsGameOver: true.
                #('Without the protection of the magical herb, Circe’s spell overwhelms you, turning you into a pig, and you lose your life.')
            ]
        ].
        dialogues at: 'underworld_advice' put: [ :gameState |
            (gameState getLocation: 'underworld_sea') setAccess: true.
            gameState addItemToEq: 'empty_bottle'.

            #('Circe nods in understanding, sensing your readiness to continue your journey.'
            '"If you wish to return to Ithaca," she says, "you must first venture west, to the Underworld, then directly come back to me."'
            'But heed my warning — avoid the north, for giants dwell there, and their strength is unmatched.'
            'Circe hands you an empty bottle, saying it may prove useful later in your journey.'
            'With her guidance, you feel prepared to set sail once more.').
        ].
        dialogues at: 'sirens_advice' put: [ :gameState |
            | currentCrew potionCrew remainingCrew hivesCovered protectedCrew scyllaSurvivalRate|
            Transcript show: 'Circe sees the weight of your journey and speaks again with wisdom:'; cr.
            Transcript show: '"Your next trial, Odysseus, is the Sirens` Sea, where their hypnotic singing lures sailors to destruction.'; cr.
            Transcript show: 'You must have your crew plug their ears with beeswax to resist the sound. But you — remain unsealed and tied to the mast to hear their song.'; cr.
            Transcript show: 'After passing the Sirens, you will face Scylla and Charybdis. I advise you to sail closer to Scylla; though she will claim some of your crew, Charybdis devours entire ships.'; cr.
            Transcript show: 'Decide now how many of your crew will assist me in gathering ingredients for a potion that could protect you against Scylla.'; cr.
            Transcript show: 'This potion will be crucial if you choose to pass near her, as it may shield you from becoming one of the unlucky she devours."'; cr.

            currentCrew := gameState getCrewCount.
            Transcript show: 'Each crew member represents a slim percent chance of survival. The rest will collect beeswax to seal their ears.'; cr.
            Transcript show: 'Your crew consists of ', currentCrew asString, ' brave warriors.'; cr.
            Transcript show: 'Please decide the number of crew for the potion:'; cr.

            Transcript show: '|> '.
            potionCrew := (FileStream stdin nextLine) asInteger.
            remainingCrew := currentCrew - (potionCrew min: 100).
            hivesCovered := remainingCrew // 5.
            protectedCrew := (currentCrew asFloat * ((hivesCovered min: 100) asFloat / 100.0)) rounded.
            scyllaSurvivalRate := ((potionCrew min: 100) asFloat / 100.0).

            gameState setCrewProtected: protectedCrew.
            gameState setScyllaSurvivalRate: scyllaSurvivalRate.

            Transcript show: 'Circe confirms your plan:'; cr.
            Transcript show: potionCrew asString, ' crew members will work on the potion, granting you a ', potionCrew asString, '% chance of survival.'; cr.
            Transcript show: protectedCrew asString, ' crew members are protected. Those unprotected by wax will perish under the Sirens` spell.'; cr.

            #('When ready, you may depart towards the Sirens` Sea.')
        ].
        character := NPC new.
        character name: 'circe' dialogues: dialogues.
        character setAvailableDialogue: 'first_confrontation' atLocation: 'circe_island'.

        ^character
    ]

    CharacterPopulation class >> createHermes [
        | character dialogues |

        dialogues := Dictionary new.
        dialogues at: 'holy_moly' put: [ :gameState |
            gameState addItemToEq: 'magic_herb'.
            #('Hermes, appearing like a ghostly figure, approaches with wisdom and a gift - a magical herb.'
            'He offers it to you, saying it will protect you from Circe`s spells. You may need it if you choose to confront her.').
        ].
        character := NPC new.
        character name: 'hermes' dialogues: dialogues.
        character setAvailableDialogue: 'holy_moly' atLocation: 'circe_island'.
        ^character
    ]

    CharacterPopulation class >> createCharon [
        | character dialogues|

        dialogues := Dictionary new.
        dialogues at: 'challenge' put: [ :gameState |
            #('Charon, the ferryman, explains the rules of the River Styx.'
            'He says he will help you cross, but only if you solve the puzzle.'
            'He can only take one thing at a time: the crew, the wine, or Cerberus, the three-headed dog.'
            'However, if the crew is left with the wine or Cerberus, disaster will strike.'
            'You must cross the river in a specific order to avoid tragedy.'
            'To face the trial "start challenge".')
        ].
        character := NPC new.
        character name: 'charon' dialogues: dialogues.
        character setAvailableDialogue: 'challenge' atLocation: 'underworld'.
        ^character
    ]

    CharacterPopulation class >> createTiresias [
        | character dialogues |

        dialogues := Dictionary new.
        dialogues at: 'prophet_advice' put: [ :gameState |
            (gameState getCharacter: 'mother') setAvailableDialogue: 'underwold_chat' atLocation: 'underworld'.
            (gameState getCharacter: 'achilles') setAvailableDialogue: 'underwold_chat' atLocation: 'underworld'.
            (gameState getCharacter: 'agamemnon') setAvailableDialogue: 'underwold_chat' atLocation: 'underworld'.
            (gameState getCharacter: 'circe') setAvailableDialogue: 'sirens_advice' atLocation: 'circe_island'.

            #('Tiresias, the prophet, speaks to you with grave solemnity:'
            '"I see your future, Odysseus. You may reach Ithaca, but the path will be fraught with hardships.'
            'The island of Helios and its sacred cattle will be your doom if you dare approach them.'
            'The fates have already sealed the tragic death of your mother, which you have yet to learn."'
            'The ferryman urges you to hurry, though he offers you the chance to talk one of three shades: your mother, Achilles, or Agamemnon.'
            'But choose wisely, for you can only speak with one of them...')
        ].
        character := NPC new.
        character name: 'tiresias' dialogues: dialogues.
        ^character
    ]

    CharacterPopulation class >> createMother [
        | character dialogues |
        dialogues := Dictionary new.
        dialogues at: 'underwold_chat' put: [ :gameState |
            | message sea |
            gameState addItemToEq: 'charybdis_lure'.

            message := #('A spectral figure appears before you — your mother, Anticleia.'
            'She tells you with a sorrowful voice, "My dear son, I died of grief and longing for you.'
            'The days of our separation broke my heart. I wish I could have seen you return to Ithaca."'
            'With a soft sigh, she hands you a charm — a Charybdis Lure, a precious item that may one day save you from the whirlpool.').

            (gameState getLongerUnderworldStay) ifFalse: [
                message := message, #('Now you must return to your ship. Further conversations are no longer possible. You`re at sea, leaving the Underworld behind.').
                sea := gameState getLocation: 'underworld_sea'.
                gameState setCurrentLocation: sea.
                sea setAccess: false.
            ].

            message
        ].
        character := NPC new.
        character name: 'mother' dialogues: dialogues.
        ^character
    ]

    CharacterPopulation class >> createAgamemnon [
        | character dialogues |
        dialogues := Dictionary new.
        dialogues at: 'underwold_chat' put: [ :gameState |
            | message sea |

            message := #('Agamemnon, the great king of Mycenae, speaks to you from the shadows:'
            '"I was betrayed by my wife, Clytemnestra, and murdered upon my return to Mycenae. But you, Odysseus, will fare better, I hope."'
            'He offers you a map of the seas ahead — knowledge of the islands that await you on your journey.'
            'You`re remainded of an option you have - "show map".').

            (gameState getLongerUnderworldStay) ifFalse: [
                message := message, #('Now you must return to your ship. Further conversations are no longer possible. You`re at sea, leaving the Underworld behind.').
                sea := gameState getLocation: 'underworld_sea'.
                gameState setCurrentLocation: sea.
                sea setAccess: false.
            ].

            message
        ].
        character := NPC new.
        character name: 'agamemnon' dialogues: dialogues.

        ^character
    ]

    CharacterPopulation class >> createAchilles [
        | character dialogues |
        dialogues := Dictionary new.
        dialogues at: 'underwold_chat' put: [ :gameState |
            gameState setLongerUnderworldStay: true.

            #('Achilles, the great warrior, speaks to you with fiery intensity:'
            '"Odysseus, your journey is long, and I know the struggles you face. The gods play cruel games with you.'
            'But you must know, I feel no peace here in the Underworld. The thought of my death haunts me still."'
            'Achilles decides to speak to Charon to extend your stay. You now have time to speak to both your mother and Agamemnon for any wisdom they can provide.').
        ].
        character := NPC new.
        character name: 'achilles' dialogues: dialogues.

        ^character
    ]
]