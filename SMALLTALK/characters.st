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
        | character |

        character := NPC new.
        character name: 'crew' dialogues: Dictionary new.

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
        | character |

        character := NPC new.
        character name: 'charon' dialogues: Dictionary new.
        ^character
    ]

    CharacterPopulation class >> createTiresias [
        | character |

        character := NPC new.
        character name: 'tiresias' dialogues: Dictionary new.
        ^character
    ]

    CharacterPopulation class >> createMother [
        | character |

        character := NPC new.
        character name: 'mother' dialogues: Dictionary new.
        ^character
    ]

    CharacterPopulation class >> createAgamemnon [
        | character |

        character := NPC new.
        character name: 'agamemnon' dialogues: Dictionary new.

        ^character
    ]

    CharacterPopulation class >> createAchilles [
        | character |

        character := NPC new.
        character name: 'achilles' dialogues: Dictionary new.

        ^character
    ]
]