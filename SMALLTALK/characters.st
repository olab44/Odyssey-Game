Object subclass: NPC [
    | name dialogues |

    NPC class >> new [
        ^ super new init
    ]

    init [
        name := ''.
        dialogues := Dictionary new.
    ]

    NPC >> name: aName dialogues: aDialogues [
        name := aName.
        dialogues := aDialogues.
    ]

    talk: gameState [
        | dialogue |
        dialogue := dialogues at: (gameState getCurrentLocation getName) ifAbsent: [nil].

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

        character := NPC new.
        character name: 'circe' dialogues: dialogues.

        ^character
    ]

    CharacterPopulation class >> createHermes [
        | character dialogues |

        dialogues := Dictionary new.
        dialogues at: 'circe_island' put: [ :gameState |
            gameState addItemToEq: 'magic_herb'.
            #('Hermes, appearing like a ghostly figure, approaches with wisdom and a gift - a magical herb.'
            'He offers it to you, saying it will protect you from Circe`s spells. You may need it if you choose to confront her.').
        ].
        character := NPC new.
        character name: 'hermes' dialogues: dialogues.

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