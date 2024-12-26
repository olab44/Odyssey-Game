Object subclass: Character [
    | name dialogues |

    Character class >> new [ ^ super new init ]

    init [
        name := ''.
        dialogues := Dictionary new.
    ]

    name: aName dialogues: aDialogues locations: aLocations [
        name := aName.
        dialogues := aDialogues.
    ]

    getDialogue: gameState [

    ]
]

Object subclass: CharactersPopulation [

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

    CharacterPopulation class >> createCirce [
        | character dialogues |

        character := Character new.
        character name: 'circe' dialogues: dialogues locations: locations.

        ^character
    ]

    WorldMap class >> createAchilles [
        | character dialogues |

        character := Character new.
        character name: 'achilles' dialogues: #(

        )

        ^character
    ]

    WorldMap class >> createCirce [
        | character dialogues |

        character := Character new.
        character name: 'circe' dialogues: dialogues locations: locations.

        ^character
    ]

]