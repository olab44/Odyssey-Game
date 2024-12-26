Object subclass: Location [
    | type name description objects characters directions onEnterFunc |

    Location class >> new [
        ^ super new init
    ]

    init [
        type = ''.
        name := ''.
        description := #().
        objects := OrderedCollection new.
        characters := OrderedCollection new.
        directions := Dictionary new.
        onEnterFunc := [ :gameState |
            gameState setCurrentLocation: self.
            description
        ]
    ]

    Location >> name: aName description: aDescription [
        name := aName.
        description := aDescription.
    ]
    Location >> characters: aCharacters [
        characters := aCharacters.
    ]
    Location >> onEnterFunc: aFunc [
        onEnterFunc := aFunc.
    ]

    addDirection: aDirection toLocation: aLocation [
        directions at: aDirection put: aLocation.
    ]
    getDirection: aDirection [
        ^directions at: aDirection ifAbsent: [nil].
    ]

    getType [ ^type ]
    getDescription [ ^description ]

    onEnter: aGameState [ ^ onEnterFunc value: aGameState ]
]
Location subclass: Sea [
    Sea class >> new [
        ^super new initSea
    ]
    initSea [
        type := 'sea'.
    ]
]
Location subclass: Land [
    Land class >> new [
        ^super new initLand
    ]
    initLand [
        type := 'land'.
    ]
]

Object subclass: WorldMap [

    WorldMap class >> createWorldMap [
        | locations |
        locations := Dictionary new.
        locations at: 'circe_sea' put: self createCirceSea.
        locations at: 'circe_island' put: self createCirceIsland.
        locations at: 'giants_sea' put: self createGiantsSea.
        locations at: 'scylla_charybdis_sea' put: self createScyllaCharybdisSea.
        locations at: 'sirens_sea' put: self createSirensSea.
        locations at: 'sun_god_sea' put: self createSunGodSea.
        locations at: 'sun_god_island' put: self createSunGodIsland.
        locations at: 'underworld_sea' put: self createUnderworldSea.
        locations at: 'underworld' put: self createUnderworld.
        ^locations
    ]

    WorldMap class >> createCirceSea [
        | location |

        location := Sea new.
        location name: 'circe_sea' description: #('The waters here feel thick with enchantment, and Circe`s island lies ominously ahead.').

        location addDirection: 'disembark' toLocation: 'circe_island'.
        location addDirection: 'north' toLocation: 'giants_sea'.
        location addDirection: 'east' toLocation: 'sirens_sea'.
        location addDirection: 'west' toLocation: 'underworld_sea'.

        ^location
    ]

    WorldMap class >> createCirceIsland [
        | location |

        location := Land new.
        location name: 'circe_island'
                description: #(
                    'After disembarking on Circe`s island, you step into a lush, enchanted forest. The air is thick with mystery, and your instincts warn you of hidden dangers.'
                    'To continue, you can talk with Hermes first or confront Circe, the enchantress, by talking to her directly.'
                    ).
        location addDirection: 'embark' toLocation: 'circe_sea'.

        ^location
    ]

    WorldMap class >> createGiantsSea [
        | location |
        location := Sea new.
        location name: 'giants_sea' description: #('You have entered the territory of dangerous giants! They begin hurling massive stones at your ships.').
        location onEnterFunc: [ :gameState |
            | currentCrew loss newCrewCount message |

            currentCrew := gameState getCrewCount.
            loss := (currentCrew * 0.2) rounded.
            loss := loss max: 20.
            message := location getDescription.
            message := message, #('The giants` fierce attack is quick to reduce the numbers of your crew. You have no choice but to retreat.').
            message := message, (gameState killCrew: loss).

            message
        ].

        ^location
    ]

    WorldMap class >> createScyllaCharybdisSea [
        | location |
        location := Sea new.
        location name: 'scylla_charybdis_sea' description: #('Scylla / Charybdis').
        ^location
    ]

    WorldMap class >> createSirensSea [
        | location |
        location := Sea new.
        location name: 'sirens_sea' description: #('Sirens').
        location addDirection: 'north' toLocation: 'giants_sea'.
        location addDirection: 'south' toLocation: 'scylla_charybdis_sea'.
        ^location
    ]

    WorldMap class >> createSunGodSea [
        | location |
        location := Sea new.
        location name: 'sun_god_sea'
            description: #(
                'A violent storm catches your ship, forcing you to seek refuge on a nearby island.'
                'This is the sacred island of the Sun God, Helios, where his holy cattle roam.'
            ).
        location addDirection: 'disembark' toLocation: 'sun_god_island'.
        ^location
    ]

    WorldMap class >> createSunGodIsland [
        | location |
        location := Land new.
        location name: 'sun_god_island'
            description: #(
                'You disembark onto the island, hoping the storm will soon pass.'
                'Unfortunately, the storm shows no sign of stopping, and you will need to stay here longer.'
                'Your food supplies are exhausted, and the crew is looking to you for guidance.'
                'You had better talk to them for advice.'
            ).
        ^location
    ]

    WorldMap class >> createUnderworldSea [
        | location |
        location := Sea new.
        location name: 'underworld_sea' description: #('Underworld Sea').
        location addDirection: 'disembark' toLocation: 'underworld'.
        location addDirection: 'east' toLocation: 'circe_sea'.
        ^location
    ]

    WorldMap class >> createUnderworld [
        | location |
        location := Sea new.
        location name: 'underworld' description: #('Underworld').
        location addDirection: 'embark' toLocation: 'underworld_sea'.
        ^location
    ]
]