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
        locations at: 'sirens_sea' put: self createSirensSea.
        locations at: 'scylla_charybdis_sea' put: self createScyllaCharybdisSea.
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

    WorldMap class >> createSirensSea [
        | location |
        location := Sea new.
        location name: 'sirens_sea'
            description: #(
                'The waters are calm but ominous as you approach the domain of the Sirens.'
                'In the distance, their figures are barely visible, their songs ready to ensnare anyone who listens.'
                'You can sail south to leave them behind or sail back west and try your luck with them once more another time.'
                ).
        location addDirection: 'north' toLocation: 'giants_sea'.
        location addDirection: 'south' toLocation: 'scylla_charybdis_sea'.
        location addDirection: 'west' toLocation: 'circe_sea'.
        location onEnterFunc: [ :gameState |
            | choice message loss |

            gameState setCurrentLocation: location.

            Transcript show: 'The waters are calm but ominous as you approach the domain of the Sirens.'; cr.
            Transcript show: 'In the distance, their figures are barely visible, their songs ready to ensnare anyone who listens.'; cr.
            Transcript show: 'You must now decide whether to block your own ears or not.'; cr.
            Transcript show: 'Will you "plug ears" or "leave ears open" to hear the Sirens` song?'; cr.

            Transcript show: '|> '.
            choice := FileStream stdin nextLine.

            message := #().
            (choice ~= 'leave ears open' and: [choice ~= 'plug ears']) ifTrue: [
                message := #('You take too long and so the crew decides for you.').
                choice := 'plug ears'
            ].

            choice = 'plug ears' ifTrue: [
                message := message, #('As you sail past, you see the Sirens singing, but their voices cannot reach you, for your ears are plugged with wax.').
            ].

            choice = 'leave ears open' ifTrue: [
                message := #('You choose to leave your ears open. Your crew ties you tightly to the mast, as per Circe`s advice.'
                    'The Sirens` voices fill the air, haunting and beautiful.'
                    'You listen, enthralled, and in their song, you learn of a mystical potion recipe that grants strength and protects life.'
                    'You have learned the potion recipe!'
                    '').
                gameState addItemToEq: 'potion_recipe'.
            ].
            loss := gameState getCrewCount - (gameState getCrewProtected min: gameState getCrewCount).

            message := message, #('Not all of your crew remained protected - some of them succumbed to the Sirens` song.'
                    'With the Sirens behind you, you should sail south towards the looming cliffs of Scylla and Charybdis.'
                    'You brace yourself for another challenge as the journey continues.').
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