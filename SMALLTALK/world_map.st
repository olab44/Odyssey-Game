Object subclass: Location [
    | name description objects characters directions |

    Location class >> new [
        ^ super new init
    ]

    init [
        name := ''.
        description := #().
        objects := OrderedCollection new.
        characters := OrderedCollection new.
        directions := Dictionary new.
    ]

    Location >> name: aName description: aDescription [
        name := aName.
        description := aDescription.
    ]

    Location >> characters: aCharacters [
        characters := aCharacters.
    ]

    "Add a direction to the location"
    addDirection: aDirection toLocation: aLocation [
        directions at: aDirection put: aLocation.
    ]

    "Get the location in a specific direction"
    getDirection: aDirection [
        ^directions at: aDirection ifAbsent: [nil].
    ]

    getDescription [
        ^description
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

        location := Location new.
        location name: 'circe_sea' description: #('The waters here feel thick with enchantment, and Circe`s island lies ominously ahead.').

        location addDirection: 'disembark' toLocation: 'circe_island'.
        location addDirection: 'north' toLocation: 'giants_sea'.
        location addDirection: 'east' toLocation: 'sirens_sea'.
        location addDirection: 'west' toLocation: 'underworld_sea'.

        ^location
    ]

    WorldMap class >> createCirceIsland [
        | location |

        location := Location new.
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
        location := Location new.
        location name: 'giants_sea' description: #('You have entered the territory of dangerous giants! They begin hurling massive stones at your ships.').
        ^location
    ]

    WorldMap class >> createScyllaCharybdisSea [
        | location |
        location := Location new.
        location name: 'scylla_charybdis_sea' description: #('Scylla / Charybdis').
        ^location
    ]

    WorldMap class >> createSirensSea [
        | location |
        location := Location new.
        location name: 'sirens_sea' description: #('Sirens').
        location addDirection: 'north' toLocation: 'giants_sea'.
        location addDirection: 'south' toLocation: 'scylla_charybdis_sea'.
        ^location
    ]

    WorldMap class >> createSunGodSea [
        | location |
        location := Location new.
        location name: 'sun_god_sea' description: #('Sun god.').
        location addDirection: 'disembark' toLocation: 'sun_god_island'.
        ^location
    ]

    WorldMap class >> createSunGodIsland [
        | location |
        location := Location new.
        location name: 'sun_god_island' description: #('Sun god island.').
        ^location
    ]

    WorldMap class >> createUnderworldSea [
        | location |
        location := Location new.
        location name: 'underworld_sea' description: #('Underworld Sea').
        location addDirection: 'disembark' toLocation: 'underworld'.
        location addDirection: 'east' toLocation: 'circe_sea'.
        ^location
    ]

    WorldMap class >> createUnderworld [
        | location |
        location := Location new.
        location name: 'underworld' description: #('Underworld').
        location addDirection: 'embark' toLocation: 'underworld_sea'.
        ^location
    ]
]