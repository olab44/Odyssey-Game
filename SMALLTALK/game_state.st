Object subclass: GameState [
    | isGameOver worldMap currentLocation equipment disembarked |

    GameState class >> new [
        ^ super new init
    ]

    init [
        isGameOver := false.
        worldMap := WorldMap createWorldMap.
        currentLocation := worldMap at: 'circe_sea'.
        equipment := Dictionary new.
        disembarked := false.
    ]

    getIsGameOver [ ^isGameOver ]
    setIsGameOver: aBoolean [ isGameOver := aBoolean ]

    getCurrentLocation [ ^currentLocation ]
    setCurrentLocation: aLocation [ currentLocation := aLocation ]

    getEquipment [ ^equipment ]
    addItemToEq: anItem [ equipment at: (anItem getName) put: anItem ]

    move: aDirection [
        | new_location |

        aDirection = 'embark' ifTrue: [ ^ self embark. ].
        aDirection = 'disembark' ifTrue: [ ^ self disembark. ].
        disembarked ifTrue: [ ^ #('You need to embark on a ship first.'). ].

        new_location := worldMap at: (currentLocation getDirection: aDirection) ifAbsent: [nil].

        new_location isNil ifTrue: [
            ^ #('You set sail, but you either find nothing of note in that direction, or the way`s impassable. You end up turning back.').
        ].

        currentLocation := new_location.
        ^ new_location getDescription
    ]

    disembark [
        | new_location |
        disembarked ifTrue: [ ^ #('You`re already on land.'). ].
        new_location := worldMap at: (currentLocation getDirection: 'disembark') ifAbsent: [nil].
        new_location isNil ifTrue: [
            ^ #('There`s no solid land to disembark on.').
        ].

        disembarked := true.
        currentLocation := new_location.
        ^ new_location getDescription
    ]
    embark [
        | new_location |
        new_location := worldMap at: (currentLocation getDirection: 'embark') ifAbsent: [nil].
        new_location isNil ifTrue: [
            ^ #('You`re already on a ship or there`s no ship at all.').
        ].

        disembarked := false.
        currentLocation := new_location.
        ^ new_location getDescription
    ]

]
