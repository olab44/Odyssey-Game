Object subclass: GameState [
    | isGameOver worldMap characterPopulation currentLocation equipment crewCount aeolusAccess crewProtected scyllaSurvivalRate underworldChallenge longerUnderworldStay|

    GameState class >> new [
        ^ super new init
    ]

    init [
        isGameOver := false.
        worldMap := WorldMap createWorldMap.
        characterPopulation := CharacterPopulation createCharacters.
        currentLocation := worldMap at: 'open_sea'.
        equipment := Dictionary new.
        crewCount := 600.
        aeolusAccess := false.
        crewProtected := 0.
        scyllaSurvivalRate := 0.
        underworldChallenge := UnderworldChallenge new.
        longerUnderworldStay := false.
    ]

    aeolusAccess [
        ^aeolusAccess
    ]

    aeolusAccess: aBoolean [
        aeolusAccess := aBoolean.
    ]



    getIsGameOver [ ^isGameOver ]
    setIsGameOver: aBoolean [ isGameOver := aBoolean ]

    getCurrentLocation [ ^currentLocation ]
    setCurrentLocation: aLocation [ currentLocation := aLocation ]

    getLocation: locationName [ ^ worldMap at: locationName ifAbsent: [nil] ]
    getCharacter: characterName [ ^ characterPopulation at: characterName ifAbsent: [nil] ]

    getEquipment [ ^equipment ]
    hasItem: itemName [
        ^equipment includesKey: itemName
    ]
    addItemToEq: itemName [
        | currentCount |
        currentCount := equipment at: itemName ifAbsent: [ 0 ].
        equipment at: itemName put: currentCount + 1.
    ]

    getCrewCount [ ^ crewCount ]
    killCrew: num [
        crewCount := crewCount - num.
        crewCount <= 0 ifTrue: [
            isGameOver := true.
            ^ #(''
                'That`s when you realize - all of your men... dead. You`re not far behind them.')
        ].
    ]



    getCrewProtected [ ^ crewProtected ]
    setCrewProtected: num [ crewProtected := num ]
    getScyllaSurvivalRate [ ^ scyllaSurvivalRate ]
    setScyllaSurvivalRate: num [ scyllaSurvivalRate := num ]

    getLongerUnderworldStay [ ^longerUnderworldStay ]
    setLongerUnderworldStay: longerStay [ longerUnderworldStay := longerStay ]

    move: aDirection [
        | new_location |

        aDirection = 'embark' ifTrue: [ ^ self embark. ].
        aDirection = 'disembark' ifTrue: [ ^ self disembark. ].

        currentLocation getType = 'land' ifTrue: [ ^ #('You need to embark on a ship first.'). ].

        new_location := worldMap at: (currentLocation getDirection: aDirection) ifAbsent: [nil].

        new_location isNil ifTrue: [
            ^ #('You set sail, but you either find nothing of note in that direction, or the way`s impassable. You end up turning back.').
        ].

        ^ new_location onEnter: self
    ]
    disembark [
        | new_location |
        currentLocation getType = 'land' ifTrue: [ ^ #('You`re already on land.'). ].
        new_location := worldMap at: (currentLocation getDirection: 'disembark') ifAbsent: [nil].
        new_location isNil ifTrue: [
            ^ #('There`s no solid land to disembark on.').
        ].

        ^ new_location onEnter: self
    ]

    embark [
        | new_location |
        currentLocation getType = 'sea' ifTrue: [ ^ #('You`re already on a ship or there`s no ship at all.'). ].
        new_location := worldMap at: (currentLocation getDirection: 'embark') ifAbsent: [nil].
        new_location isNil ifTrue: [
            ^ #('You`re already on a ship or there`s no ship at all.').
        ].

        ^ new_location onEnter: self
    ]

    talk: characterName [
        | character |
        character := characterPopulation at: characterName ifAbsent: [nil].

        (character isNil) ifTrue: [
            ^ #('It`s not a time nor place for a talk with someone who`s busy - or someone who`s not even there.')
        ].
        ^ character talk: self
    ]

    take: itemName [
        | item location |
        
        location := self getCurrentLocation.
        item := location getItemNamed: itemName.
        
        item ifNotNil: [
            self addItemToEq: item.  "Add item to inventory"
            location removeItem: itemName.
            ^ 'You have taken ', itemName
        ].
        
        ^ 'There is no ', itemName, ' here to take.'
    ]



    showMap [

    ]

    startChallenge [
        | loss message |
        (currentLocation getName = 'underworld' | underworldChallenge ) ifTrue: [
            underworldChallenge startChallenge.
            (underworldChallenge getFailedChallenge) ifTrue: [
                loss := (crewCount * 0.2) max: 20.
                message := (self killCrew: loss)
            ].
            (self getCharacter: 'charon') setAvailableDialogue: 'nil' atLocation: 'underworld'.
            (self getCharacter: 'tiresias') setAvailableDialogue: 'prophet_advice' atLocation: 'underworld'.
            ^ #('Charon opens the gates to Hades, and you may now speak with Tiresias.'), message
        ].
        ^ #('No such challenge stands before you here. Perhaps you`ve already completed it?')
    ]

    completeElixir [
        ((self hasItem: 'empty_bottle') and: [self hasItem: 'helios_blood']) ifTrue: [
            self addItemToEq: 'strength_elixir'.
            ^ #('You now possess the Elixir of Strength, a potent potion that may save your life.')
        ].
        ^ #('You don`t have the knowlege or ingredients needed to complete the elixir.')
    ]
]
