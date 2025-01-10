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

    aeolusAccess [^aeolusAccess]
    aeolusAccess: aBoolean [aeolusAccess := aBoolean.]


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

    removeItemFromEq: itemName [
        | currentCount |
        currentCount := equipment at: itemName ifAbsent: [ nil ].
        currentCount ifNotNil: [
            currentCount > 1 ifTrue: [
                equipment at: itemName put: currentCount - 1.
            ] ifFalse: [
                equipment removeKey: itemName.
            ].
        ] ifNil: [
            'Item not found in equipment.'.
        ].
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
        | new_location randomDestination crewCount |

        aDirection = 'embark' ifTrue: [ ^ self embark. ].
        aDirection = 'disembark' ifTrue: [ ^ self disembark. ].

        currentLocation getType = 'land' ifTrue: [ 
            ^ #('You need to embark on a ship first.') 
        ].

        new_location := worldMap at: (currentLocation getDirection: aDirection) ifAbsent: [nil].

        "ithaca sea storm logic"
        (currentLocation getName = 'ithaca_sea' and: [aDirection ~= 'south']) ifTrue: [
            (self hasItem: 'wind-bag') ifTrue: [
                Transcript show: 'With the storm contained, it should not take much longer to reach the coast. Mere weeks.'; cr.
                Transcript show: 'You wish you could relax now, but the wind gods'' words won''t let you. ''Be careful who you'; cr.
                Transcript show: 'trust, captain pushes you to keep your eyes open at all times.'; cr.
                Transcript show: 'You guard the wind-bag to the best of your abilities, but the need to sleep proves stronger eventually.'; cr.
                Transcript show: 'The dream is a lovely one - the family reunited, Penelope and Telemachus in your arms.'; cr.
                Transcript show: 'The reality is not. The bag lies open. The raging storm tears at your ships with vengeance, vicious'; cr.
                Transcript show: 'currents leading you to distant shores far, far away from Ithaca.'; cr.
                self removeItemFromEq: 'wind-bag'.
                new_location := worldMap at: 'circe_sea'.
                ^ new_location onEnter: self.
            ] ifFalse: [
                crewCount := self getCrewCount.
                (crewCount > 150) ifTrue: [
                    Transcript show: 'It turns out you cannot beat the force of nature that easily. You lose ships - the screams of over'; cr.
                    Transcript show: 'na hundred men are drowned out by the storm as they disappear below the waves.'; cr.
                    self killCrew: 150.
                    randomDestination := (1 to: 4) atRandom.
                    new_location := self getRandomLocation: randomDestination.
                    Transcript show: 'You cannot control your course as the currents are too strong. You should look around to see where you have ended up.'; cr.
                    ^ new_location onEnter: self.
                ] ifFalse: [
                    Transcript show: 'The storm is your final fight, the only thing still blocking your way home - and so you are ready'; cr.
                    Transcript show: 'to risk it all, look for the way through even when there seems to be none.'; cr.
                    Transcript show: 'You try your best...'; cr.
                    Transcript show: 'It is just not enough.'; cr.
                    Transcript show: 'Your ship is the last one from the fleet to fall victim to the crashing waves.'; cr.
                    self setIsGameOver: true.
                ].
            ].
        ].

        new_location isNil ifTrue: [
            ^ #('You set sail, but you either find nothing of note in that direction, or the way`s impassable. You end up turning back.')
        ].

        ^ new_location onEnter: self.
    ]

    getRandomLocation: randomDestination [
        | destinations |
        destinations := Array with: (worldMap at: 'lotus_sea')
                                with: (worldMap at: 'ithaca_sea')
                                with: (worldMap at: 'polyphemus_sea')
                                with: (worldMap at: 'open_sea').

        ^ destinations at: randomDestination ifAbsent: [ worldMap at: 'ithaca_sea' ].
    ]

    disembark [
        | new_location |
        currentLocation getType = 'land' ifTrue: [ ^ #('You`re already on land.'). ].
        new_location := worldMap at: (currentLocation getDirection: 'disembark') ifAbsent: [nil].
        new_location isNil ifTrue: [
            ^ #('There`s no solid land to disembark on.').
        ].

        (new_location getName = 'polyphemus_cave' and: [self aeolusAccess]) ifTrue: [
            ^ #('Going back to the Cyclops cave after all that has happened is a suicide. You crew knows it'
            'and refuses to risk it. You should know better, too').
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
        | item location inventory |
        
        location := self getCurrentLocation. 
        item := location getItemNamed: itemName.
        inventory := self getEquipment.  
        (inventory includesKey: itemName) ifTrue: [
            ^ 'You have already taken ', itemName.
        ].
        
        item ifNotNil: [
            self addItemToEq: itemName.
            location removeItem: itemName.
            ^ 'You have taken ', itemName.
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
