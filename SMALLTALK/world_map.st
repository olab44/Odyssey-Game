Object subclass: Location [
    | access type name description objects directions onEnterFunc blockedAccessFunc |

    Location class >> new [
        ^ super new init
    ]

    init [
        access := true.
        type := ''.
        name := ''.
        description := #().
        objects := OrderedCollection new.
        directions := Dictionary new.
        onEnterFunc := [ :gameState |
            gameState setCurrentLocation: self.
            description
        ].
        blockedAccessFunc := [
            #('You either find nothing of note or the way`s impassable. You end up turning back.')
        ]
    ]
    Location >> name: aName description: aDescription [
        name := aName.
        description := aDescription.
    ]
    Location >> onEnterFunc: aFunc [ onEnterFunc := aFunc. ]
    Location >> blockedAccessFunc: aFunc [ blockedAccessFunc := aFunc. ]

    setAccess: isAccessible [ access := isAccessible. ]

    addDirection: aDirection toLocation: aLocation [
        directions at: aDirection put: aLocation.
    ]
    getDirection: aDirection [
        ^directions at: aDirection ifAbsent: [nil].
    ]

    getName [ ^name ]
    getType [ ^type ]
    getDescription [ ^description ]

    addItem: itemName [
        objects add: itemName.
    ]

    removeItem: itemName [
        ^ objects remove: itemName ifAbsent: [ nil ].
    ]

    getItemNamed: itemName [
        ^ objects detect: [ :item | item = itemName ] ifNone: [ nil ].
    ]

    onEnter: aGameState [
        (access) ifTrue: [ ^ onEnterFunc value: aGameState ].
        ^ blockedAccessFunc value
    ]
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
        locations at: 'open_sea' put: self createOpenSea.
        locations at: 'lotus_sea' put: self createLotusSea.
        locations at: 'lotus_island' put: self createLotusIsland.
        locations at: 'polyphemus_sea' put: self createPolyphemusSea.
        locations at: 'polyphemus_cave' put: self createPolyphemusCave.
        locations at: 'ithaca_sea' put: self createIthacaSea.
        locations at: 'aeolus_island' put: self createAeolusIsland.
        locations at: 'circe_sea' put: self createCirceSea.
        locations at: 'circe_island' put: self createCirceIsland.
        locations at: 'giants_sea' put: self createGiantsSea.
        locations at: 'sirens_sea' put: self createSirensSea.
        locations at: 'scylla_charybdis_sea' put: self createScyllaCharybdisSea.
        locations at: 'sun_god_sea' put: self createSunGodSea.
        locations at: 'sun_god_island' put: self createSunGodIsland.
        locations at: 'underworld_sea' put: self createUnderworldSea.
        locations at: 'underworld' put: self createUnderworld.
        locations at: 'sun_god_storm' put: self createSunGodStorm.
        locations at: 'calypso_island' put: self createCalypsoIsland.
        locations at: 'ithaca' put: self createIthaca.
        ^locations
    ]

    WorldMap class >> createOpenSea [
        | location |
        location := Sea new.
        location name: 'open_sea' description: #('Open sea stretches in all directions, no land in sight.').

        location onEnterFunc: [ :gameState |
            | description |

            (gameState aeolusAccess) ifTrue: [
                location addDirection: 'disembark' toLocation: 'aeolus_island'.

                (gameState hasItem: 'wind-bag') ifTrue: [
                    gameState setCurrentLocation: location.
                    description := #('Calm open sea and the island of a god, nothing more for you here.'
                                    'You instinctively look towards north, where the sky''s a clear expanse of blue.').
                ] ifFalse: [
                    gameState setCurrentLocation: location.
                    description := #('Where once was nothing but water, now lies an island, floating on the waves.'
                                    'The blowing winds feel different, too, and you can''t stop the thought -'
                                    'the home of the wind god. It means hope, a lone chance of getting help'
                                    'against the storm, if only you disembark and beg.').
                ].
            ] ifFalse: [
                gameState setCurrentLocation: location.
                description := #('Open sea stretches in all directions, no land in sight.'
                                'Were you to believe your charts, Ithaca lies north.'
                                'North, where you can see a mass of dark clouds covering the sky.').
            ]
        ].

        location addDirection: 'north' toLocation: 'ithaca_sea'.
        location addDirection: 'west' toLocation: 'lotus_sea'.
        location addDirection: 'act_iii' toLocation: 'calypso_island'.

        ^location
    ]


    WorldMap class >> createLotusSea [
        | location |

        location := Sea new.
        location name: 'lotus_sea' description: #(
            'There is solid land nearby - you notice the glowing light of a fire. It seems inviting.'
        ).

        location addDirection: 'disembark' toLocation: 'lotus_island'.
        location addDirection: 'east' toLocation: 'open_sea'.
        location addDirection: 'north' toLocation: 'polyphemus_sea'.

        ^location
    ]

    WorldMap class >> createLotusIsland [
        | location |

        location := Land new.
        location name: 'lotus_island' description: #(
            'The island is calm and serene, the whole atmosphere making you sleepy. You see a lake'
            'that is surrounded by plain houses. The people milling around pay you no mind at all,'
            'their thoughts far away.'
            'There is plenty of food on their tables - strange, glowing fruits, which you recognize as'
            'mind-numbing lotus. There is also an abundance of wine - surely the lotus-eaters would not'
            'be mad, were you to take a jug for yourself.'
        ).

        location addItem: 'wine'.
        location addDirection: 'embark' toLocation: 'lotus_sea'.

        ^location
    ]


    WorldMap class >> createPolyphemusSea [
        | location |

        location := Sea new.
        location name: 'polyphemus_sea' description: #(
        'You are not far from an island, rugged shoreline making way to green fields and rocky mountains'
        'full of caves. One cave in particular looks easy to reach.'
        ).

        location addDirection: 'disembark' toLocation: 'polyphemus_cave'.
        location addDirection: 'east' toLocation: 'ithaca_sea'.
        location addDirection: 'south' toLocation: 'lotus_sea'.

        ^location
    ]

    WorldMap class >> createPolyphemusCave [
        | location |
        location := Land new.
        location name: 'polyphemus_cave' description: ''.

        location addDirection: 'embark' toLocation: 'polyphemus_sea'.

        location onEnterFunc: [:gameState |
            | name hasWine currentCrew loss newCrewCount message aeolusAccess |

            Transcript show: 'You walk a long while, far into the cave, when a deep voice echoes through the darkness.'; cr.
            Transcript show: 'Who are you? What are you doing, breaking into the house of Polyphemus?'; cr.
            Transcript show: 'A single, massive eye opens behind you, glowing in the light of your torch. Polyphemus does not look happy as he waits for an answer.'; cr.
            Transcript show: 'What is your name, stranger?'; cr.
            name := FileStream stdin nextLine.
            hasWine := gameState hasItem: 'wine'.
            currentCrew := gameState getCrewCount.
            gameState aeolusAccess: true.
            gameState setCurrentLocation: location.

            hasWine ifTrue: [
                #('At the last second, you grab the flask of wine taken from the island of lotus-eaters and aim for the cyclops'' still open mouth.'
                'The wine from lotus flowers makes his thoughts and movements sluggish. He manages to land only a few blows.')
                    do: [:msg | Transcript show: msg; cr.].
                gameState removeItemFromEq: 'wine'.
                loss := 6.
            ] ifFalse: [
                #('The fight is long and grueling, with much death on your side. Pools of fresh blood form on the cave floor.')
                    do: [:msg | Transcript show: msg; cr.].
                loss := 44.
            ].

            (name = 'nobody') ifTrue: [
                #('But no one comes to his aid. It is your opportunity to leave the cave, embark on a ship and get the hell away.'
                'You sense that something in the air has changed, you should scan the see once more')
            ] ifFalse: [
                #('Then there is a sound of heavy steps coming from the direction of the only exit.'
                'There are more of them. Much, much more. You are not leaving this cave alive.')
                gameState setIsGameOver: true.
            ].
        ].

        ^location
    ]


    WorldMap class >> createIthacaSea [
        | location |

        location := Sea new.
        location name: 'ithaca_sea' description: #('Ithaca has never been closer in the last ten years and the way home is through').

        location addDirection: 'south' toLocation: 'open_sea'.
        location addDirection: 'west' toLocation: 'polyphemus_sea'.

        location onEnterFunc: [ :gameState |
            | description |

            (gameState hasItem: 'wind-bag') ifTrue: [
                gameState setCurrentLocation: location.
                description := #('The sky is clear, the water smooth - no storm, no tidal wave.'
                                'You see Ithaca - your destination, your kingdom, your home - on the horizon.'
                                'You can already feel the ghost of your wifes embrace.').
            ] ifFalse: [
                gameState setCurrentLocation: location.
                description := #('Your way is blocked by giant waves and giant storms.'
                                'You should probably turn back south towards calmer waters -'
                                'but Ithaca has never been closer in the last ten years, and the way home is through.').
            ].

        ].

        ^location
    ]



    WorldMap class >> createAeolusIsland [
        | location |

        location := Land new.
        location name: 'aeolus_island' description: #(
            'The home of the wind god, Aeolus, is just as unique as its owner might suggest. Loud and playful'
            'with various puffs of clouds twisting and turning in the air, as if alive.'
            'Aeolus himself flies around as well, never in one place for long. You feel his gaze following you.'
        ).
        location addItem: 'wind-bag'.
        location addDirection: 'embark' toLocation: 'open_sea'.

        ^location
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
                    'You might be able to spot and talk to Hermes around here sometimes. Confront Circe, the enchantress, by talking to her directly.'
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
            gameState setCurrentLocation: location.

            currentCrew := gameState getCrewCount.
            loss := (currentCrew * 0.2) rounded.
            loss := loss max: 20.
            message := location getDescription.
            message := message, #('The giants` fierce attack is quick to reduce the numbers of your crew. You have no choice but to retreat.').
            message := message, (gameState killCrew: loss).

            gameState move: 'south'.
            message
        ].
        location addDirection: 'south' toLocation: 'circe_sea'.

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
        | choice location |
        location := Sea new.
        location name: 'scylla_charybdis_sea' description: #('The sea grows treacherous as you approach the domain of Scylla and Charybdis.').
        location addDirection: 'south' toLocation: 'sun_god_sea'.
        location onEnterFunc: [ :gameState |
            | choice message randomRate loss |
            gameState setCurrentLocation: location.

            Transcript show: 'The sea grows treacherous as you approach the domain of Scylla and Charybdis.'; cr.
            Transcript show: 'To your left, you see Scylla`s ominous cliffs, while Charybdis churns the water violently to your right.'; cr.
            Transcript show: 'You must choose which path to take to continue - Scylla or Charybdis?'; cr.

            choice := ''.
            [ choice = 'scylla' or: [choice = 'charybdis'] ] whileFalse: [
                Transcript show: 'Your choice now is only between Scylla and Charybdis.'; cr.
                Transcript show: '|> '.
                choice := FileStream stdin nextLine.
            ].

            choice = 'scylla' ifTrue: [
                message := #('You approach Scylla...').
                randomRate := Random new next.
                (randomRate > gameState getScyllaSurvivalRate) ifTrue: [
                    message := message, #('Scylla strikes with terrifying speed, catching you off guard.' 'You have been taken.').
                    gameState setIsGameOver: true
                ] ifFalse: [
                    message := message, #('You sail past Scylla successfully, but she manages to claim some of your crew.').
                    loss := (6 max: (0.06 * gameState getCrewCount) rounded) min: gameState getCrewCount.
                    gameState killCrew: loss
                ].
            ].

            choice = 'charybdis' ifTrue: [
                (gameState hasItem: 'charybdis_lure') ifTrue: [
                    message := #('You use the mysterious Charybdis Lure, guiding your ship safely past the deadly whirlpool.'
                        'The waters calm, and you find yourselves out of danger.').
                ] ifFalse: [
                    message := #('The whirlpool`s powerful currents pull your ship into its deadly grasp.'
                        'Your ship and crew are lost to Charybdis.').
                    gameState setIsGameOver: true
                ].
            ].

            (gameState getIsGameOver) ifFalse: [
                gameState move: 'south'.
                message := message, #('After surviving the perilous pass, a fierce storm catches you off guard.'
                'The raging waves drive your ship eastward, and you find yourselves on the shores of the Island of the Sun God.').
            ].

            message
        ].
        ^location
    ]

    WorldMap class >> createUnderworldSea [
        | location |
        location := Sea new.
        location name: 'underworld_sea' description: #('You have reached the mysterious and eerie waters of the Underworld.').
        location setAccess: false.
        location blockedAccessFunc: [ #('The path to the Underworld is blocked. You need access to the Underworld to sail west from Circe`s Sea.') ].
        location addDirection: 'disembark' toLocation: 'underworld'.
        location addDirection: 'east' toLocation: 'circe_sea'.
        location addDirection: 'north' toLocation: 'giants_sea'.
        ^location
    ]

    WorldMap class >> createUnderworld [
        | location |
        location := Land new.
        location name: 'underworld' description: #('This land confuses your mind. You barely sense a presence of Charon, the ferryman who can guide you across the River Styx.').
        location addDirection: 'embark' toLocation: 'underworld_sea'.
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
        location addDirection: 'embark' toLocation: 'sun_god_storm'.
        ^location
    ]

    WorldMap class >> createSunGodStorm [
        | location |
        location := Sea new.
        location setAccess: false.
        location name: 'sun_god_storm' description: #('The storm rages on.').
        location onEnterFunc: [ :gameState |
            (gameState hasItem: 'strength_elixir') ifTrue: [
                gameState setCurrentLocation: location.
                gameState move: 'west'.
                #('As you leave the island, a thunderous voice echoes: "For your desecration, you shall be punished!"'
                'The sea rages as a powerful storm descends upon your ship.'
                'You see all of your crew dying, then you lose consciousness yourself.')
            ] ifFalse: [
                gameState setIsGameOver: true.
                #('As you sail eastward, the wrath of the gods descends upon you.'
                'A powerful storm engulfs your ship, smashing it to pieces.'
                'If only you had something, a magic spell or a potion, to give you strength...'
                'You have perished at sea due to the wrath of the gods.')
            ]
        ].
        location blockedAccessFunc: [ #('The storm rages on, you have no means to embark on a ship now.') ].
        location addDirection: 'west' toLocation: 'calypso_island'.
        ^location
    ]

    WorldMap class >> createCalypsoIsland [
        | location |
        location := Land new.
        location name: 'calypso_island'
            description: #(
                'You stand on the shores of an island. It is breathtaking but empty, nobody is to be seen.'
                'The only sound is wind blowing through the trees. You are exhausted from all your travels and collapse on the ground.'
                'Your crew is no longer alive, your ship is gone. You are left all alone.'
                'When you wake up, the first thing you see is a woman, a goddess, who introduces herself as Calypso.'
                'She is beautiful, but you want to return home. You can try asking her how to leave.'
            ).
        ^location
    ]

    WorldMap class >> createIthaca [
        | location |
        location := Land new.
        location name: 'ithaca'
            description: #('no siema').
        location onEnterFunc: [ :gameState |
            gameState setIsGameOver: true.
            gameState setCurrentLocation: location.
            Transcript show: 'At last, you set foot on Ithaca, your homeland.'; cr.
            Transcript show: 'After all the trials, you`ve made it home, where every person on the street talks about the queen`s challenge.'; cr.
            Transcript show: 'But that`s a story for another day.'; cr.
            Transcript show: 'After years lost at sea, battles fought, and gods defied, you`ve finally reached Ithaca.'; cr.
            Transcript show: 'You`ve proven that courage and loyalty can overcome even the wrath of gods.'; cr.
            Transcript show: 'Welcome home, Odysseus.'; cr.
        ].
        ^location
    ]
]