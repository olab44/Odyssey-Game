Object subclass: Game [
    | gameState |

    Game class >> new [
        ^ super new init
    ]

    init [
        gameState := GameState new.
    ]

    startingText := #(
        'The city of Troy has been seized, the war won. Ten years away from home are finally coming to'
        'an end. You''re on your way to Ithaca now, hundreds of sea miles both behind and ahead of you.'
        'The problem''s not the distance, you know very well, but the dangers that lie in between'
        '- you''ll have to act smart to get back to your homeland.'
        'You think of your wife. You think of your little boy, who''s probably not so little anymore.'
        'Failure is not an option.'
        'With trusted crew by your side, the six hundred men under your command, there just might be'
        'hope for you after all.'
        'You should look around, talk to them while it''s not busy or sail to cross the sea and reach home.'
    ).

    "Print strings from array in separate lines."
    printLines: anArray [
        anArray do: [ :line | Transcript show: line; cr. ]
    ]

    printIntroduction [
        | green yellow reset |
        green := ('%1[32m' % #($<16r1B>)).  "ANSI escape code for green"
        yellow := ('%1[33m' % #($<16r1B>)). "ANSI escape code for yellow"
        reset := ('%1[0m' % #($<16r1B>)).   "Reset the color"

        "Print the text with colors"
            Transcript show: green; cr.
            Transcript show: '---------------------- THE ODYSSEY : THE GAME ----------------------'; cr.
            Transcript show: yellow; cr.
            Transcript show: '                                                    _  _                '; cr.
            Transcript show: '                                                   ` \/ `              '; cr.
            Transcript show: '   _  _                          <|                                     '; cr.
            Transcript show: '    \/                  _\`_______\`__________\`___                      '; cr.
            Transcript show: '                      /\`                     //                        '; cr.
            Transcript show: '                    /\`                      `/           _  _          '; cr.
            Transcript show: '                   |                       `/ \            \/         '; cr.
            Transcript show: '                   ||                      ||  \                       '; cr.
            Transcript show: '                  /`|                      `\   \                     '; cr.
            Transcript show: '        _==_     /  \                       `\   \                  '; cr.
            Transcript show: '       //  //   /    `\                       \\  \                 '; cr.
            Transcript show: '       \\      /       \\                      \\  \             '; cr.
            Transcript show: '        \ \   /         `\\______________________\  \        ^%%  '; cr.
            Transcript show: '         `\ \/            /       | |            \   \/   ^%= ^%%=  '; cr.
            Transcript show: '           \\ \\_______ _/_____ __|_|__ __________\_/||   ^^     %^ '; cr.
            Transcript show: '            `\\|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_|_/|/         %=%    '; cr.
            Transcript show: ' =_  %%!_   __\    ________________                /   __.%% !^%% _^   '; cr.
            Transcript show: '^^%%^=%^=^^^%%^^\`/_)/_)_/_)__)/_)/)/)_)\\_`_`___//)/)/)/)%%=%^^^%^^    '; cr.
            Transcript show: '   =^=!%%=^^^!^^^!!^^^%%=%%!!!!^^^%%^^^!!%%%=^^^!!!%%!!%%%^^^^%^%       '; cr.
            Transcript show: green, '--------------------------------------------------------------------'; cr.
            Transcript show: '', reset; cr.
    ]

    printInstructions [
        | green yellow reset |
        green := ('%1[32m' % #($<16r1B>)).  "ANSI escape code for green"
        yellow := ('%1[33m' % #($<16r1B>)). "ANSI escape code for yellow"
        reset := ('%1[0m' % #($<16r1B>)).   "Reset the color"

        Transcript show: green, '--------------------------------------------------------------------', reset; cr.
        Transcript show: 'AVAILABLE COMMANDS:'; cr.
        Transcript show: 'In all acts, you can use:'; cr.
        Transcript show: '- count crew : to check how many crew members are still alive'; cr.
        Transcript show: '- look : To observe your surroundings.'; cr.
        Transcript show: '- talk <person> : To interact with characters.'; cr.
        Transcript show: '- embark : To board your ship'; cr.
        Transcript show: '- disembark : To leave your ship and explore on land.'; cr.
        Transcript show: '- sail <direction> : to sail to another location'; cr.
        Transcript show: ''; cr.
        Transcript show: 'In Act I, you can additionally use:'; cr.
        Transcript show: '- take <object> : To collect items you find.'; cr.
        Transcript show: ''; cr.
        Transcript show: 'In Act III, you can additionally use:'; cr.
        Transcript show: '- gather <material> : To gather essential materials for crafting.'; cr.
        Transcript show: '- build <material> : To construct a raft or other items needed to progress.'; cr.
        Transcript show: '- escape : To escape if you have completed the raft.'; cr.
        Transcript show: ''; cr.
        Transcript show: 'Good luck on your journey, brave Odysseus!'; cr.
        Transcript show: yellow, 'Type "start" to play the game or type "finish" anytime to close it.'; cr.
        Transcript show: green, '--------------------------------------------------------------------', reset; cr.
        Transcript show: ''
    ]

    printFinish [
        | green yellow reset |
        green := ('%1[32m' % #($<16r1B>)).  "ANSI escape code for green"
        yellow := ('%1[33m' % #($<16r1B>)). "ANSI escape code for yellow"
        reset := ('%1[0m' % #($<16r1B>)).   "Reset the color"

        Transcript show: green, '--------------------------------------------------------------------'; cr.
        Transcript show: yellow,'                      Thank you for playing!                       '; cr.
        Transcript show: green, '--------------------------------------------------------------------', reset; cr.
    ]


    readCommand [
        Transcript show: '> '.
        ^ FileStream stdin nextLine
    ]

    "Run the game."
    run [
        | cmd isUnknown |

        self printIntroduction.
        self printInstructions.

        "Game loop."
        [ gameState getIsGameOver ] whileFalse: [

            isUnknown := true.
            cmd := self readCommand.

            cmd = 'start' ifTrue: [
                self printLines: startingText.
                isUnknown := false
            ].

            cmd = 'look' ifTrue: [
                self printLines: (gameState getCurrentLocation getDescription).
                isUnknown := false
            ].

            (cmd startsWith: 'sail') ifTrue: [
                self printLines: (gameState move: (cmd copyFrom: 6 to: cmd size)).
                isUnknown := false
            ].

            cmd = 'disembark' ifTrue: [
                self printLines: (gameState move: 'disembark').
                isUnknown := false
            ].

            cmd = 'embark' ifTrue: [
                self printLines: (gameState move: 'embark').
                isUnknown := false
            ].

            (cmd startsWith: 'talk') ifTrue: [
                self printLines: (gameState talk: (cmd copyFrom: 6 to: cmd size)).
                isUnknown := false
            ].

            cmd = 'start challenge' ifTrue: [
                self printLines: (gameState startChallenge).
                isUnknown := false
            ].

            cmd = 'complete the elixir' ifTrue: [
                self printLines: (gameState completeElixir).
                isUnknown := false
            ].

            cmd = 'count crew' ifTrue: [
                Transcript show: 'You have ', (gameState getCrewCount asString), ' men remaining.'; cr.
                isUnknown := false
            ].

            (cmd startsWith: 'take') ifTrue: [
                | itemName |
                itemName := cmd copyFrom: 6 to: cmd size.
                self printLines: { (gameState take: itemName) }.
                isUnknown := false
            ].

            cmd = 'show map' ifTrue: [
                self printLines: (gameState showMap).
                isUnknown := false
            ].

            cmd = 'instructions' ifTrue: [
                self printInstructions.
                isUnknown := false
            ].

            cmd = 'finish' ifTrue: [
                gameState setIsGameOver: true.
                isUnknown := false
            ].

            (cmd startsWith: 'gather') ifTrue: [
                | materialName |
                materialName := cmd copyFrom: 8 to: cmd size.
                Transcript show: 'Gathering material: ', materialName; cr. "Diagnostyka"
                
                "Wywołanie metody gather"
                gameState gather: materialName.
                isUnknown := false.
            ].

            (cmd startsWith: 'build') ifTrue: [
                | materialName |
                materialName := cmd copyFrom: 7 to: cmd size.  "Extract material name from the command"
                gameState build: materialName.  "Call the build: method"
                isUnknown := false.
            ].

            cmd = 'escape' ifTrue: [
                gameState escape.
                isUnknown := false
            ].

            isUnknown ifTrue: [
                self printLines: #('You cannot do that.')
            ]
        ].

        self printFinish.
    ]
]

Game new run.
