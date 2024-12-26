Object subclass: UnderworldChallenge [
    | leftBank rightBank isCompleted failedChallenge |

    UnderworldChallenge class >> new [
        ^ super new init
    ]

    init [
        leftBank = #('crew' 'wine' 'cerberus' 'charon').
        rightBank = #().
        isCompleted = false.
        failedChallenge = false.
    ]

    getIsCompleted [ ^isCompleted ]

    startChallenge [
        | cmd isUnknown |
        Transcript show: 'The challenge has started! Charon will help you cross the River Styx.'; cr.
        Transcript show: 'Do make sure to follow the correct order to avoid disaster!'; cr.
        Transcript show: 'You can "ferry X" to have Charon take the crew, wine, Cerberus or none across the river.'; cr.
        Transcript show: 'To have Charon return with X - "return X"'; cr.

        [isCompleted] whileFalse: [
            isUnknown := true.
            Transcript show: '|> '.
            cmd = FileStream stdin nextLine.

            (cmd startsWith: 'ferry') ifTrue: [
                self ferry: (cmd copyFrom: 7 to: cmd size).
                isUnknown := false
            ].

            (cmd startsWith: 'return') ifTrue: [
                self return: (cmd copyFrom: 8 to: cmd size).
                isUnknown := false
            ].

            isUnknown ifTrue: [
                Transcript show: 'You cannot do that.'; cr.
            ]

        ]
    ]
]