Object subclass: UnderworldChallenge [
    | leftBank rightBank isCompleted failedChallenge |

    UnderworldChallenge class >> new [
        ^ super new init
    ]

    init [
        leftBank := #('crew' 'wine' 'cerberus' 'charon').
        rightBank := #().
        isCompleted := false.
        failedChallenge := false.
    ]

    getIsCompleted [ ^isCompleted ]
    getFailedChallenge [ ^failedChallenge ]

    startChallenge [
        | cmd isUnknown |
        Transcript show: 'The challenge has started! Charon will help you cross the River Styx.'; cr.
        Transcript show: 'Do make sure to follow the correct order to avoid disaster!'; cr.
        Transcript show: 'You can "ferry X" to have Charon take the crew, wine, Cerberus or only Charon across the river.'; cr.
        Transcript show: 'To have Charon return with X - "return X"'; cr.

        [ isCompleted ] whileFalse: [
            isUnknown := true.
            Transcript show: '|> '.
            cmd := FileStream stdin nextLine.

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

    ferry: anItem [
        "Handles the ferry operation from the left bank to the right bank"
        ((leftBank includes: 'charon') and: [leftBank includes: anItem]) ifTrue: [
            anItem = 'charon' ifTrue: [
                Transcript show: 'Charon crosses without taking anything.'; cr.
            ] ifFalse: [
                leftBank := leftBank copyWithout: anItem.
                rightBank := rightBank copyWith: anItem.
                Transcript show: 'Charon and ', anItem , ' have been ferried across.'; cr.
            ].
            leftBank := leftBank copyWithout: 'charon'.
            rightBank := rightBank copyWith: 'charon'.
            self checkLeftBank
        ] ifFalse: [
            Transcript show: 'Cannot ferry ' , anItem , '. Check if it''s on the correct side of the river.'; cr.
        ].
    ]

    return: anItem [
        "Handles the return operation from the right bank to the left bank"
        ((rightBank includes: 'charon') and: [rightBank includes: anItem]) ifTrue: [
            anItem = 'charon' ifTrue: [
                Transcript show: 'Charon crosses back without taking anything.'; cr.
            ] ifFalse: [
                rightBank := rightBank copyWithout: anItem.
                leftBank := leftBank copyWith: anItem.
                Transcript show: 'Charon and ', anItem , ' have returned.'; cr.
            ].
            rightBank := rightBank copyWithout: 'charon'.
            leftBank := leftBank copyWith: 'charon'.
            self checkRightBank
        ] ifFalse: [
            Transcript show: 'Cannot return ' , anItem , '. Check if it and Charon are on the correct side of the river.'; cr.
        ].
    ]

    checkRightBank [
        ((rightBank includes: 'wine') and: [ rightBank includes: 'crew' ]) ifTrue: [
            failedChallenge := true.
            Transcript show: 'Some from your crew drowned in the River Styx after drinking the wine.'; cr.
            self gameOver.
        ].
        ((rightBank includes: 'crew') and: [ rightBank includes: 'cerberus' ]) ifTrue: [
            failedChallenge := true.
            Transcript show: 'Cerberus attacked the crew.'; cr.
            self gameOver.
        ].
        (rightBank size = 4) ifTrue: [
            isCompleted := true.
            Transcript show: 'Congratulations! You have successfully ferried everything across the River Styx.'; cr.
        ].
    ]
    checkLeftBank [
        ((leftBank includes: 'wine') and: [ leftBank includes: 'crew' ]) ifTrue: [
            failedChallenge := true.
            Transcript show: 'Some from your crew drowned in the River Styx after drinking the wine.'; cr.
            self gameOver.
        ].
        ((leftBank includes: 'crew') and: [ leftBank includes: 'cerberus' ]) ifTrue: [
            failedChallenge := true.
            Transcript show: 'Cerberus attacked the crew.'; cr.
            self gameOver.
        ].
        (rightBank size = 4) ifTrue: [
            isCompleted := true.
            Transcript show: 'Congratulations! You have successfully ferried everything across the River Styx.'; cr.
        ].
    ]

    gameOver [
        Transcript show: 'Your crew has suffered a loss due to a misstep. The challenge is over.'; cr.
        failedChallenge := true.
        isCompleted := true.
    ]
]