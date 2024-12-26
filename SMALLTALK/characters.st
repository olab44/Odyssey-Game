Object subclass: Character [
    | name dialogues |

    Character class >> new [ ^ super new init ]

    init [
        name := ''.
        dialogues := #()
    ]

    name: aName dialogues: aDialogues [
        name := aName.
        dialogues := aDialogues
    ]
]