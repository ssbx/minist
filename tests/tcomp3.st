Object subclass: #Rectangle
	instanceVariableNames: 'origin corner'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Primitives'!

Rectangle comment: 'comment'!

!Rectangle methodsFor: 'test'!


"
unarytest
    ^ origin msg !
"

at: y
    | a b |
    corner := origin := a := b := (y * 2 + 2) !

!
