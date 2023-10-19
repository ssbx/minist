Object subclass: #Rectangle
	instanceVariableNames: 'origin corner'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Primitives'!

Rectangle comment: 'comment'!

!Rectangle methodsFor: 'test'!

+ anInteger
    ^ origin + anInteger !

+ anInteger2
    ^ origin !

+ anInteger3
    ^ anInteger3 !


mult: x by: y and: z
    ^ x * y * z !

unarytest
    ^ origin msg !

center
    ^ origin + corner / 2 !

extent: newExtent
    corner := origin + newExtent !

extent: newExtent
    origin := origin + newExtent !

+ anInteger4
    | tmpx tmpy |
    tmpx := origin * tmpx * tmpy * anInteger4 !

!
