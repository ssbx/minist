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

+ anInteger4
    | tmpx tmpy |
    ^ origin * tmpx * tmpy * anInteger4 !

mult: x by: y and: z
    ^ x * y * z !

unarytest
    ^ origin msg !

center
    " page 543 "
    ^ origin + corner / 2 !

extent: newExtent
    " page 544 "
    corner := origin + newExtent !
!
