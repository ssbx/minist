Object subclass: #Rectangle
	instanceVariableNames: 'origin corner '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Primitives'!

Rectangle comment: 'comment'!

!Rectangle methodsFor: 'test'!

centerx
    | a b |
    ^ origin + corner / (2 + a + b)!


center
    ^ origin + corner / 2 !

center: b
    | a |
    ^ origin + (corner / 2) !
!

"========================" !

Rectangle class
    instanceVariableNames: '' !

