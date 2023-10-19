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
    ^ origin + b / (corner / 2) !

+ a
    | b c |
    ^ origin + b + a / 2 !

at: x put: y
    | z |
    ^ x * (y * z) !

testunary
    ^ a corner !

cascade
    ^ a b c d !

assign: c
    | a b |
    a := b := c !

assign
    | a b |
    ^ (a := b := c ) !

!

"========================" !

Rectangle class
    instanceVariableNames: '' !
