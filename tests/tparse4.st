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
    | a b c |
    a := b := c !

assign
    | a b c |
    ^ (a := b := c ) !

!

!Rectangle methodsFor: 'test' stamp: 'lkjlk'!

dd
    ^2 !

withSide: side setTo: value
    ^ self perform: (#(withLeft: withRight: withTop: withBottom: )
                            at: (#(left right top bottom) indexOf: side))
        with: value .
    b := #( 1 2 3 #a: #at:put: a b a: ).
    self perform: (a at: x) with: b !
!


"========================" !

Rectangle class
    instanceVariableNames: '' !
