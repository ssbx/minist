Object subclass: #Rectangle
	instanceVariableNames: 'origin corner'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Primitives'!

Rectangle comment: 'comment'!

!Rectangle methodsFor: 'test'!

other: aValue
    | a b c d |
    ^ Index := a := c := d := 4 + aValue asInteger !

" page 549 "

" Increment a shared variable with a constant "
incrementIndex
    ^ Index := Index + 4 !

!
