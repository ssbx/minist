Object subclass: #Rectangle
	instanceVariableNames: 'origin corner'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Primitives'!

Rectangle comment: 'comment'!

!Rectangle methodsFor: 'test'!


" page 549 "

" Increment a shared variable with a constant "
incrementIndex
    ^ Index := Index + 2 !

!
