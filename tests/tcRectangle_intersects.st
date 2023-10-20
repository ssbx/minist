Object subclass: #Rectangle
	instanceVariableNames: 'origin corner'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Primitives'!

Rectangle comment: 'comment'!

!Rectangle methodsFor: 'test'!


intersects: aRectangle
    ^ (origin max: aRectangle origin) < (corner min: aRectangle corner) !

!
