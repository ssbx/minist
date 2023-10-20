Object subclass: #Rectangle
	instanceVariableNames: 'origin corner'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Primitives'!

Rectangle comment: 'comment'!

!Rectangle methodsFor: 'test'!

" page 550 "

includesPoint: aPoint
    origin <= aPoint
        ifTrue:  [ ^ aPoint < corner ]
        ifFalse: [ ^ false ] !

!
