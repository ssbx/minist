Object subclass: #Rectangle
	instanceVariableNames: 'origin corner shade'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Primitives'!

Rectangle comment: 'comment'!

!Rectangle methodsFor: 'test'!

" page 562 "

intersect: aRectangle
    ^ (super intersect: aRectangle) shade: shade !

!
