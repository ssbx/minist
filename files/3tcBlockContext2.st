Object subclass: #Rectangle
	instanceVariableNames: 'origin corner'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Primitives'!

Rectangle comment: 'comment'!

!Rectangle methodsFor: 'test'!

" page 560 "

containsInstanceOf: aClass
    self do: [ :element | (element isKindOf: aClass) ifTrue: [ ^ true ] .
    ^ false !
!
