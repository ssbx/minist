Object subclass: #Rectangle
	instanceVariableNames: 'origin corner'
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Primitives'!

Rectangle comment: 'comment'!

!Rectangle methodsFor: 'test'!

" bytecodes page 596 "
" merge page 546 "

merge: aRectangle
    | minPoint maxPoint |
    minPoint := origin min: aRectangle origin .
    maxPoint := corner max: aRectangle corner . !
    "^ Rectangle origin: minPoint corner: maxPoint !"

!
