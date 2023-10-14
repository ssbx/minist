
Object subclass: #Rectangle
	instanceVariableNames: 'origin corner '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Primitives'!
Rectangle comment:
'I usually represent a rectangular area on the screen.  Arithmetic functions take points as arguments and carry out scaling and translating operations to create new Rectangles.  Rectangle functions create new Rectangles by determining intersections of rectangles with rectangles. '!


!Rectangle methodsFor: 'accessing'!

center
    " expect bytecodes: 0, 1, 176, 119, 185, 124 (p543)
        0 pushRcvr: 0
        1 pushRcvr: 1
        176 send: +
        119 pushConstant: 2
        185 send: /
        124 returnTop
    "

    " dans squeak: 0, 1, 96, 232, 2, 105, 92
        0 pushRcvr: 0
        1 pushRcvr: 1
        96 send: +
        232,2 pushConstant: 2
        105 send: /
        92 returnTop
    "
    ^origin + corner / 2!

extent: nexExtent
    " expect bytecodes: 0, 16, 176, 97, 120 (p544) "
	corner _ origin + nexExtent! !



!Rectangle methodsFor: 'rectangle functions'!

intersects: aRectangle
    " expect bytecodes: 0, 16, 209, 224, 1, 16, 221, 226, 178, 124 "
    " literal frame:
        #max:
        #origin
        #min:
        #corner "
	^(origin max: aRectangle origin) < (corner min: aRectangle corner)! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

Rectangle class
	instanceVariableNames: ''!


!Rectangle class methodsFor: 'instance creation'!

simplenew
	^self new initialize ! !

!Rectangle class methodsFor: 'blala'!

hellow
    ^'hello' !

world
    ^'world'! !


