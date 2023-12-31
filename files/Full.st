
Object subclass: #Rectangle
	instanceVariableNames: 'origin corner '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Primitives'!
Rectangle comment:
'I usually represent a rectangular area on the screen.  Arithmetic functions take points as arguments and carry out scaling and translating operations to create new Rectangles.  Rectangle functions create new Rectangles by determining intersections of rectangles with rectangles. '!


!Rectangle methodsFor: 'accessing'!

area
	"Answer the receiver's area, the product of width and height."
	^self width * self height!

bottom
	"Answer the position of the receiver's bottom horizontal line."
	^corner y!

bottom: anInteger
	"Set the position of the bottom horizontal line of the receiver."
	corner y: anInteger!

bottomCenter
	"Answer the point at the center of the bottom horizontal line of the receiver."
	^self center x @ self bottom!

bottomLeft
	"Answer the point at the left edge of the bottom horizontal line of the receiver."
	^origin x @ corner y!

bottomRight
	"Answer the point at the right edge of the bottom horizontal line of the receiver."
	^corner!

bottomRight: bottomRightPoint
	"Set the position of the right corner of the bottom horizontal line of the receiver."
	corner _ bottomRightPoint!

center
	"Answer the point at the center of the receiver."
	^self topLeft + self bottomRight // 2!

corner
	"Answer the point at the bottom right corner of the receiver."
	^corner!

corner: cornerPoint
	"Set the point at the bottom right corner of the receiver."
	corner _ cornerPoint!

extent
	"Answer with a rectangle with origin 0@0 and corner the receiver's
	width @ the receiver's height."
	^corner - origin!

extent: extentPoint
	"Set the extent (width and height) of the receiver to be extentPoint."
	corner _ origin + extentPoint!

height
	"Answer the height of the receiver."
	^corner y - origin y!

height: heightInteger
	"Change the receiver's bottom y to make its height heightInteger."
	corner y: origin y + heightInteger!

left
	"Answer the position of the receiver's left vertical line."
	^origin x!

left: anInteger
	"Set the position of the receiver's left vertical line."
	origin x: anInteger!

leftCenter
	"Answer the point at the center of the receiver's left vertical line."
	^self left @ self center y!

origin
	"Answer the point at the top left corner of the receiver."
	^origin!

origin: originPoint
	"Set the point at the top left corner of the receiver."
	origin _ originPoint!

origin: originPoint corner: cornerPoint
	"Set the points at the top left corner and the bottom right corner of the receiver."
	origin _ originPoint.
	corner _ cornerPoint!

origin: originPoint extent: extentPoint
	"Set the point at the top left corner of the receiver to be originPoint and
	set the width and height of the receiver to be extentPoint."
	origin _ originPoint.
	corner _ origin + extentPoint!

right
	"Answer the position of the receiver's right vertical line."
	^corner x!

right: anInteger
	"Set the position of the receiver's right vertical line."
	corner x: anInteger!

rightCenter
	"Answer the point at the center of the receiver's right vertical line."
	^self right @ self center y!

top
	"Answer the position of the receiver's top horizontal line."
	^origin y!

top: anInteger
	"Set the position of the receiver's top horizontal line."
	origin y: anInteger!

topCenter
	"Answer the point at the center of the receiver's top horizontal line."
	^self center x @ self top!

topLeft
	"Answer the point at the top left corner of the receiver's top horizontal line."
	^origin!

topLeft: topLeftPoint
	"Set the point at the top left corner of the receiver's top horizontal line."
	origin _ topLeftPoint!

topRight
	"Answer the point at the top right corner of the receiver's top horizontal line."
	^corner x @ origin y!

width
	"Answer the width of the receiver."
	^corner x - origin x!

width: widthInteger
	"Change the receiver's right vertical line to make its width widthInteger."
	corner x: origin x + widthInteger! !

!Rectangle methodsFor: 'comparing'!

= aRectangle
	"Answer true if the receiver's species, origin and corner match aRectangle's."

	self species = aRectangle species
		ifTrue: [^origin = aRectangle origin and: [corner = aRectangle corner]]
		ifFalse: [^false]!

hash
	^origin hash bitXor: corner hash!

hashMappedBy: map
	"My hash is independent of my oop"
	^ self hash! !

!Rectangle methodsFor: 'rectangle functions'!

amountToTranslateWithin: aRectangle
	"Answer a Point, delta, such that self + delta is forced within aRectangle."

	| delta |
	delta _ 0@0.
	self left < aRectangle left ifTrue: [delta x: aRectangle left - self left].
	self top < aRectangle top ifTrue: [delta y: aRectangle top - self top].
	self right > aRectangle right ifTrue: [delta x: aRectangle right - self right].
	self bottom > aRectangle bottom ifTrue: [delta y: aRectangle bottom - self bottom].
	^delta!

areasOutside: aRectangle
	"Answer with a Collection of Rectangles comprising the parts of me
	which do not lie within aRectangle."
	| areas yOrigin yCorner |
	"Make sure the intersection is non-empty"
	(origin <= aRectangle corner and: [aRectangle origin <= corner])
		ifFalse: [^Array with: self].
	areas _ OrderedCollection new.
	aRectangle origin y > origin y
		ifTrue: [areas add: (origin corner: corner x @ (yOrigin _ aRectangle origin y))]
		ifFalse: [yOrigin _ origin y].
	aRectangle corner y < corner y
		ifTrue: [areas add: (origin x @ (yCorner _ aRectangle corner y) corner: corner)]
		ifFalse: [yCorner _ corner y].
	aRectangle origin x > origin x
		ifTrue: [areas add: (origin x @ yOrigin corner: aRectangle origin x @ yCorner)].
	aRectangle corner x < corner x
		ifTrue: [areas add: (aRectangle corner x @ yOrigin corner: corner x @ yCorner)].
	^areas!

expandBy: delta
	"Answer a Rectangle that is outset from the receiver by delta.
	 delta is a Rectangle, Point, or scalar."

	(delta isKindOf: Rectangle)
		ifTrue: [^Rectangle
					origin: origin - delta origin
					corner: corner + delta corner]
		ifFalse: [^Rectangle
					origin: origin - delta
					corner: corner + delta]!

insetBy: delta
	"Answer a Rectangle that is inset from the receiver by delta.
	 delta is a Rectangle, Point, or scalar."

	(delta isKindOf: Rectangle)
		ifTrue: [^Rectangle
					origin: origin + delta origin
					corner: corner - delta corner]
		ifFalse: [^Rectangle
					origin: origin + delta
					corner: corner - delta]!

insetOriginBy: originDeltaPoint cornerBy: cornerDeltaPoint
	"Answer a Rectangle that is inset from the receiver by a given amount in the
	origin and corner."

	^Rectangle
		origin: origin + originDeltaPoint
		corner: corner - cornerDeltaPoint!

intersect: aRectangle
	"Answer a Rectangle that is the area in which the receiver overlaps with
	aRectangle. "

	^Rectangle
		origin: (origin max: aRectangle origin)
		corner: (corner min: aRectangle corner)!

merge: aRectangle
	"Answer a Rectangle that contains both the receiver and aRectangle."

	^Rectangle
		origin: (origin min: aRectangle origin)
		corner: (corner max: aRectangle corner)! !

!Rectangle methodsFor: 'testing'!

contains: aRectangle
	"Answer whether the receiver is equal to aRectangle or whether aRectangle
	is contained within the receiver."

	^aRectangle origin >= origin and: [aRectangle corner <= corner]!

containsPoint: aPoint
	"Answer whether aPoint is within the receiver."

	^origin <= aPoint and: [aPoint < corner]!

intersects: aRectangle
	"Answer whether aRectangle intersects the receiver anywhere."

	^(origin max: aRectangle origin) < (corner min: aRectangle corner)! !

!Rectangle methodsFor: 'truncation and round off'!

rounded
	"Answer a Rectangle whose origin and corner are rounded."

	^Rectangle origin: origin rounded corner: corner rounded! !

!Rectangle methodsFor: 'transforming'!

align: aPoint1 with: aPoint2
	"Answer a new Rectangle that is a translated by aPoint2 - aPoint1."
	^self translateBy: aPoint2 - aPoint1!

moveBy: aPoint
	"Change the corner positions of the receiver so that its area translates by
	the amount defined by the argument, aPoint."
	origin _ origin + aPoint.
	corner _ corner + aPoint!

moveTo: aPoint
	"Change the corners of the receiver so that its top left position is aPoint."

	corner _ corner + aPoint - origin.
	origin _ aPoint!

scaleBy: scale
	"Answer a new Rectangle scaled by scale, a Point or a scalar."

	^Rectangle origin: origin * scale corner: corner * scale!

translateBy: factor
	"Answer a new Rectangle translated by factor, a Point or a scalar."

	^Rectangle origin: origin + factor corner: corner + factor! !

!Rectangle methodsFor: 'copying'!

copy
	^self deepCopy! !

!Rectangle methodsFor: 'printing'!

printOn: aStream
	origin printOn: aStream.
	aStream nextPutAll: ' corner: '.
	corner printOn: aStream!

storeOn: aStream
	aStream nextPut: $(;
	nextPutAll: self species name;
	nextPutAll: ' origin: ';
	store: origin;
	nextPutAll: ' corner: ';
	store: corner;
	nextPut: $).! !

"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

Rectangle class
	instanceVariableNames: ''!


!Rectangle class methodsFor: 'instance creation'!

fromUser
	"Answer an instance of me that is determined by having the
	user designate the top left and bottom right corners.  The gridding for
	user selection is 1@1."
	^self fromUser: 1 @ 1!

fromUser: gridPoint
	"Answer an instance of me that is determined by having the
	user designate the top left and bottom right corners.  The gridding for
	user selection is gridPoint."

	| originPoint newSelf minCorner oldCursor|
	oldCursor _ Sensor currentCursor.
	Cursor cursorLink: false.
	originPoint _ Sensor cursorPoint grid: gridPoint.
	[Sensor redButtonPressed]
		whileFalse:
			[Processor yield.
			Cursor origin showGridded: gridPoint.
			originPoint _ Sensor cursorPoint grid: gridPoint].
	newSelf _ self new origin: originPoint.
	minCorner _ originPoint + gridPoint.
	Sensor cursorPoint: minCorner.
	[Sensor redButtonPressed]
			whileTrue:
				[Processor yield.
				Cursor corner showGridded: gridPoint.
				newSelf corner: ((Sensor cursorPoint grid: gridPoint) max: minCorner).
				Display flash: newSelf].
	Cursor cursorLink: true.
	oldCursor show.
	^newSelf

	"Rectangle fromUser: 100@100."!

fromUserAspectRatio: aspectPoint
	"Answer an instance of Rectangle that is determined by having the user designate the top left and bottom right corners, constrained so the aspect ratio is determined by aspectPoint."

	| rectangle extent ratio |
	Cursor origin showWhile:
		[rectangle _ Sensor waitButton extent: 1@1].
	ratio _ aspectPoint y/aspectPoint x.
	Cursor corner showWhile:
		[Sensor cursorPoint: rectangle corner.
		[Sensor anyButtonPressed] whileTrue:
			[extent _ Sensor cursorPoint-rectangle origin max: 1@1.
			(extent y*aspectPoint x) > (aspectPoint y*extent x)
				ifTrue: [extent x: (extent y/ratio) rounded] 	"contrain width"
				ifFalse: [extent y: (extent x*ratio) rounded]. 	"constrain height"
			rectangle extent: extent.
			Display flash: rectangle]].
	^rectangle

	"Rectangle fromUserAspectRatio: Sensor waitButton"!

left: leftNumber right: rightNumber top: topNumber bottom: bottomNumber
	"Answer an instance of me whose left, right, top, and bottom coordinates are
	determined by the arguments."

	^self origin: leftNumber @ topNumber corner: rightNumber @ bottomNumber!

origin: originPoint corner: cornerPoint
	"Answer an instance of me whose corners (top left and bottom right) are
	determined by the arguments."

	^self new origin: originPoint corner: cornerPoint!

origin: originPoint extent: extentPoint
	"Answer an instance of me whose top left corner is originPoint and width by
	height is extentPoint."

	^self new origin: originPoint extent: extentPoint!

originFromUser: extentPoint
	"Answer an instance of me that is determined by having the
	user designate the top left corner.  The width and height are determined
	by extentPoint.  The gridding for user selection is 1@1."

	^self originFromUser: extentPoint grid: 1 @ 1!

originFromUser: extentPoint grid: scaleFactor
	"Answer an instance of me that is determined by having the
	user designate the top left corner.  The width and height are determined
	by extentPoint.  The gridding for user selection is scaleFactor. Assumes that the
	sender has determined an extent that is a proper multiple of scaleFactor."

	| tempRect |
	tempRect _ (Sensor cursorPoint grid: scaleFactor) extent: extentPoint.
	[Sensor redButtonPressed]
		whileFalse:
			[Display reverse: tempRect.
			Display reverse: tempRect.
			tempRect _ (Sensor cursorPoint grid: scaleFactor) extent: extentPoint].
	^tempRect! !
