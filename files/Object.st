
nil subclass: #Object
	instanceVariableNames: ''
	classVariableNames: 'DependentsFields ErrorRecursion '
	poolDictionaries: ''
	category: 'Kernel-Objects'!
Object comment: 'Object is the superclass of all classes.  It thus provides default behavior common to all objects, such as class access, copying and printing.'!


!Object methodsFor: 'initialize-release'!

release
	"Remove references to objects that may refer to the receiver. Answers self. 
	This message should be overidden by subclasses with any cycles, in which
	case the subclass should also include the expression super release."

	self breakDependents! !

!Object methodsFor: 'accessing'!

at: index 
	"Answer the value of an indexable field in the receiver. Fail if the 
	argument index is not an Integer or is out of bounds. Essential. See 
	documentation in Object metaclass."

	<primitive: 60>
	index isInteger
		ifTrue: [self errorSubscriptBounds: index].
	(index isKindOf: Number)
		ifTrue: [^self at: index truncated]
		ifFalse: [self errorNonIntegerIndex]!

at: index put: value 
	"Store the argument value in the indexable field of the receiver indicated by 
	index. Fail if the index is not an Integer or is out of bounds. Or fail if the 
	value is not of the right type for this kind of collection. Answer the 
	value that was stored. Essential. See documentation in Object metaclass."

	<primitive: 61>
	index isInteger
		ifTrue: [(index >= 1 and: [index <= self size])
					ifTrue: [self errorImproperStore]
					ifFalse: [self errorSubscriptBounds: index]].
	(index isKindOf: Number)
		ifTrue: [^self at: index truncated put: value]
		ifFalse: [self errorNonIntegerIndex]!

basicAt: index 
	"Answer the value of an indexable field in the receiver. Fail if the 
	argument index is not an Integer or is out of bounds. Essential. See 
	documentation in Object metaclass.  Do not override this message in any 
	subclass."

	<primitive: 60>
	(index isKindOf: Integer) ifTrue: [self errorSubscriptBounds: index].
	(index isKindOf: Number)
		ifTrue: [^self basicAt: index truncated]
		ifFalse: [self errorNonIntegerIndex]!

basicAt: index put: value 
	"Store the second argument value in the indexable field of the receiver 
	indicated by index. Fail if the index is not an Integer or is out of bounds. Or 
	fail if the value is not of the right type for this kind of collection. Answer 
	the value that was stored. Essential. See documentation in Object 
	metaclass. Do not override in a subclass."

	<primitive: 61>
	(index isKindOf: Integer)
		ifTrue: [(index >= 1 and: [index <= self size])
					ifTrue: [self errorImproperStore]
					ifFalse: [self errorSubscriptBounds: index]].
	(index isKindOf: Number)
		ifTrue: [^self basicAt: index truncated put: value]
		ifFalse: [self errorNonIntegerIndex]!

basicSize
	"Answer the number of indexable fields in the receiver. This value is the 
	same as the largest legal subscript. Essential. See documentation in Object 
	metaclass. Do not override in any subclass."

	<primitive: 62>
	"The number of indexable fields of fixed-length objects is 0"
	^0!

readFromString: aString
	"Create an object based on the contents of aString."

	^self readFrom: (ReadStream on: aString)!

size
	"Answer the number of indexable fields in the receiver.  This value is the 
	same as the largest legal subscript.  Essential.  See documentation in Object 
	metaclass. "

	<primitive: 62>
	"The number of indexable fields of fixed-length objects is 0"
	^0!

yourself
	"Answer self."! !

!Object methodsFor: 'testing'!

isInteger
	"Coerces Integers to true and everything else to false.  Integer
	overrides with ^true"

	^false!

isNil
	"Coerces nil to true and everything else to false.  UndefinedObject 
	overrides with ^true"

	^false!

notNil
	"Coerces nil to false and everything else to true.  UndefinedObject 
	overrides with ^false"

	^true! !

!Object methodsFor: 'comparing'!

= anObject 
	"Answer true if the receiver and the argument represent the same object 
	and false otherwise. If = is redefined in any subclass, consider also
	redefining the message hash."

	^self == anObject!

== anObject 
	"Answer true if the receiver and the argument are the same object (have the same 
	object pointer) and false otherwise.  Do not redefine the message == in any 
	other class!!  Essential.  No Lookup.  See documentation in Object metaclass."

	<primitive: 110>
	self primitiveFailed!

hash
	"Answer a SmallInteger whose value is half of the receiver's object pointer
	(interpreting object pointers as 16-bit signed quantities).  Fails if the 
	receiver is a SmallInteger.  Essential.  See documentation in Object metaclass."

	<primitive: 75>
	self primitiveFailed!

hashMappedBy: map
	"Answer what my hash would be if oops changed according to map"
	^ map newHashFor: self hash!

~= anObject 
	"Answer true if the receiver and the argument do not represent the same 
	object and false otherwise."

	^self = anObject == false!

~~ anObject 
	"Answer true if the receiver and the argument are not the same object 
	(have the same object pointer) and false otherwise."

	^(self == anObject) not! !

!Object methodsFor: 'copying'!

copy
	"Answer another instance just like the receiver. Subclasses typically override
	this method;  they typically do not override shallowCopy"

	^self shallowCopy!

deepCopy
	"Answer a copy of the receiver with its own copy of each instance variable."

	| newObject class index |
	class _ self class.
	(class == Object) ifTrue: [^self].
	class isVariable
		ifTrue: 
			[index _ self basicSize.
			newObject _ class basicNew: index.
			[index > 0]
				whileTrue: 
					[newObject basicAt: index put: (self basicAt: index) deepCopy.
					index _ index - 1]]
		ifFalse: [newObject _ class basicNew].
	index _ class instSize.
	[index > 0]
		whileTrue: 
			[newObject instVarAt: index put: (self instVarAt: index) deepCopy.
			index _ index - 1].
	^newObject!

shallowCopy
	"Answer a copy of the receiver which shares the receiver's instance 
	variables. "

	| class newObject index |
	class _ self class.
	"I don't understand why the following check is here.  Object is not 
	supposed to have any instances at all."
	class == Object ifTrue: [^self].
	class isVariable
		ifTrue: 
			[index _ self basicSize.
			newObject _ class basicNew: index.
			[index > 0]
				whileTrue: 
					[newObject basicAt: index put: (self basicAt: index).
					index _ index - 1]]
		ifFalse: [newObject _ class basicNew].
	index _ class instSize.
	[index > 0]
		whileTrue: 
			[newObject instVarAt: index put: (self instVarAt: index).
			index _ index - 1].
	^newObject! !

!Object methodsFor: 'dependents access'!

addDependent: anObject
	"Add anObject as one of the receiver's dependents."
	(DependentsFields at: self ifAbsent: [self setDependents])
		add: anObject.
	^anObject!

dependents 
	"Answer an OrderedCollection of the objects that are dependent on the
	receiver, that is, the objects that should be notified if the receiver changes."

	^ DependentsFields at: self ifAbsent: [OrderedCollection new]!

removeDependent: anObject
	"Remove the argument, anObject, as one of the receiver's dependents."
	| dependents |
	dependents _ DependentsFields at: self ifAbsent: [^ anObject].
	dependents remove: anObject ifAbsent: [].
	dependents isEmpty ifTrue: [self breakDependents].
	^anObject! !

!Object methodsFor: 'updating'!

broadcast: aSymbol 
	"Send the argument, aSymbol, as a unary message to all of the receiver's dependents."

	self dependents ~~ nil
		ifTrue: [self dependents do: 
					[:aDependent | aDependent perform: aSymbol]]!

broadcast: aSymbol with: anObject 
	"Send the argument, aSymbol, as a keyword message with argument anObject to
	all of the receiver's dependents."

	self dependents ~~ nil
		ifTrue: [self dependents do:
					[:aDependent | aDependent perform: aSymbol with: anObject]]!

changed
	"Receiver changed in a general way; inform all the dependents by sending 
	each dependent an update: message."

	self changed: self!

changed: aParameter 
	"Receiver changed.  The change is denoted by the argument aParameter.
	Usually the argument is a Symbol that is part of the dependent's change
	protocol.  Inform all of the dependents."

	self dependents do: [:aDependent | aDependent update: aParameter]!

changeRequest
	"Receiver wants to change; check with all dependents that it is OK."

	self dependents do: [:dep | dep updateRequest ifFalse: [^false]].
	^true!

changeRequestFrom: requestor
	"Receiver wants to change; check with all dependents (other than requestor) that it is OK."

	self dependents do: [:dep | (dep == requestor or: [dep updateRequest]) ifFalse: [^false]].
	^true!

update: aParameter 
	"Receive a change notice from an object of whom the receiver is a dependent. 
	The default behavior is to do nothing;  a subclass might want to change
	itself in some way."

	^self!

updateRequest
	"Default behavior is to grant update requests;  a subclass might want to override
	this behavior if it is in the middle of making another change."

	^ true! !

!Object methodsFor: 'printing'!

isLiteral
	"Answer whether the receiver has a literal text form recognized by the compiler."

	^false!

printOn: aStream 
	"Append to the argument aStream a sequence of characters that identifies the receiver."

	| title |
	title _ self class name.
	aStream nextPutAll: ((title at: 1) isVowel
							ifTrue: ['an ']
							ifFalse: ['a '])
						, title!

printString
	"Answer a String whose characters are a description of the receiver."

	| aStream |
	aStream _ WriteStream on: (String new: 16).
	self printOn: aStream.
	^aStream contents!

storeOn: aStream 
	"Append to the argument aStream a sequence of characters that is an expression 
	whose evaluation creates an object similar to the receiver."

	aStream nextPut: $(.
	self class isVariable
		ifTrue: [aStream nextPutAll: '(', self class name, ' basicNew: ';
					store: self basicSize;
					nextPutAll: ') ']
		ifFalse: [aStream nextPutAll: self class name, ' basicNew'].
	1 to: self class instSize do:
		[:i |
		aStream nextPutAll: ' instVarAt: ';
			store: i;
			nextPutAll: ' put: ';
			store: (self instVarAt: i);
			nextPut: $;].
	1 to: self basicSize do:
		[:i |
		aStream nextPutAll: ' basicAt: ';
			store: i;
			nextPutAll: ' put: ';
			store: (self basicAt: i);
			nextPut: $;].
	aStream nextPutAll: ' yourself)'!

storeString
	"Answer a String representation of the receiver from which the receiver
	can be reconstructed."

	| aStream |
	aStream _ WriteStream on: (String new: 16).
	self storeOn: aStream.
	^aStream contents! !

!Object methodsFor: 'class membership'!

class
	"Answer the object which is the receiver's class. Essential.  See 
	documentation in Object metaclass."

	<primitive: 111>
	self primitiveFailed!

isKindOf: aClass 
	"Answer a Boolean as to whether the class, aClass, is a superclass or class of
	the receiver."

	self class == aClass
		ifTrue: [^true]
		ifFalse: [^self class inheritsFrom: aClass]!

isMemberOf: aClass 
	"Answer a Boolean as to whether the receiver is an instance of the class, aClass."

	^self class == aClass!

respondsTo: aSymbol 
	"Answer a Boolean as to whether the method dictionary of the receiver's class 
	contains aSymbol as a message selector."

	^self class canUnderstand: aSymbol! !

!Object methodsFor: 'message handling'!

perform: aSymbol 
	"Send the receiver the unary message indicated by the argument. The argument is 
	the selector of the message. Invoke messageNotUnderstood: if the number of 
	arguments expected by the selector is not zero. Optional. See documentation
	in Object metaclass."

	<primitive: 83>
	^self perform: aSymbol withArguments: (Array new: 0)!

perform: aSymbol with: anObject 
	"Send the receiver the keyword message indicated by the arguments. The first 
	argument is the selector of the message. The other argument is the 
	argument of the message to be sent. Invoke messageNotUnderstood: if the 
	number of arguments expected by the selector is not one. Optional. See 
	documentation in Object metaclass."

	<primitive: 83>
	^self perform: aSymbol withArguments: (Array with: anObject)!

perform: aSymbol with: firstObject with: secondObject 
	"Send the receiver the keyword message indicated by the arguments. The first 
	argument is the selector of the message. The other arguments are the 
	arguments of the message to be sent. Invoke messageNotUnderstood: if 
	the number of arguments expected by the selector is not two. Optional. 
	See documentation in Object metaclass."

	<primitive: 83>
	^self perform: aSymbol withArguments: (Array with: firstObject with: secondObject)!

perform: aSymbol with: firstObject with: secondObject with: thirdObject 
	"Send the receiver the keyword message indicated by the arguments. The first 
	argument is the selector of the message. The other arguments are the 
	arguments of the message to be sent. Invoke messageNotUnderstood: if 
	the number of arguments expected by the selector is not three. Optional. 
	See documentation in Object metaclass."

	<primitive: 83>
	^self perform: aSymbol withArguments: (Array
			with: firstObject
			with: secondObject
			with: thirdObject)!

perform: selector withArguments: anArray 
	"Send the receiver the keyword message indicated by the arguments. The argument  
	selector is the selector of the message. The arguments of the message are the 
	elements of anArray. Invoke messageNotUnderstood: if the number of 
	arguments expected by the selector is not the same as the length of 
	anArray. Essential. See documentation in Object metaclass."

	<primitive: 84>
	self primitiveFailed! !

!Object methodsFor: 'error handling'!

confirm: aString 
	"Create and start up a BinaryChoice menu with the argument as the message in order
	to determine true or false.  Answers true or false."
	| answer |
	answer _ false.
	BinaryChoice
		message: aString
		displayAt: Sensor cursorPoint
		centered: true
		ifTrue: [answer _ true]
		ifFalse: [answer _ false].
	^answer!

conflictingInheritanceError
	"Browse to the method which called this,
		redefine it appropriately,
		and then restart that calling method. "

	self error: 'Conflicting methods due to multiple inheritance'!

doesNotUnderstand: aMessage 
	"First check for a compound selector.  If found, try copying down code
	into the receiver's class.  If this is unsuccessful,
	announce that the receiver does not understand the argument, aMessage,
	as a message.  The default behavior is to create a Notifier containing the 
	appropriate message and to allow the user to open a Debugger. 
	Subclasses can override this message in order to modify this behavior."
	| status gripe |

	status _ self class tryCopyingCodeFor: aMessage selector.
	status==#OK ifTrue:
		[^self perform: aMessage selector withArguments: aMessage arguments].

	gripe _ status==#HierarchyViolation
		ifTrue: [aMessage selector classPart , ' is not one of my superclasses: ']
		ifFalse: ['Message not understood: '].
	NotifierView
		openContext: thisContext
		label: gripe , aMessage selector
		contents: thisContext shortStack.
	"Try the message again if the programmer decides to proceed."
	^self perform: aMessage selector withArguments: aMessage arguments

	"3 zork."!

error: aString 
	"The default behavior for error: is the same as halt:. 
	This additional message is the one a subclass should override in order to
	change the handling of errors."

	NotifierView
		openContext: thisContext
		label: aString
		contents: thisContext shortStack

	"nil error: 'error message'."!

halt
	"This is a simple message to use for inserting breakpoints during debugging."

	NotifierView
		openContext: thisContext
		label: 'Halt encountered.'
		contents: thisContext shortStack

	"nil halt."!

halt: aString 
	"This message can be used for inserting breakpoints during debugging.
	It creates and schedules a Debugger with the argument, aString, as the label."

	NotifierView
		openContext: thisContext
		label: aString
		contents: thisContext shortStack

	"nil halt: 'Test of halt:.'."!

notify: aString 
	"Create and schedule a Notifier with the argument as the message in order
	to request confirmation before a process can proceed."


	NotifierView
		openContext: thisContext
		label: 'Notifier'
		contents: aString

	"nil notify: 'confirmation message'."!

primitiveFailed
	"Announce that a primitive has failed and there is no appropriate 
	Smalltalk code to run."

	self error: 'a primitive has failed'!

shouldNotImplement
	"Announce that although the receiver inherits this message, it
	should not implement it."

	self error: 'This message is not appropriate for this object'!

subclassResponsibility
	"This message sets up a framework for the behavior of the class' subclasses.
	Announce that the subclass should have implemented this message."

	self error: 'My subclass should have overridden one of my messages.'! !

!Object methodsFor: 'user interface'!

basicInspect
	"Create and schedule an Inspector in which the user can examine the
	receiver's variables.  This method should not be overwritten."

	InspectorView open: (Inspector inspect: self)!

inspect
	"Create and schedule an Inspector in which the user can examine the
	receiver's variables."
	self basicInspect! !

!Object methodsFor: 'system primitives'!

asOop
	"Answer an Integer which is unique to me.  See below.
	Essential.  See also documentation in Object metaclass."
	<primitive: 75>
	self primitiveFailed
	"
	Non-Stretch mapping between objects and asOop value:
	oops 0...16K-1			-->	0...16K-1
	oops 16K...32K-1			-->	-16K...-1
	SmallIntegers 0...16K		-->	32K...48K-1
	SmallIntegers -16K...0	-->	48K...64K-1

	Stretch mapping between objects and asOop value:
	oops 0...48K-1			-->	0...48K-1
	SmallIntegers -8K...-1	-->	-16K...-2 even
	SmallIntegers 0...8K-1	-->	-(16K-1)...-1 odd

	Non-Stretch Consistency check:
	| obj |
	-16384 to: 16383 do:
		[:i | obj _ i asObject.
		(obj == #NonExistentObject or: [obj asOop = i])
			ifFalse: [self error: 'inconsistency']].
	32768 to: 65536 do:
		[:i | obj _ i asObject.
		(obj == #NonExistentObject or: [obj asOop = i])
			ifFalse: [self error: 'inconsistency']].

	Stretch Consistency check:
	| obj |
	-16384 to: 49151 do:
		[:i | obj _ i asObject.
		(obj == #NonExistentObject or: [obj asOop = i])
			ifFalse: [self error: 'inconsistency']].
	"!

become: otherObject 
	"Swap the instance pointers of the receiver and the argument, otherObject.  All
	variables in the entire system that used to point to the receiver now point to the
	argument, and vice-versa.  Fails if either object in a SmallInteger.  Answer the
	argument which is now the same instance pointer that formerly denoted the receiver.  	Essential.  See documentation in Object metaclass."

	<primitive: 72>
	self primitiveFailed!

instVarAt: index 
	"Answer a fixed variable in an object.  The numbering of the variables 
	corresponds to the named instance variables.  Fail if the index is not an 
	Integer or is not the index of a fixed variable.  Essential.  See documentation in 
	Object metaclass."

	<primitive: 73>
	"Access beyond fixed variables."
	^self basicAt: index - self class instSize!

instVarAt: anInteger put: anObject 
	"Store a value into a fixed variable in the receiver.  The numbering of the variables  
	corresponds to the named instance variables.  Fail if the index is not an  
	Integer or is not the index of a fixed variable.   Answer the value stored as the  
	result.  Using this message violates the principle that each object has  
	sovereign control over the storing of values into its instance variables.   
	Essential.  See documentation in Object metaclass."

	<primitive: 74>
	"Access beyond fixed fields"
	^self basicAt: anInteger - self class instSize put: anObject!

nextInstance
	"Answer the next instance after the receiver in the enumeration of all 
	instances of this class.  Fails if all instances have been enumerated.  
	Essential.  See documentation in Object metaclass."

	<primitive: 78>
	^nil! !

!Object methodsFor: 'system simulation'!

tryPrimitive0
	"Warning!!!! This is not a real primitive.  This method is a template that the 
	Smalltalk simulator uses to execute primitives with no arguments.  See 
	ContextPart class initPrimitives and ContextPart doPrimitive:receiver:args:."

	<primitive: 007>
	^#primitiveFail!

tryPrimitive1: arg1 
	"Warning!!!! This is not a real primitive.  This method is a template that the 
	Smalltalk simulator uses execute primitives with one argument.  See 
	ContextPart class|initPrimitives and ContextPart|doPrimitive:receiver:args:."

	<primitive: 007>
	^#primitiveFail!

tryPrimitive2: arg1 with: arg2 
	"Warning!!!! This is not a real primitive.  This method is a template that the 
	Smalltalk simulator uses execute primitives with two arguments.  See 
	ContextPart class|initPrimitives and ContextPart|doPrimitive:receiver:args:."

	<primitive: 007>
	^#primitiveFail!

tryPrimitive3: arg1 with: arg2 with: arg3 
	"Warning!!!! This is not a real primitive.  This method is a template that the 
	Smalltalk simulator uses execute primitives with three arguments.  See 
	ContextPart class|initPrimitives and ContextPart|doPrimitive:receiver:args:."

	<primitive: 007>
	^#primitiveFail!

tryPrimitive4: arg1 with: arg2 with: arg3 with: arg4 
	"Warning!!!! This is not a real primitive.  This method is a template that the 
	Smalltalk simulator uses execute primitives with four arguments.  See 
	ContextPart class|initPrimitives and ContextPart|doPrimitive:receiver:args:."

	<primitive: 007>
	^#primitiveFail! !

!Object methodsFor: 'private'!

breakDependents
	"Deallocate the soft field for the receiver's dependents."

	DependentsFields removeKey: self ifAbsent: []!

errorImproperStore
	"Create an error notification that an improper store was attempted."

	self error: 'Improper store into indexable object'!

errorNonIntegerIndex
	"Create an error notification that an improper object was used as an index."

	self error: 'only integers should be used as indices'!

errorSubscriptBounds: index 
	"Create an error notification that an improper integer was used as an index."

	self error: 'subscript is out of bounds: ' , index printString!

forkEmergencyEvaluatorAt: priority
	"Fork a process running a simple Smalltalk evaluator using as little of the system as possible.  Used for desperate debugging.  may be invoked by control-shift-C."

	| stream char |
		[Display white: (0@0 extent: 1024@36).
		'EMERGENCY EVALUATOR (priority ', priority printString, ') -- type an expression terminated by ESC' displayAt: 50@0.
		Display reverse: (0@0 extent: 1024@36).
		stream _ WriteStream on: String new.
		[[Sensor keyboardPressed] whileFalse.
		(char _ Sensor keyboard) = 160 asCharacter] whileFalse:
			[char = Character backspace
				ifTrue: [stream skip: -1.  Display black: (0@18 extent: 1024@18)]
				ifFalse: [stream nextPut: char].
			stream contents displayAt: 50@18].
		Display black: (0@0 extent: 1024@18).
		(Compiler evaluate: stream contents) printString displayAt: 50@0] forkAt: priority!

mustBeBoolean
	"Catches attempts to test truth of non-Booleans.  This message is sent from the
	interpreter."

	self error: 'NonBoolean receiver--proceed for truth.'.
	^true!

nilFields
	"Store nil into all pointer fields of the receiver."

	self class isPointers ifFalse: [^self].
	1 to: self basicSize do:
		[:index | self basicAt: index put: nil].
	1 to: self class instSize do:
		[:index | self instVarAt: index put: nil].!

primitiveError: aString 
	"This method is called when the error handling results in a recursion in calling
	on error: or halt or halt:."

	| context key |
	Transcript cr.
	Transcript show: '**System Error Handling Failed** '.
	Transcript show: aString.
	Transcript cr.
	context _ thisContext sender sender.
	3 timesRepeat: 
		[context == nil ifFalse: [Transcript print: (context _ context sender); cr]].

	[Transcript show: '**type <s> for more stack; anything else restarts scheduler**'.
	Transcript cr.
	key _ Sensor keyboard.
	key = $s | (key = $S)] 
		whileTrue: 
			[5 timesRepeat: 
				[context == nil 
					ifFalse: [Transcript print: (context _ context sender); cr]]].
	ScheduledControllers searchForActiveController!

setDependents
	"Allocate the soft field for the receiver's dependents."
	| dependents |
	dependents _ OrderedCollection new.
	DependentsFields add: (Association key: self value: dependents).
	^ dependents!

species
	"Answer the preferred class for reconstructing the receiver.  For example, 
	collections create new collections whenever enumeration messages such as 
	collect: or select: are invoked.  The new kind of collection is determined by 
	the species of the original collection.  Species and class are not always the 
	same.  For example, the species of Interval is Array."

	^self class! !
"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!

Object class
	instanceVariableNames: ''!


!Object class methodsFor: 'class initialization'!

initialize 
	(self confirm: 'Do you really want to initialize
class Object?  It will reset all dependents.')
		ifTrue:
			[self initializeDependentsFields.
			self initializeErrorRecursion]

	"Object initialize"! !

!Object class methodsFor: 'instance creation'!

readFrom: aStream
	"Create an object based on the contents of aStream."

	| object |
	object _ Compiler evaluate: aStream.
	(object isKindOf: self) ifFalse: [self error: self name, ' expected'].
	^object! !

!Object class methodsFor: 'documentation'!

howToModifyPrimitives
	"You are allowed to write methods which specify primitives, but please use 
	caution.  If you make a subclass of a class which contains a primitive method, 
	the subclass inherits the primitive.  The message which is implemented 
	primitively may be overridden in the subclass (E.g., see at:put: in String's 
	subclass Symbol).  The primitive behavior can be invoked using super (see 
	Symbol string:). 
	 
	A class which attempts to mimic the behavior of another class without being 
	its subclass may or may not be able to use the primitives of the original class.  
	In general, if the instance variables read or written by a primitive have the 
	same meanings and are in the same fields in both classes, the primitive will 
	work.  

	For certain frequently used 'special selectors', the compiler emits a 
	send-special-selector bytecode instead of a send-message bytecode.  
	Special selectors were created because they offer two advantages.  Code 
	which sends special selectors compiles into fewer bytes than normal.  For 
	some pairs of receiver classes and special selectors, the interpreter jumps 
	directly to a primitive routine without looking up the method in the class.  
	This is much faster than a normal message lookup. 
	 
	A selector which is a special selector solely in order to save space has a 
	normal behavior.  Methods whose selectors are special in order to 
	gain speed contain the comment, 'No Lookup'.  When the interpreter 
	encounters a send-special-selector bytecode, it checks the class of the 
	receiver and the selector.  If the class-selector pair is a no-lookup pair, 
	then the interpreter swiftly jumps to the routine which implements the 
	corresponding primitive.  (A special selector whose receiver is not of the 
	right class to make a no-lookup pair, is looked up normally).  The pairs are 
	listed below.  No-lookup methods contain a primitive number specification, 
	<primitive: xx>, which is redundant.  Since the method is not normally looked 
	up, deleting the primitive number specification cannot prevent this 
	primitive from running.  If a no-lookup primitive fails, the method is looked 
	up normally, and the expressions in it are executed. 
	 
	No Lookup pairs of (class, selector) 
	 
	SmallInteger with any of		+ - * /  \\  bitOr: bitShift: bitAnd:  // 
	SmallInteger with any of		=  ~=  >  <  >=  <= 
	Any class with					== 
	Any class with 					@ 
	Point with either of				x y 
	ContextPart with					blockCopy: 
	BlockContext with either of 		value value:
	"

	self error: 'comment only'!

whatIsAPrimitive
	"Some messages in the system are responded to primitively. A primitive   
	response is performed directly by the interpreter rather than by evaluating   
	expressions in a method. The methods for these messages indicate the   
	presence of a primitive response by including <primitive: xx> before the   
	first expression in the method.   
	  
	Primitives exist for several reasons. Certain basic or 'primitive' 
	operations cannot be performed in any other way. Smalltalk without 
	primitives can move values from one variable to another, but cannot add two 
	SmallIntegers together. Many methods for arithmetic and comparison 
	between numbers are primitives. Some primitives allow Smalltalk to 
	communicate with I/O devices such as the disk, the display, and the keyboard. 
	Some primitives exist only to make the system run faster; each does the same 
	thing as a certain Smalltalk method, and its implementation as a primitive is 
	optional.  
	  
	When the Smalltalk interpreter begins to execute a method which specifies a 
	primitive response, it tries to perform the primitive action and to return a 
	result. If the routine in the interpreter for this primitive is successful, 
	it will return a value and the expressions in the method will not be evaluated. 
	If the primitive routine is not successful, the primitive 'fails', and the 
	Smalltalk expressions in the method are executed instead. These 
	expressions are evaluated as though the primitive routine had not been 
	called.  
	  
	The Smalltalk code that is evaluated when a primitive fails usually 
	anticipates why that primitive might fail. If the primitive is optional, the 
	expressions in the method do exactly what the primitive would have done (See 
	Number @). If the primitive only works on certain classes of arguments, the 
	Smalltalk code tries to coerce the argument or appeals to a superclass to find 
	a more general way of doing the operation (see SmallInteger +). If the 
	primitive is never supposed to fail, the expressions signal an error (see 
	SmallInteger asFloat).  
	  
	Each method that specifies a primitive has a comment in it. If the primitive is 
	optional, the comment will say 'Optional'. An optional primitive that is not 
	implemented always fails, and the Smalltalk expressions do the work 
	instead.  
	 
	If a primitive is not optional, the comment will say, 'Essential'. Some 
	methods will have the comment, 'No Lookup'. See Object 
	howToModifyPrimitives for an explanation of special selectors which are 
	not looked up.  
	  
	For the primitives for +, -, *, and bitShift: in SmallInteger, and truncated 
	in Float, the primitive constructs and returns a 16-bit 
	LargePositiveInteger when the result warrants it. Returning 16-bit 
	LargePositiveIntegers from these primitives instead of failing is 
	optional in the same sense that the LargePositiveInteger arithmetic 
	primitives are optional. The comments in the SmallInteger primitives say, 
	'Fails if result is not a SmallInteger', even though the implementor has the 
	option to construct a LargePositiveInteger. For further information on 
	primitives, see the 'Primitive Methods' part of the chapter on the formal 
	specification of the interpreter in the Smalltalk book."

	self error: 'comment only'! !

!Object class methodsFor: 'private'!

initializeDependentsFields
	DependentsFields _ IdentityDictionary new: 4

	"Object initializeDependentsFields"!

initializeErrorRecursion
	ErrorRecursion _ false

	"Object initializeErrorRecursion"! !

Object initialize!
