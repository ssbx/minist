Object subclass: #Rectangle
	instanceVariableNames: 'origin corner '
	classVariableNames: ''
	poolDictionaries: ''
	category: 'Graphics-Primitives'!

Rectangle comment: 'comment'!

!Rectangle methodsFor: 'accessing'!

test
    " a := a := b ."
    b := a + b - c * d.
    a.
    a b c + x + a d e f  !

othertest
    | a b c |
    a := 'hello'.
    a := 'hello'.
    a := 'hello'.
    a := 'hello'.
    y put: c .
    ^a b c !

test: a put: b
    ^c b + c at: x y put: y / a  !

othertest
    a + b put: c + e at: x .
    ^a !

cascade
    a b c; b; d; c; + y; at: x put: this.
    a + b; - c .
    a + b + c; - c .
    y at: x put: y ; f; a; + x; at: x put: y and: z .
    a + b !

+ a
    y put: c asString + E at: y + b.
    ^ c put: a !
!

"========================" !

Rectangle class
    instanceVariableNames: '' !

!Rectangle class methodsFor: 'accessing'!

test: a put: b
    ^c b + c at: x y put: y / a  !

blocks
    a := [ a b + c ] .

    a := [ a b + c ] value: x ; value; + x .
    a := [ | a b c | a b + c ] + y .
    ^ [ a b c + d at: x put:y. a b c] !

parentheses
    a := ( a + b ) value .
    ^ ((a + c) b) + 3 .
    d ifTrue: [areas add: (origin corner: corner x @ (yOrigin _ aRectangle origin y))]
!

colonvar
    a := [ :colonvar | a b . colonvar + x ] .
    a := [ :colonvar | | a b c | a b . colonvar + x ] .
    a := [ :vx :vy :vyy | | a b c | a b . colonvar + x ] .
    (v + 2) ifFalse: ([^ a b] value).

!
numbers
    a := b + 2 * 36 - 2 + -3.
    (a + b) - (b + a) .
!

primtest
    <primitive: 43>
    self primitiveFailed .
!

strunit
    'lqsdkfj ' printOn: something .
    a printThis: 'lhfdlkj', a , b .
!

chars
    $a at: $b v put: $c + 3.
    this w: $( w: $) w: $+ w: ${ w: $-
        w: $\
        w: $/
        w: $_
        w: $@
        w: $[
        w: $]
    .
    $( printOn: aStream with: $+ .
!


yourself
    ^ a | b
    "empty method"
    !
tt
    [:field | copyOK & (unmoved at: field)] !

arr
    ^ #( $a $b $c $d $e ( $c $d $e) ($f $g $h) ) !
!

