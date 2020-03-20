"
Description
--------------------

I am a class implementing a way to report crashes in a Pharo application.

I am initialized with the context of an error and a description and I'll write a report on a stream. 

My subclasses should implement the way to export the result.

Examples
--------------------

	[ (Set withAll: #(1 2 3 4)) first ]
		on: Error
		do: [ :ex |
			String streamContents: [ :s | 
				BugReporter new
					context: thisContext;
					description: ex description;
					stream: s;
					write
				 ] ]		
 
Internal Representation and Key Implementation Points.
--------------------

    Instance Variables
	currentContext:				<aContext>	The current context been exported.
	description:					<aString>		A description of the error.
	stream:						<aStream>		A stream on which write the report.

"
Class {
	#name : #BugReporter,
	#superclass : #Object,
	#instVars : [
		'description',
		'stream',
		'originalContext',
		'currentContext'
	],
	#category : #BugReport
}

{ #category : #accessing }
BugReporter >> context: anObject [
	currentContext := anObject
]

{ #category : #accessing }
BugReporter >> description [
	^ description
]

{ #category : #accessing }
BugReporter >> description: anObject [
	description := anObject
]

{ #category : #private }
BugReporter >> movePastMessageNotUnderstood [
	| nextContext |
	nextContext := currentContext.
	[ nextContext isNil or: [ nextContext printString includesSubstring: 'doesNotUnderstand:' ] ] whileFalse: [ nextContext := nextContext sender ].
	nextContext ifNotNil: [ currentContext := nextContext ]
]

{ #category : #private }
BugReporter >> moveToContextRaising: anError [
	"Reduce the context size up to where the error was raised"

	| nextContext |
	nextContext := currentContext.
	[ nextContext isNotNil and: [ (nextContext stack includes: anError) or: [ nextContext method classBinding value includesBehavior: Exception ] ] ]
		whileTrue: [ nextContext := nextContext sender ].
	nextContext ifNotNil: [ currentContext := nextContext ]
]

{ #category : #writing }
BugReporter >> moveUp [
	currentContext := currentContext sender
]

{ #category : #writing }
BugReporter >> moveUpWhileReceiverIs: anObject [
	[ currentContext isNotNil and: [ currentContext receiver == anObject ] ] whileTrue: [ self moveUp ]
]

{ #category : #writing }
BugReporter >> moveUpWhileReceiverIsException [
	"Making the stack more readable by remnoving the traces located in the exceptions."

	[ currentContext isNil or: [ currentContext receiver isException ] ] whileFalse: [ self moveUp ]
]

{ #category : #writing }
BugReporter >> newLine [
	stream nextPutAll: OSPlatform current lineEnding
]

{ #category : #writing }
BugReporter >> newSectionNamed: title [
	self
		newLine;
		newLine.
	stream nextPutAll: title.
	self newLine
]

{ #category : #accessing }
BugReporter >> stream [
	^ stream
]

{ #category : #accessing }
BugReporter >> stream: anObject [
	stream := anObject
]

{ #category : #writing }
BugReporter >> write [
	| error |
	self assert: stream isNotNil.
	self writeDescription.

	currentContext ifNil: [ ^ self ].

	self moveUpWhileReceiverIsException.

	error := currentContext ifNotNil: #receiver.

	"Make stack more readable by removing the traces in the error raised."
	self moveUpWhileReceiverIs: error.

	(self description beginsWith: 'MNU') ifFalse: [ self moveToContextRaising: error ] ifTrue: [ self movePastMessageNotUnderstood ].

	[ currentContext isNil ]
		whileFalse: [ self writeCurrentContext.
			self moveUp ]
]

{ #category : #writing }
BugReporter >> writeCurrentContext [
	self newLine.
	100 timesRepeat: [ stream nextPut: $- ].
	self newLine.
	stream print: currentContext.
	self
		newLine;
		writeReceiverForCurrentContext;
		writeTemporariesForCurrentContext;
		writeReceiverInstanceVariablesForCurrentContext
]

{ #category : #writing }
BugReporter >> writeDescription [
	self description ifNil: [ ^ self ].

	stream nextPutAll: self description.
	self newLine
]

{ #category : #writing }
BugReporter >> writeObject: anObject labelled: aLabel [
	| max |
	max := 30.
	stream
		tab;
		nextPutAll: aLabel.
	aLabel size < max
		ifTrue: [ max - aLabel size timesRepeat: [ stream space ] ]
		ifFalse: [ self newLine.
			stream tab.
			max timesRepeat: [ stream space ] ].
	stream nextPutAll: (anObject printStringLimitedTo: 200).
	self newLine
]

{ #category : #writing }
BugReporter >> writeReceiverForCurrentContext [
	self newSectionNamed: 'receiver'.
	self writeObject: (currentContext ifNotNil: #receiver) labelled: ''
]

{ #category : #writing }
BugReporter >> writeReceiverInstanceVariablesForCurrentContext [
	currentContext receiver
		ifNotNil: [ :receiver | 
			receiver class instSize > 0
				ifTrue: [ self newSectionNamed: 'inst vars'.
					receiver class allInstVarNames withIndexDo: [ :name :index | self writeObject: (receiver instVarAt: index) labelled: name ] ] ]
]

{ #category : #writing }
BugReporter >> writeTemporariesForCurrentContext [
	self newSectionNamed: 'temps'.
	[ currentContext tempNames withIndexDo: [ :name :index | self writeObject: (currentContext tempAt: index) labelled: name ] ]
		on: Error
		do: [ :ex | 
			"The above fails if source code is not available"
			1 to: 100 do: [ :index | 
				"Show temp names at tn, giving up on the first error"
				[ self writeObject: (currentContext tempAt: index) labelled: 't' , index printString ]
					on: Error
					do: [ :ex2 | ^ self ] ] ]
]
