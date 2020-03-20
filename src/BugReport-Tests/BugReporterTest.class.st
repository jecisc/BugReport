"
A BugReporterTest is a test class for testing the behavior of BugReporter
"
Class {
	#name : #BugReporterTest,
	#superclass : #TestCase,
	#instVars : [
		'reporter'
	],
	#category : #'BugReport-Tests'
}

{ #category : #helpers }
BugReporterTest >> actualClass [
	^ BugReporter
]

{ #category : #tests }
BugReporterTest >> testNewSectionNamed [
	reporter := BugReporter new.
	reporter stream: '' writeStream.

	reporter newSectionNamed: 'shortLabel'.

	self
		assert: reporter stream contents
		equals:
			'

shortLabel
' withSystemLineEnding
]

{ #category : #tests }
BugReporterTest >> testWriteCurrentContext [
	[ BugReporterMock new crash ]
		on: Error
		do: [ :ex | 
			reporter := BugReporter new
				context: thisContext;
				description: ex description;
				stream: '' writeStream;
				moveToFirstInterestingContext;
				writeCurrentContext;
				yourself ].

	self
		assert: reporter stream contents
		equals:
			'
----------------------------------------------------------------------------------------------------
BugReporterMock>>crash


receiver
	                              a BugReporterMock


temps
	temp1                         1
	temp2                         2


inst vars
	a                             true
	instanceVariableWithAReallyLongNameToMessUpWithFormating
	                              false
'
				withSystemLineEnding
]

{ #category : #tests }
BugReporterTest >> testWriteDescription [
	[ BugReporterMock new crash ]
		on: Error
		do: [ :ex | 
			reporter := BugReporter new
				context: thisContext;
				description: ex description;
				stream: '' writeStream;
				writeDescription;
				yourself ].

	self
		assert: reporter stream contents
		equals:
			'Error: Error!
' withSystemLineEnding
]

{ #category : #tests }
BugReporterTest >> testWriteObjectLabelled [
	reporter := BugReporter new.
	reporter stream: '' writeStream.

	reporter writeObject: true labelled: 'shortLabel'.

	self
		assert: reporter stream contents
		equals:
			'	shortLabel                    true
' withSystemLineEnding
]

{ #category : #tests }
BugReporterTest >> testWriteObjectLabelledWithLongLabel [
	reporter := BugReporter new.
	reporter stream: '' writeStream.

	reporter writeObject: true labelled: 'reallyLongLabelThatShouldPushTheValueToTheNextLine'.

	self
		assert: reporter stream contents
		equals:
			'	reallyLongLabelThatShouldPushTheValueToTheNextLine
	                              true
' withSystemLineEnding
]

{ #category : #tests }
BugReporterTest >> testWriteReceiverForCurrentContext [
	[ BugReporterMock new crash ]
		on: Error
		do: [ :ex | 
			reporter := BugReporter new
				context: thisContext;
				description: ex description;
				stream: '' writeStream;
				moveToFirstInterestingContext;
				writeReceiverForCurrentContext;
				yourself ].

	self
		assert: reporter stream contents
		equals:
			'

receiver
	                              a BugReporterMock
' withSystemLineEnding
]

{ #category : #tests }
BugReporterTest >> testWriteReceiverInstanceVariablesForCurrentContext [
	[ BugReporterMock new crash ]
		on: Error
		do: [ :ex | 
			reporter := BugReporter new
				context: thisContext;
				description: ex description;
				stream: '' writeStream;
				moveToFirstInterestingContext;
				writeReceiverInstanceVariablesForCurrentContext;
				yourself ].

	self
		assert: reporter stream contents
		equals:
			'

inst vars
	a                             true
	instanceVariableWithAReallyLongNameToMessUpWithFormating
	                              false
' withSystemLineEnding
]

{ #category : #tests }
BugReporterTest >> testWriteTemporariesForCurrentContext [
	[ BugReporterMock new crash ]
		on: Error
		do: [ :ex | 
			reporter := BugReporter new
				context: thisContext;
				description: ex description;
				stream: '' writeStream;
				moveToFirstInterestingContext;
				writeTemporariesForCurrentContext;
				yourself ].

	self
		assert: reporter stream contents
		equals:
			'

temps
	temp1                         1
	temp2                         2
' withSystemLineEnding
]
