"
I am a mock used to test BugReporter.
"
Class {
	#name : #BugReporterMock,
	#superclass : #Object,
	#instVars : [
		'a',
		'instanceVariableWithAReallyLongNameToMessUpWithFormating'
	],
	#category : #'BugReport-Tests'
}

{ #category : #initialization }
BugReporterMock >> crash [
	| temp1 temp2 |
	temp1 := 1.
	temp2 := 2.

	Error new signal: 'Error!'.

	^ temp1 + temp2
]

{ #category : #initialization }
BugReporterMock >> initialize [
	super initialize.
	a := true.
	instanceVariableWithAReallyLongNameToMessUpWithFormating := false
]
