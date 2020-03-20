Extension { #name : #Exception }

{ #category : #'*BugReport' }
Exception >> dumpStack [
	^ self
		dumpStackFor:
			([ self signalerContext ifNil: [ thisContext ] ]
				on: Error
				do: [ :ex | thisContext ])
]

{ #category : #'*BugReport' }
Exception >> dumpStackFor: aContext [
	^ (DateAndTime now asFileNameCompatibleString , '.' , UUID new printString , '.dump') asFileReference
		in: [ :file | FileBugReporter dump: aContext in: file description: self description ];
		yourself
]

{ #category : #'*BugReport' }
Exception >> isException [
	^ true
]
