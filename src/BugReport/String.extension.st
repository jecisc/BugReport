Extension { #name : #String }

{ #category : #'*BugReport' }
String >> withSystemLineEnding [
	^ self withLineEndings: OSPlatform current lineEnding
]
