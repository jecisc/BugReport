"
I am a baseline to define the structure and dependencies of the BugReport project.
"
Class {
	#name : #BaselineOfBugReport,
	#superclass : #BaselineOf,
	#category : #BaselineOfBugReport
}

{ #category : #baseline }
BaselineOfBugReport >> baseline: spec [
	<baseline>
	spec
		for: #common
		do: [
			"Packages"
			spec
				package: 'BugReport'";
				package: 'BugReport-Tests' with: [ spec requires: #('BugReport') ]".

			"Groups"
"			spec
				group: 'Core' with: #('BugReport');
				group: 'Tests' with: #('BugReport-Tests')" ].
]

{ #category : #accessing }
BaselineOfBugReport >> projectClass [
	^ MetacelloCypressBaselineProject
]
