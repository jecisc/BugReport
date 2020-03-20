"
Description
--------------------

I am a bug reporter printing the error in a file.

Examples
--------------------

	[ (Set withAll: #(1 2 3 4)) first ]
		on: Error
		do: [ :ex |
			BugReporter new
				context: thisContext;
				description: ex description;
				file: 'report.txt' asFileReference;
				write
			]		
				
	[ (Set withAll: #(1 2 3 4)) first ]
		on: Error
		do: [ :ex |ex dumpStackFor: thisContext ]
 
Internal Representation and Key Implementation Points.
--------------------

    Instance Variables
	file:		<aFileReference>	The file in which we should write the error.

"
Class {
	#name : #FileBugReporter,
	#superclass : #BugReporter,
	#instVars : [
		'file'
	],
	#category : #BugReport
}

{ #category : #'instance creation' }
FileBugReporter class >> dump: aContext in: aFile description: aString [
	^ self new
		context: aContext;
		description: aString;
		file: aFile;
		export
]

{ #category : #services }
FileBugReporter >> export [
	self file
		writeStreamDo: [ :aStream | 
			stream := aStream.
			[ self execute: [ self write ] recordedAs: 'Dump stack in ' , self file basename ]
				on: Error
				do: [ :ex | 
					3 timesRepeat: [ self newLine ].
					stream
						nextPutAll: 'Error - ';
						nextPutAll: self file basename;
						nextPutAll: ' - ';
						nextPutAll: DateAndTime now printString;
						nextPutAll: ' Error while dumping the stack: ';
						nextPutAll: ex description.
					('Error while dumping the stack: ' , ex description) record.
					ex signalerContext longStack record ].
			stream := nil ]
]

{ #category : #accessing }
FileBugReporter >> file [
	^ file
]

{ #category : #accessing }
FileBugReporter >> file: anObject [
	file := anObject
]
