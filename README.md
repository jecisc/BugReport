# BugReport

I provide some utilities to dump bug reports in Pharo applications.

- [Installation](#installation)
- [Documentation](#documentation)
- [Version management](#version-management)
- [Smalltalk versions compatibility](#smalltalk-versions-compatibility)
- [Contact](#contact)

## Installation

To install the project in your Pharo image execute:

```Smalltalk
    Metacello new
    	githubUser: 'jecisc' project: 'BugReport' commitish: 'v1.x.x' path: 'src';
    	baseline: 'BugReport';
    	load
```

To add it to your baseline:

```Smalltalk
    spec
    	baseline: 'BugReport'
    	with: [ spec repository: 'github://jecisc/BugReport:v1.x.x/src' ]
```

Note that you can replace the #v1.x.x by another branch such as #development or a tag such as #v1.0.0, #v1.? or #v1.1.?.


## Documentation

It is possible to export a bug report in a string.

```Smalltalk
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
```

It is also possible to export a bug report in a dump file.

```Smalltalk
[ (Set withAll: #(1 2 3 4)) first ]
	on: Error
	do: [ :ex |
		FileBugReporter new
			context: thisContext;
			description: ex description;
			file: 'report.txt' asFileReference;
			write
		]
```

Or in an easier way.

```Smalltalk
[ (Set withAll: #(1 2 3 4)) first ] on: Error do: [ :ex | ex dumpStackFor: thisContext ].

"or"

[ (Set withAll: #(1 2 3 4)) first ] on: Error do: [ :ex | ex dumpStack ].
```

## Version management 

This project use semantic versioning to define the releases. This means that each stable release of the project will be assigned a version number of the form `vX.Y.Z`. 

- **X** defines the major version number
- **Y** defines the minor version number 
- **Z** defines the patch version number

When a release contains only bug fixes, the patch number increases. When the release contains new features that are backward compatible, the minor version increases. When the release contains breaking changes, the major version increases. 

Thus, it should be safe to depend on a fixed major version and moving minor version of this project.

## Smalltalk versions compatibility

| Version 	| Compatible Pharo versions 		|
|-------------	|---------------------------	|
| 1.x.x       	| Pharo 61, 70, 80				|

## Contact

If you have any questions or problems do not hesitate to open an issue or contact cyril (a) ferlicot.me 
