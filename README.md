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

<details>
  <summary>Example of result</summary>

```
MessageNotUnderstood: Set>> #first

----------------------------------------------------------------------------------------------------
Set(Object)>>doesNotUnderstand: #first


receiver
	                              a Set(1 2 3 4)


temps
	aMessage                      first
	RFReifyValueVar               nil
	RFArgumentsReificationVar     an Array(first)
	exception                     MessageNotUnderstood: Set>> #first
	resumeValue                   nil


inst vars
	tally                         4
	array                         #(nil 1 2 3 4 nil nil)

----------------------------------------------------------------------------------------------------
[ (Set withAll: #(1 2 3 4)) first ] in BugReporter class>>DoIt


receiver
	                              BugReporter


temps


inst vars
	superclass                    Object
	methodDict                    a MethodDictionary(#context:->BugReporter>>#context: #description->BugReporter>>#description #description:->BugReporter>>#description: #movePastMessageNotUnderstood->BugReporter>>#movePastMessageNotUn...etc...
	format                        65539
	layout                        a FixedLayout
	organization                  a ClassOrganization
	subclasses                    {FileBugReporter}
	name                          #BugReporter
	classPool                     a Dictionary()
	sharedPools                   an OrderedCollection()
	environment                   a SystemDictionary(lots of globals)
	category                      #BugReport

----------------------------------------------------------------------------------------------------
BlockClosure>>on:do:


receiver
	                              [ (Set withAll: #(1 2 3 4)) first ]


temps
	exception                     Error
	handlerAction                 [ :ex | 
String
	streamContents: [ :s | 
		BugReporter new
			context: thisContext;
			description: ex description;
			stream: s;
			write ] ]


inst vars
	outerContext                  BugReporter class>>DoIt
	startpc                       141
	numArgs                       0

----------------------------------------------------------------------------------------------------
BugReporter class>>DoIt


receiver
	                              BugReporter


temps


inst vars
	superclass                    Object
	methodDict                    a MethodDictionary(#context:->BugReporter>>#context: #description->BugReporter>>#description #description:->BugReporter>>#description: #movePastMessageNotUnderstood->BugReporter>>#movePastMessageNotUn...etc...
	format                        65539
	layout                        a FixedLayout
	organization                  a ClassOrganization
	subclasses                    {FileBugReporter}
	name                          #BugReporter
	classPool                     a Dictionary()
	sharedPools                   an OrderedCollection()
	environment                   a SystemDictionary(lots of globals)
	category                      #BugReport

----------------------------------------------------------------------------------------------------
OpalCompiler>>evaluate


receiver
	                              an OpalCompiler


temps
	value                         nil


inst vars
	ast                           DoIt
	^ [ (Set withAll: #(1 2 3 4)) first ]
		on: Error
		do: [ :ex | 
			String
				streamContents: [ :s | 
					BugReporter new
						context: thisContext;
						description: ex description;
						s...etc...
	source                        a ReadStream
	context                       nil
	receiver                      BugReporter
	compilationContext            a CompilationContext
	compilationContextClass       nil

----------------------------------------------------------------------------------------------------
ClyTextEditor(RubSmalltalkEditor)>>evaluate:andDo:


receiver
	                              a ClyTextEditor


temps
	aStream                       a ReadWriteStream
	aBlock                        [ :result | result inspect ]
	result                        nil
	rcvr                          BugReporter
	ctxt                          nil


inst vars
	defaultKeymappingIndex        nil
	textArea                      a RubEditingArea(620650752)
	findReplaceService            nil
	selectorChooser               nil
	notificationStrategy          a RubTextInsertionStrategy
	completionEngine              a CompletionEngine

----------------------------------------------------------------------------------------------------
ClyTextEditor(RubSmalltalkEditor)>>evaluateSelectionAndDo:


receiver
	                              a ClyTextEditor


temps
	aBlock                        [ :result | result inspect ]


inst vars
	defaultKeymappingIndex        nil
	textArea                      a RubEditingArea(620650752)
	findReplaceService            nil
	selectorChooser               nil
	notificationStrategy          a RubTextInsertionStrategy
	completionEngine              a CompletionEngine

----------------------------------------------------------------------------------------------------
ClyTextEditor(RubSmalltalkEditor)>>inspectIt


receiver
	                              a ClyTextEditor


temps


inst vars
	defaultKeymappingIndex        nil
	textArea                      a RubEditingArea(620650752)
	findReplaceService            nil
	selectorChooser               nil
	notificationStrategy          a RubTextInsertionStrategy
	completionEngine              a CompletionEngine

----------------------------------------------------------------------------------------------------
ClyTextEditor(RubSmalltalkEditor)>>inspectIt:


receiver
	                              a ClyTextEditor


temps
	aKeyboardEvent                nil


inst vars
	defaultKeymappingIndex        nil
	textArea                      a RubEditingArea(620650752)
	findReplaceService            nil
	selectorChooser               nil
	notificationStrategy          a RubTextInsertionStrategy
	completionEngine              a CompletionEngine

----------------------------------------------------------------------------------------------------
[ :target | target editor inspectIt: nil ] in RubSmalltalkEditor class>>buildShortcutsOn:


receiver
	                              RubSmalltalkEditor


temps
	aBuilder                      a RubEditingArea(620650752)
	t1                            a RubEditingArea(620650752)


inst vars
	superclass                    RubTextEditor
	methodDict                    a MethodDictionary(size 103)
	format                        65542
	layout                        a FixedLayout
	organization                  a ClassOrganization
	subclasses                    {ClyTextEditor}
	name                          #RubSmalltalkEditor
	classPool                     a Dictionary(#CompletionEngineClass->CompletionEngine )
	sharedPools                   an OrderedCollection()
	environment                   a SystemDictionary(lots of globals)
	category                      #'Rubric-Editing-Code'

----------------------------------------------------------------------------------------------------
BlockClosure>>cull:


receiver
	                              [ :target | target editor inspectIt: nil ]


temps
	anArg                         a RubEditingArea(620650752)


inst vars
	outerContext                  RubSmalltalkEditor class>>buildShortcutsOn:
	startpc                       567
	numArgs                       1

----------------------------------------------------------------------------------------------------
BlockClosure>>cull:cull:


receiver
	                              [ :target | target editor inspectIt: nil ]


temps
	firstArg                      a RubEditingArea(620650752)
	secondArg                     a RubEditingArea(620650752)


inst vars
	outerContext                  RubSmalltalkEditor class>>buildShortcutsOn:
	startpc                       567
	numArgs                       1

----------------------------------------------------------------------------------------------------
BlockClosure>>cull:cull:cull:


receiver
	                              [ :target | target editor inspectIt: nil ]


temps
	firstArg                      a RubEditingArea(620650752)
	secondArg                     a RubEditingArea(620650752)
	thirdArg                      [keystroke '<Cmd-i>']


inst vars
	outerContext                  RubSmalltalkEditor class>>buildShortcutsOn:
	startpc                       567
	numArgs                       1

----------------------------------------------------------------------------------------------------
KMCategoryBinding>>completeMatch:buffer:


receiver
	                              aKMCategoryTarget(#RubSmalltalkEditor)


temps
	aKeymap                       #inspectIt on Meta + i do [ :target | target editor inspectIt: nil ]
	aBuffer                       an OrderedCollection([keystroke '<Cmd-i>'])


inst vars
	target                        a RubEditingArea(620650752)
	category                      a KMCategory
	morph                         a RubEditingArea(620650752)

----------------------------------------------------------------------------------------------------
[ :l | l completeMatch: self buffer: aBuffer ] in KMKeymap>>notifyCompleteMatchTo:buffer:


receiver
	                              #inspectIt on Meta + i do [ :target | target editor inspectIt: nil ]


temps
	aListener                     aKMCategoryTarget(#RubSmalltalkEditor)
	aBuffer                       an OrderedCollection([keystroke '<Cmd-i>'])
	t1                            aKMCategoryTarget(#RubSmalltalkEditor)
	t2                            an OrderedCollection([keystroke '<Cmd-i>'])


inst vars
	action                        [ :target | target editor inspectIt: nil ]
	name                          #inspectIt
	shortcut                      Meta + i
	defaultShortcut               Meta + i
	description                   'Inspect it'

----------------------------------------------------------------------------------------------------
Array(SequenceableCollection)>>do:


receiver
	                              an Array(aKMCategoryTarget(#RubSmalltalkEditor) a CmdKMDispatcher)


temps
	aBlock                        [ :l | l completeMatch: self buffer: aBuffer ]
	index                         2

----------------------------------------------------------------------------------------------------
KMKeymap>>notifyCompleteMatchTo:buffer:


receiver
	                              #inspectIt on Meta + i do [ :target | target editor inspectIt: nil ]


temps
	aListener                     an Array(aKMCategoryTarget(#RubSmalltalkEditor) a CmdKMDispatcher)
	aBuffer                       an OrderedCollection([keystroke '<Cmd-i>'])
	listeners                     an Array(aKMCategoryTarget(#RubSmalltalkEditor) a CmdKMDispatcher)


inst vars
	action                        [ :target | target editor inspectIt: nil ]
	name                          #inspectIt
	shortcut                      Meta + i
	defaultShortcut               Meta + i
	description                   'Inspect it'

----------------------------------------------------------------------------------------------------
KMKeymap>>onMatchWith:notify:andDo:


receiver
	                              #inspectIt on Meta + i do [ :target | target editor inspectIt: nil ]


temps
	anEventBuffer                 an OrderedCollection([keystroke '<Cmd-i>'])
	aMatchListener                an Array(aKMCategoryTarget(#RubSmalltalkEditor) a CmdKMDispatcher)
	anAction                      [ ^ self ]


inst vars
	action                        [ :target | target editor inspectIt: nil ]
	name                          #inspectIt
	shortcut                      Meta + i
	defaultShortcut               Meta + i
	description                   'Inspect it'

----------------------------------------------------------------------------------------------------
[ :entry | entry onMatchWith: anEventBuffer notify: aMatchListener andDo: aBlock ] in KMCategory>>onMatchWith:notify:andDo:


receiver
	                              a KMCategory


temps
	anEventBuffer                 #inspectIt on Meta + i do [ :target | target editor inspectIt: nil ]
	aMatchListener                [ ^ self ]
	aBlock                        an Array(aKMCategoryTarget(#RubSmalltalkEditor) a CmdKMDispatcher)
	entry                         an OrderedCollection([keystroke '<Cmd-i>'])


inst vars
	name                          #RubSmalltalkEditor
	platforms                     a Dictionary(#MacOSX->a KMStorage #Windows->a KMStorage #all->a KMStorage )

----------------------------------------------------------------------------------------------------
Set>>do:


receiver
	                              a Set(#widenSelectionOfIt on Meta + 2 do [ :target | target editor widenSelectionOfIt ]
 #implementorsOfIt on Meta + m do [ :target | target editor implementorsOfIt: nil ]
 #jumpToNextKeywordOfIt on S...etc...


temps
	aBlock                        [ :entry | entry onMatchWith: anEventBuffer notify: aMatchListener andDo: aBlock ]
	index                         23
	each                          #inspectIt on Meta + i do [ :target | target editor inspectIt: nil ]


inst vars
	tally                         14
	array                         an Array(nil #widenSelectionOfIt on Meta + 2 do [ :target | target editor widenSelectionOfIt ]
 #implementorsOfIt on Meta + m do [ :target | target editor implementorsOfIt: nil ]
 nil #jumpToNextKeywo...etc...

----------------------------------------------------------------------------------------------------
KMCategory>>onMatchWith:notify:andDo:


receiver
	                              a KMCategory


temps
	anEventBuffer                 an OrderedCollection([keystroke '<Cmd-i>'])
	aMatchListener                an Array(aKMCategoryTarget(#RubSmalltalkEditor) a CmdKMDispatcher)
	aBlock                        [ ^ self ]


inst vars
	name                          #RubSmalltalkEditor
	platforms                     a Dictionary(#MacOSX->a KMStorage #Windows->a KMStorage #all->a KMStorage )

----------------------------------------------------------------------------------------------------
KMCategoryBinding>>verifyMatchWith:notifying:thenDoing:


receiver
	                              aKMCategoryTarget(#RubSmalltalkEditor)


temps
	anEventBuffer                 an OrderedCollection([keystroke '<Cmd-i>'])
	aListener                     a CmdKMDispatcher
	anAction                      [ ^ self ]


inst vars
	target                        a RubEditingArea(620650752)
	category                      a KMCategory
	morph                         a RubEditingArea(620650752)

----------------------------------------------------------------------------------------------------
[ :aTarget | "nice hack to stop in the first listener" aTarget verifyMatchWith: anEventBuffer notifying: self thenDoing: [ ^ self ] ] in CmdKMDispatcher(KMDispatcher)>>dispatch:


receiver
	                              a CmdKMDispatcher


temps
	anEventBuffer                 aKMCategoryTarget(#RubSmalltalkEditor)
	aTarget                       an OrderedCollection([keystroke '<Cmd-i>'])


inst vars
	target                        a KMTarget
	currentEvent                  nil
	targets                       a Set(aKMCategoryTarget(#MorphFocusCtrlNavigation) aKMCategoryTarget(#RubTextEditor) aKMCategoryTarget(#RubSmalltalkEditor))
	morph                         a RubEditingArea(620650752)
	directKeymaps                 a KMCategory
	commandProvider               a ClyClassCommentEditorToolMorph(1007235072)

----------------------------------------------------------------------------------------------------
OrderedCollection>>do:


receiver
	                              an OrderedCollection(aKMCategoryTarget(nil) aKMCategoryTarget(#MorphFocusCtrlNavigation) aKMCategoryTarget(#RubTextEditor) aKMCategoryTarget(#RubSmalltalkEditor))


temps
	aBlock                        [ :aTarget | "nice hack to stop in the first listener" aTarget verifyMatchWith: anEventBuffer notifying: self thenDoing: [ ^ self ] ]
	index                         4


inst vars
	array                         an Array(aKMCategoryTarget(nil) aKMCategoryTarget(#MorphFocusCtrlNavigation) aKMCategoryTarget(#RubTextEditor) aKMCategoryTarget(#RubSmalltalkEditor) nil nil nil nil nil nil)
	firstIndex                    1
	lastIndex                     4

----------------------------------------------------------------------------------------------------
CmdKMDispatcher(KMDispatcher)>>dispatch:


receiver
	                              a CmdKMDispatcher


temps
	anEventBuffer                 an OrderedCollection([keystroke '<Cmd-i>'])


inst vars
	target                        a KMTarget
	currentEvent                  nil
	targets                       a Set(aKMCategoryTarget(#MorphFocusCtrlNavigation) aKMCategoryTarget(#RubTextEditor) aKMCategoryTarget(#RubSmalltalkEditor))
	morph                         a RubEditingArea(620650752)
	directKeymaps                 a KMCategory
	commandProvider               a ClyClassCommentEditorToolMorph(1007235072)

----------------------------------------------------------------------------------------------------
CmdKMDispatcher>>dispatch:


receiver
	                              a CmdKMDispatcher


temps
	executed                      an OrderedCollection([keystroke '<Cmd-i>'])
	anEventBuffer                 a ClyTextEditorContext
	context                       #(false)


inst vars
	target                        a KMTarget
	currentEvent                  nil
	targets                       a Set(aKMCategoryTarget(#MorphFocusCtrlNavigation) aKMCategoryTarget(#RubTextEditor) aKMCategoryTarget(#RubSmalltalkEditor))
	morph                         a RubEditingArea(620650752)
	directKeymaps                 a KMCategory
	commandProvider               a ClyClassCommentEditorToolMorph(1007235072)

----------------------------------------------------------------------------------------------------
KMTarget>>dispatch:


receiver
	                              a KMTarget


temps
	buffer                        an OrderedCollection([keystroke '<Cmd-i>'])


inst vars
	morph                         a RubEditingArea(620650752)
	targetSelector                #yourself

----------------------------------------------------------------------------------------------------
[ :targetToDispatch | 
targetToDispatch dispatch: KMBuffer uniqueInstance buffer copy.
aKeyboardEvent wasHandled ifTrue: [ ^ self ] ] in KMDispatchChain>>dispatch:


receiver
	                              a KMDispatchChain


temps
	aKeyboardEvent                a KMTarget
	targetToDispatch              [keystroke '<Cmd-i>']


inst vars
	target                        a KMTarget
	dispatcher                    a CmdKMDispatcher
	initialTarget                 a KMGlobalDispatcher

----------------------------------------------------------------------------------------------------
KMDispatchChain>>do:


receiver
	                              a KMDispatchChain


temps
	aBlock                        [ :targetToDispatch | 
targetToDispatch dispatch: KMBuffer uniqueInstance buffer copy.
aKeyboardEvent wasHandled ifTrue: [ ^ self ] ]
	currentTarget                 a KMTarget


inst vars
	target                        a KMTarget
	dispatcher                    a CmdKMDispatcher
	initialTarget                 a KMGlobalDispatcher

----------------------------------------------------------------------------------------------------
KMDispatchChain>>dispatch:


receiver
	                              a KMDispatchChain


temps
	aKeyboardEvent                [keystroke '<Cmd-i>']


inst vars
	target                        a KMTarget
	dispatcher                    a CmdKMDispatcher
	initialTarget                 a KMGlobalDispatcher

----------------------------------------------------------------------------------------------------
CmdKMDispatcher(KMDispatcher)>>dispatchKeystroke:


receiver
	                              a CmdKMDispatcher


temps
	aKeyEvent                     [keystroke '<Cmd-i>']
	chain                         a KMDispatchChain


inst vars
	target                        a KMTarget
	currentEvent                  nil
	targets                       a Set(aKMCategoryTarget(#MorphFocusCtrlNavigation) aKMCategoryTarget(#RubTextEditor) aKMCategoryTarget(#RubSmalltalkEditor))
	morph                         a RubEditingArea(620650752)
	directKeymaps                 a KMCategory
	commandProvider               a ClyClassCommentEditorToolMorph(1007235072)

----------------------------------------------------------------------------------------------------
RubEditingArea(Morph)>>dispatchKeystrokeForEvent:


receiver
	                              a RubEditingArea(620650752)


temps
	evt                           [keystroke '<Cmd-i>']


inst vars
	bounds                        (0@0) corner: (1875@601)
	owner                         a TransformWithLayoutMorph(952606208)
	submorphs                     an Array(a RubCursor(162526720) a RubPrimarySelectionMorph(1064620288))
	fullBounds                    (0@0) corner: (1875@601)
	color                         Color transparent
	extension                     a MorphExtension (242383104) [other:  (announcer -> an Announcer) (kmDispatcher -> a CmdKMDispatcher)]
	model                         a RubScrolledTextModel
	paragraph                     a RubShoutStylerDecorator
	editor                        a ClyTextEditor
	scrollPane                    a RubTextScrollPane(964899840)
	editingState                  a RubEditingState
	textStyle                     a TextStyle Source Code Pro
	textColor                     Color white
	margins                       a Margin top: 6 left: 6 bottom: 6 right: 6
	readOnly                      false
	menuAllowed                   nil
	editingMode                   a ClyTextEditingMode
	cursor                        a RubCursor(162526720)
	segments                      an OrderedCollection()
	getMenuPolicy                 nil
	mouseDownPoint                nil
	completionEngine              a CompletionEngine
	maxLength                     nil
	findReplaceService            a SpRubFindReplaceService
	wrapped                       true
	editorClass                   ClyTextEditor

----------------------------------------------------------------------------------------------------
RubEditingArea(RubAbstractTextArea)>>handleKeystroke:


receiver
	                              a RubEditingArea(620650752)


temps
	anEvent                       [keystroke '<Cmd-i>']


inst vars
	bounds                        (0@0) corner: (1875@601)
	owner                         a TransformWithLayoutMorph(952606208)
	submorphs                     an Array(a RubCursor(162526720) a RubPrimarySelectionMorph(1064620288))
	fullBounds                    (0@0) corner: (1875@601)
	color                         Color transparent
	extension                     a MorphExtension (242383104) [other:  (announcer -> an Announcer) (kmDispatcher -> a CmdKMDispatcher)]
	model                         a RubScrolledTextModel
	paragraph                     a RubShoutStylerDecorator
	editor                        a ClyTextEditor
	scrollPane                    a RubTextScrollPane(964899840)
	editingState                  a RubEditingState
	textStyle                     a TextStyle Source Code Pro
	textColor                     Color white
	margins                       a Margin top: 6 left: 6 bottom: 6 right: 6
	readOnly                      false
	menuAllowed                   nil
	editingMode                   a ClyTextEditingMode
	cursor                        a RubCursor(162526720)
	segments                      an OrderedCollection()
	getMenuPolicy                 nil
	mouseDownPoint                nil
	completionEngine              a CompletionEngine
	maxLength                     nil
	findReplaceService            a SpRubFindReplaceService
	wrapped                       true
	editorClass                   ClyTextEditor

----------------------------------------------------------------------------------------------------
KeyboardEvent>>sentTo:


receiver
	                              [keystroke '<Cmd-i>']


temps
	anObject                      a RubEditingArea(620650752)


inst vars
	timeStamp                     277123888
	source                        a HandMorph(677264640)
	windowIndex                   nil
	type                          #keystroke
	buttons                       64
	position                      (-23@234)
	handler                       nil
	wasHandled                    true
	keyValue                      105
	charCode                      105
	scanCode                      2

----------------------------------------------------------------------------------------------------
RubEditingArea(Morph)>>handleEvent:


receiver
	                              a RubEditingArea(620650752)


temps
	anEvent                       [keystroke '<Cmd-i>']


inst vars
	bounds                        (0@0) corner: (1875@601)
	owner                         a TransformWithLayoutMorph(952606208)
	submorphs                     an Array(a RubCursor(162526720) a RubPrimarySelectionMorph(1064620288))
	fullBounds                    (0@0) corner: (1875@601)
	color                         Color transparent
	extension                     a MorphExtension (242383104) [other:  (announcer -> an Announcer) (kmDispatcher -> a CmdKMDispatcher)]
	model                         a RubScrolledTextModel
	paragraph                     a RubShoutStylerDecorator
	editor                        a ClyTextEditor
	scrollPane                    a RubTextScrollPane(964899840)
	editingState                  a RubEditingState
	textStyle                     a TextStyle Source Code Pro
	textColor                     Color white
	margins                       a Margin top: 6 left: 6 bottom: 6 right: 6
	readOnly                      false
	menuAllowed                   nil
	editingMode                   a ClyTextEditingMode
	cursor                        a RubCursor(162526720)
	segments                      an OrderedCollection()
	getMenuPolicy                 nil
	mouseDownPoint                nil
	completionEngine              a CompletionEngine
	maxLength                     nil
	findReplaceService            a SpRubFindReplaceService
	wrapped                       true
	editorClass                   ClyTextEditor

----------------------------------------------------------------------------------------------------
RubEditingArea(Morph)>>handleFocusEvent:


receiver
	                              a RubEditingArea(620650752)


temps
	anEvent                       [keystroke '<Cmd-i>']


inst vars
	bounds                        (0@0) corner: (1875@601)
	owner                         a TransformWithLayoutMorph(952606208)
	submorphs                     an Array(a RubCursor(162526720) a RubPrimarySelectionMorph(1064620288))
	fullBounds                    (0@0) corner: (1875@601)
	color                         Color transparent
	extension                     a MorphExtension (242383104) [other:  (announcer -> an Announcer) (kmDispatcher -> a CmdKMDispatcher)]
	model                         a RubScrolledTextModel
	paragraph                     a RubShoutStylerDecorator
	editor                        a ClyTextEditor
	scrollPane                    a RubTextScrollPane(964899840)
	editingState                  a RubEditingState
	textStyle                     a TextStyle Source Code Pro
	textColor                     Color white
	margins                       a Margin top: 6 left: 6 bottom: 6 right: 6
	readOnly                      false
	menuAllowed                   nil
	editingMode                   a ClyTextEditingMode
	cursor                        a RubCursor(162526720)
	segments                      an OrderedCollection()
	getMenuPolicy                 nil
	mouseDownPoint                nil
	completionEngine              a CompletionEngine
	maxLength                     nil
	findReplaceService            a SpRubFindReplaceService
	wrapped                       true
	editorClass                   ClyTextEditor

----------------------------------------------------------------------------------------------------
[ ActiveHand := self.
ActiveEvent := anEvent.
result := focusHolder handleFocusEvent: (anEvent transformedBy: (focusHolder transformedFrom: self)) ] in HandMorph>>sendFocusEvent:to:clear:


receiver
	                              a HandMorph(677264640)


temps
	anEvent                       a RubEditingArea(620650752)
	focusHolder                   [keystroke '<Cmd-i>']
	aBlock                        #(nil)
	t1                            a RubEditingArea(620650752)
	t2                            [keystroke '<Cmd-i>']
	t3                            #(nil)


inst vars
	bounds                        (-5@721) corner: (11@737)
	owner                         a WorldMorph(691814656) [world]
	submorphs                     #()
	fullBounds                    (-5@721) corner: (11@737)
	color                         Color blue
	extension                     a MorphExtension (440854784)
	mouseFocus                    nil
	keyboardFocus                 a RubEditingArea(620650752)
	eventListeners                a WeakArray(a GLMFallbackScrollListBrick(739556864))
	mouseListeners                nil
	mouseClickState               nil
	mouseOverHandler              a MouseOverHandler
	lastMouseEvent                [(3@729) mouseOver CMD 277123888 nil]
	targetOffset                  (149@411)
	damageRecorder                a DamageRecorder
	cacheCanvas                   nil
	cachedCanvasHasHoles          true
	temporaryCursor               Form(16x16x8)
	temporaryCursorOffset         (-8@ -8)
	hardwareCursor                ((CursorWithMask
	extent: (16@16)
	depth: 1
	fromArray: #(
		2r0
		2r0
		2r0
		2r0
		2r0
		2r100000010000000000000000000
		2r1100000011000000000000000000
		2r11100000011100000000000000000
		2r11111111...etc...
	hasChanged                    true
	savedPatch                    nil
	lastEventBuffer               #(1 277123888 3 729 0 8 0 1)
	lastKeyScanCode               2
	combinedChar                  nil
	captureBlock                  nil
	recentModifiers               8

----------------------------------------------------------------------------------------------------
BlockClosure>>on:do:


receiver
	                              [ ActiveHand := self.
ActiveEvent := anEvent.
result := focusHolder handleFocusEvent: (anEvent transformedBy: (focusHolder transformedFrom: self)) ]


temps
	exception                     Error
	handlerAction                 [ :ex | 
ActiveWorld := priorWorld.
ActiveEvent := priorEvent.
ActiveHand := priorHand.
ex pass ]


inst vars
	outerContext                  HandMorph>>sendFocusEvent:to:clear:
	startpc                       111
	numArgs                       0

----------------------------------------------------------------------------------------------------
WorldMorph>>becomeActiveDuring:


receiver
	                              a WorldMorph(691814656) [world]


temps
	aBlock                        [ ActiveHand := self.
ActiveEvent := anEvent.
result := focusHolder handleFocusEvent: (anEvent transformedBy: (focusHolder transformedFrom: self)) ]
	priorWorld                    a WorldMorph(691814656) [world]
	priorHand                     a HandMorph(677264640)
	priorEvent                    nil


inst vars
	bounds                        (0@0) corner: (1920@1033)
	owner                         nil
	submorphs                     an Array(a MenubarMorph(1031397632) a TaskbarMorph(351923968) a SystemWindow(199374848) named: BugReporter a SpecWindow(486588928) named: Working copy of BugReport a SystemWindow(996783872) named: Bas...etc...
	fullBounds                    (0@0) corner: (1920@1033)
	color                         (Color r: 0.05865102639296188 g: 0.07331378299120235 b: 0.10557184750733138 alpha: 1.0)
	extension                     a MorphExtension (704038144) [other:  (dragEnabled -> true) (dropEnabled -> true) (kmDispatcher -> a KMDispatcher)]
	borderWidth                   0
	borderColor                   (Color r: 0.8611925708699902 g: 1.0 b: 0.7223851417399805 alpha: 1.0)
	backgroundMorph               an AlphaImageMorph(939675904)
	worldState                    a WorldState
	griddingOn                    nil

----------------------------------------------------------------------------------------------------
HandMorph>>sendFocusEvent:to:clear:


receiver
	                              a HandMorph(677264640)


temps
	result                        [keystroke '<Cmd-i>']
	anEvent                       a RubEditingArea(620650752)
	focusHolder                   [ self keyboardFocus: nil ]
	aBlock                        a WorldMorph(691814656) [world]
	w                             #(nil)


inst vars
	bounds                        (-5@721) corner: (11@737)
	owner                         a WorldMorph(691814656) [world]
	submorphs                     #()
	fullBounds                    (-5@721) corner: (11@737)
	color                         Color blue
	extension                     a MorphExtension (440854784)
	mouseFocus                    nil
	keyboardFocus                 a RubEditingArea(620650752)
	eventListeners                a WeakArray(a GLMFallbackScrollListBrick(739556864))
	mouseListeners                nil
	mouseClickState               nil
	mouseOverHandler              a MouseOverHandler
	lastMouseEvent                [(3@729) mouseOver CMD 277123888 nil]
	targetOffset                  (149@411)
	damageRecorder                a DamageRecorder
	cacheCanvas                   nil
	cachedCanvasHasHoles          true
	temporaryCursor               Form(16x16x8)
	temporaryCursorOffset         (-8@ -8)
	hardwareCursor                ((CursorWithMask
	extent: (16@16)
	depth: 1
	fromArray: #(
		2r0
		2r0
		2r0
		2r0
		2r0
		2r100000010000000000000000000
		2r1100000011000000000000000000
		2r11100000011100000000000000000
		2r11111111...etc...
	hasChanged                    true
	savedPatch                    nil
	lastEventBuffer               #(1 277123888 3 729 0 8 0 1)
	lastKeyScanCode               2
	combinedChar                  nil
	captureBlock                  nil
	recentModifiers               8

----------------------------------------------------------------------------------------------------
HandMorph>>sendEvent:focus:clear:


receiver
	                              a HandMorph(677264640)


temps
	anEvent                       [keystroke '<Cmd-i>']
	focusHolder                   a RubEditingArea(620650752)
	aBlock                        [ self keyboardFocus: nil ]
	result                        nil


inst vars
	bounds                        (-5@721) corner: (11@737)
	owner                         a WorldMorph(691814656) [world]
	submorphs                     #()
	fullBounds                    (-5@721) corner: (11@737)
	color                         Color blue
	extension                     a MorphExtension (440854784)
	mouseFocus                    nil
	keyboardFocus                 a RubEditingArea(620650752)
	eventListeners                a WeakArray(a GLMFallbackScrollListBrick(739556864))
	mouseListeners                nil
	mouseClickState               nil
	mouseOverHandler              a MouseOverHandler
	lastMouseEvent                [(3@729) mouseOver CMD 277123888 nil]
	targetOffset                  (149@411)
	damageRecorder                a DamageRecorder
	cacheCanvas                   nil
	cachedCanvasHasHoles          true
	temporaryCursor               Form(16x16x8)
	temporaryCursorOffset         (-8@ -8)
	hardwareCursor                ((CursorWithMask
	extent: (16@16)
	depth: 1
	fromArray: #(
		2r0
		2r0
		2r0
		2r0
		2r0
		2r100000010000000000000000000
		2r1100000011000000000000000000
		2r11100000011100000000000000000
		2r11111111...etc...
	hasChanged                    true
	savedPatch                    nil
	lastEventBuffer               #(1 277123888 3 729 0 8 0 1)
	lastKeyScanCode               2
	combinedChar                  nil
	captureBlock                  nil
	recentModifiers               8

----------------------------------------------------------------------------------------------------
HandMorph>>sendKeyboardEvent:


receiver
	                              a HandMorph(677264640)


temps
	anEvent                       [keystroke '<Cmd-i>']


inst vars
	bounds                        (-5@721) corner: (11@737)
	owner                         a WorldMorph(691814656) [world]
	submorphs                     #()
	fullBounds                    (-5@721) corner: (11@737)
	color                         Color blue
	extension                     a MorphExtension (440854784)
	mouseFocus                    nil
	keyboardFocus                 a RubEditingArea(620650752)
	eventListeners                a WeakArray(a GLMFallbackScrollListBrick(739556864))
	mouseListeners                nil
	mouseClickState               nil
	mouseOverHandler              a MouseOverHandler
	lastMouseEvent                [(3@729) mouseOver CMD 277123888 nil]
	targetOffset                  (149@411)
	damageRecorder                a DamageRecorder
	cacheCanvas                   nil
	cachedCanvasHasHoles          true
	temporaryCursor               Form(16x16x8)
	temporaryCursorOffset         (-8@ -8)
	hardwareCursor                ((CursorWithMask
	extent: (16@16)
	depth: 1
	fromArray: #(
		2r0
		2r0
		2r0
		2r0
		2r0
		2r100000010000000000000000000
		2r1100000011000000000000000000
		2r11100000011100000000000000000
		2r11111111...etc...
	hasChanged                    true
	savedPatch                    nil
	lastEventBuffer               #(1 277123888 3 729 0 8 0 1)
	lastKeyScanCode               2
	combinedChar                  nil
	captureBlock                  nil
	recentModifiers               8

----------------------------------------------------------------------------------------------------
HandMorph>>handleEvent:


receiver
	                              a HandMorph(677264640)


temps
	anEvent                       [keystroke '<Cmd-i>']
	evt                           [keystroke '<Cmd-i>']


inst vars
	bounds                        (-5@721) corner: (11@737)
	owner                         a WorldMorph(691814656) [world]
	submorphs                     #()
	fullBounds                    (-5@721) corner: (11@737)
	color                         Color blue
	extension                     a MorphExtension (440854784)
	mouseFocus                    nil
	keyboardFocus                 a RubEditingArea(620650752)
	eventListeners                a WeakArray(a GLMFallbackScrollListBrick(739556864))
	mouseListeners                nil
	mouseClickState               nil
	mouseOverHandler              a MouseOverHandler
	lastMouseEvent                [(3@729) mouseOver CMD 277123888 nil]
	targetOffset                  (149@411)
	damageRecorder                a DamageRecorder
	cacheCanvas                   nil
	cachedCanvasHasHoles          true
	temporaryCursor               Form(16x16x8)
	temporaryCursorOffset         (-8@ -8)
	hardwareCursor                ((CursorWithMask
	extent: (16@16)
	depth: 1
	fromArray: #(
		2r0
		2r0
		2r0
		2r0
		2r0
		2r100000010000000000000000000
		2r1100000011000000000000000000
		2r11100000011100000000000000000
		2r11111111...etc...
	hasChanged                    true
	savedPatch                    nil
	lastEventBuffer               #(1 277123888 3 729 0 8 0 1)
	lastKeyScanCode               2
	combinedChar                  nil
	captureBlock                  nil
	recentModifiers               8

----------------------------------------------------------------------------------------------------
HandMorph>>processEventsFromQueue:


receiver
	                              a HandMorph(677264640)


temps
	anEventQueue                  an InputEventSensor
	evt                           [keystroke '<Cmd-i>']
	evtBuf                        #(2 277123888 105 0 8 105 0 1)
	type                          2
	hadAny                        true


inst vars
	bounds                        (-5@721) corner: (11@737)
	owner                         a WorldMorph(691814656) [world]
	submorphs                     #()
	fullBounds                    (-5@721) corner: (11@737)
	color                         Color blue
	extension                     a MorphExtension (440854784)
	mouseFocus                    nil
	keyboardFocus                 a RubEditingArea(620650752)
	eventListeners                a WeakArray(a GLMFallbackScrollListBrick(739556864))
	mouseListeners                nil
	mouseClickState               nil
	mouseOverHandler              a MouseOverHandler
	lastMouseEvent                [(3@729) mouseOver CMD 277123888 nil]
	targetOffset                  (149@411)
	damageRecorder                a DamageRecorder
	cacheCanvas                   nil
	cachedCanvasHasHoles          true
	temporaryCursor               Form(16x16x8)
	temporaryCursorOffset         (-8@ -8)
	hardwareCursor                ((CursorWithMask
	extent: (16@16)
	depth: 1
	fromArray: #(
		2r0
		2r0
		2r0
		2r0
		2r0
		2r100000010000000000000000000
		2r1100000011000000000000000000
		2r11100000011100000000000000000
		2r11111111...etc...
	hasChanged                    true
	savedPatch                    nil
	lastEventBuffer               #(1 277123888 3 729 0 8 0 1)
	lastKeyScanCode               2
	combinedChar                  nil
	captureBlock                  nil
	recentModifiers               8

----------------------------------------------------------------------------------------------------
HandMorph>>processEvents


receiver
	                              a HandMorph(677264640)


temps


inst vars
	bounds                        (-5@721) corner: (11@737)
	owner                         a WorldMorph(691814656) [world]
	submorphs                     #()
	fullBounds                    (-5@721) corner: (11@737)
	color                         Color blue
	extension                     a MorphExtension (440854784)
	mouseFocus                    nil
	keyboardFocus                 a RubEditingArea(620650752)
	eventListeners                a WeakArray(a GLMFallbackScrollListBrick(739556864))
	mouseListeners                nil
	mouseClickState               nil
	mouseOverHandler              a MouseOverHandler
	lastMouseEvent                [(3@729) mouseOver CMD 277123888 nil]
	targetOffset                  (149@411)
	damageRecorder                a DamageRecorder
	cacheCanvas                   nil
	cachedCanvasHasHoles          true
	temporaryCursor               Form(16x16x8)
	temporaryCursorOffset         (-8@ -8)
	hardwareCursor                ((CursorWithMask
	extent: (16@16)
	depth: 1
	fromArray: #(
		2r0
		2r0
		2r0
		2r0
		2r0
		2r100000010000000000000000000
		2r1100000011000000000000000000
		2r11100000011100000000000000000
		2r11111111...etc...
	hasChanged                    true
	savedPatch                    nil
	lastEventBuffer               #(1 277123888 3 729 0 8 0 1)
	lastKeyScanCode               2
	combinedChar                  nil
	captureBlock                  nil
	recentModifiers               8

----------------------------------------------------------------------------------------------------
[ :h | 
self activeHand: h.
h processEvents.
self activeHand: nil ] in WorldState>>doOneCycleNowFor:


receiver
	                              a WorldState


temps
	aWorld                        a HandMorph(677264640)
	t1                            a HandMorph(677264640)


inst vars
	hands                         an Array(a HandMorph(677264640))
	damageRecorder                a DamageRecorder
	stepList                      a Heap()
	lastStepTime                  277121260
	lastStepMessage               nil
	lastCycleTime                 277123912
	alarms                        a Heap(MorphicAlarm(#hideShow -> a RubCursor(162526720)))
	lastAlarmTime                 277123891
	menuBuilder                   a PragmaMenuBuilder ( nil ) 
	activeHand                    a HandMorph(677264640)
	currentCursor                 ((CursorWithMask
	extent: (16@16)
	depth: 1
	fromArray: #(
		2r0
		2r0
		2r0
		2r0
		2r0
		2r100000010000000000000000000
		2r1100000011000000000000000000
		2r11100000011100000000000000000
		2r11111111...etc...
	worldRenderer                 a VMWorldRenderer
	realWindowExtent              (1920@1033)

----------------------------------------------------------------------------------------------------
Array(SequenceableCollection)>>do:


receiver
	                              an Array(a HandMorph(677264640))


temps
	aBlock                        [ :h | 
self activeHand: h.
h processEvents.
self activeHand: nil ]
	index                         1

----------------------------------------------------------------------------------------------------
WorldState>>handsDo:


receiver
	                              a WorldState


temps
	aBlock                        [ :h | 
self activeHand: h.
h processEvents.
self activeHand: nil ]


inst vars
	hands                         an Array(a HandMorph(677264640))
	damageRecorder                a DamageRecorder
	stepList                      a Heap()
	lastStepTime                  277121260
	lastStepMessage               nil
	lastCycleTime                 277123912
	alarms                        a Heap(MorphicAlarm(#hideShow -> a RubCursor(162526720)))
	lastAlarmTime                 277123891
	menuBuilder                   a PragmaMenuBuilder ( nil ) 
	activeHand                    a HandMorph(677264640)
	currentCursor                 ((CursorWithMask
	extent: (16@16)
	depth: 1
	fromArray: #(
		2r0
		2r0
		2r0
		2r0
		2r0
		2r100000010000000000000000000
		2r1100000011000000000000000000
		2r11100000011100000000000000000
		2r11111111...etc...
	worldRenderer                 a VMWorldRenderer
	realWindowExtent              (1920@1033)

----------------------------------------------------------------------------------------------------
WorldState>>doOneCycleNowFor:


receiver
	                              a WorldState


temps
	aWorld                        a WorldMorph(691814656) [world]


inst vars
	hands                         an Array(a HandMorph(677264640))
	damageRecorder                a DamageRecorder
	stepList                      a Heap()
	lastStepTime                  277121260
	lastStepMessage               nil
	lastCycleTime                 277123912
	alarms                        a Heap(MorphicAlarm(#hideShow -> a RubCursor(162526720)))
	lastAlarmTime                 277123891
	menuBuilder                   a PragmaMenuBuilder ( nil ) 
	activeHand                    a HandMorph(677264640)
	currentCursor                 ((CursorWithMask
	extent: (16@16)
	depth: 1
	fromArray: #(
		2r0
		2r0
		2r0
		2r0
		2r0
		2r100000010000000000000000000
		2r1100000011000000000000000000
		2r11100000011100000000000000000
		2r11111111...etc...
	worldRenderer                 a VMWorldRenderer
	realWindowExtent              (1920@1033)

----------------------------------------------------------------------------------------------------
WorldState>>doOneCycleFor:


receiver
	                              a WorldState


temps
	aWorld                        a WorldMorph(691814656) [world]


inst vars
	hands                         an Array(a HandMorph(677264640))
	damageRecorder                a DamageRecorder
	stepList                      a Heap()
	lastStepTime                  277121260
	lastStepMessage               nil
	lastCycleTime                 277123912
	alarms                        a Heap(MorphicAlarm(#hideShow -> a RubCursor(162526720)))
	lastAlarmTime                 277123891
	menuBuilder                   a PragmaMenuBuilder ( nil ) 
	activeHand                    a HandMorph(677264640)
	currentCursor                 ((CursorWithMask
	extent: (16@16)
	depth: 1
	fromArray: #(
		2r0
		2r0
		2r0
		2r0
		2r0
		2r100000010000000000000000000
		2r1100000011000000000000000000
		2r11100000011100000000000000000
		2r11111111...etc...
	worldRenderer                 a VMWorldRenderer
	realWindowExtent              (1920@1033)

----------------------------------------------------------------------------------------------------
WorldMorph>>doOneCycle


receiver
	                              a WorldMorph(691814656) [world]


temps


inst vars
	bounds                        (0@0) corner: (1920@1033)
	owner                         nil
	submorphs                     an Array(a MenubarMorph(1031397632) a TaskbarMorph(351923968) a SystemWindow(199374848) named: BugReporter a SpecWindow(486588928) named: Working copy of BugReport a SystemWindow(996783872) named: Bas...etc...
	fullBounds                    (0@0) corner: (1920@1033)
	color                         (Color r: 0.05865102639296188 g: 0.07331378299120235 b: 0.10557184750733138 alpha: 1.0)
	extension                     a MorphExtension (704038144) [other:  (dragEnabled -> true) (dropEnabled -> true) (kmDispatcher -> a KMDispatcher)]
	borderWidth                   0
	borderColor                   (Color r: 0.8611925708699902 g: 1.0 b: 0.7223851417399805 alpha: 1.0)
	backgroundMorph               an AlphaImageMorph(939675904)
	worldState                    a WorldState
	griddingOn                    nil

----------------------------------------------------------------------------------------------------
WorldMorph class>>doOneCycle


receiver
	                              WorldMorph


temps


inst vars
	superclass                    PasteUpMorph
	methodDict                    a MethodDictionary(#acceptDroppingMorph:event:->WorldMorph>>#acceptDroppingMorph:event: #activateCursor:->WorldMorph>>#activateCursor: #activateCursor:withMask:->WorldMorph>>#activateCursor:withMask: ...etc...
	format                        65547
	layout                        a FixedLayout
	organization                  a ClassOrganization
	subclasses                    {AthensWorldMorph. OSWindowWorldMorph}
	name                          #WorldMorph
	classPool                     a Dictionary(#AllowDropFiles->nil #CursorOwnerWorld->a WorldMorph(691814656) [world] #ExtraWorldList->#() )
	sharedPools                   an OrderedCollection()
	environment                   a SystemDictionary(lots of globals)
	category                      #'Morphic-Core-Worlds'
	announcer                     nil
	displayScaleFactor            1

----------------------------------------------------------------------------------------------------
[ [ WorldMorph doOneCycle.
Processor yield.
false ] whileFalse: [  ] ] in MorphicUIManager>>spawnNewProcess


receiver
	                              a MorphicUIManager


temps


inst vars
	activeTranscript              nil

----------------------------------------------------------------------------------------------------
[ self value.
Processor terminateActive ] in BlockClosure>>newProcess


receiver
	                              [ [ WorldMorph doOneCycle.
Processor yield.
false ] whileFalse: [  ] ]


temps


inst vars
	outerContext                  MorphicUIManager>>spawnNewProcess
	startpc                       125
	numArgs                       0
```
</details>

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
| 1.x.x       	| Pharo 61, 70, 80, 90			|

## Contact

If you have any questions or problems do not hesitate to open an issue or contact cyril (a) ferlicot.me 
