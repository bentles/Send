IDEAS:
- design by contract
	- put asserts wherever i have made assumptions
-``` // TODO: something like this for later
type Level =
    | Level of WorldConfig

type Screen =
    | Menu
    | Playing of Level
    | GameOver ```
- add the concept of a level and some way of setting them up

- THEN make levels and stuff

- observables and subjects
	- just have an observing field and an action
	- subject is just a function of time

- investigate world datastructure options?
	- user vctor from fsharpx for world??:
	- use zipper data structure for world

Observable ideas:
	- lets be real just copy rxjs marbles
	- it does seem like i need multi observers
	- set the type of observer using the other signal??

Sinks:
	- we need some way to test that the player has achieved the correct signal
		- single signal match
			- correct signal type
		- match timing, any signal
		- match signal type and timing

	- how do I implement this?
		- kind of like an observable with a reference subject
			- implement a multi observer with compare then any subject i can think of???
			- OR implement an observer with a verify function

	- need to iron out what emitting means
		- can something emit forever?
			- no but maybe it's just a subject that emits every single frame

Door:
	- toggle
	- single use


Can I separate state out so that some changes every frame and some does not???

Toggle:
	- should I use this as a kind of NOT gate??
		- either with T and F or with (Toggle true) and (Toggle false)
		- this begs a broader question about how "normal" things in the world emit events and what those events look like?
			- like open and close events
			- or this thing grew 1 step event etc.
		- are events ethereal?
			- unit kind of already is
			- could possibly have T and F events
			- it feels like possibly the role of big centure guy is to bridge the gap between signals and the real world somehow
			- BUT unit is the default signal so we can get away with this in the
		- maybe Map is underpowered?
			- but you still can't get a toggle from the same input


