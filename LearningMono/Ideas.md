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


