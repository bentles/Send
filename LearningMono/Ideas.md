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
	- option 1: build a tree of observables that I maintain as items are added and removed
				
	- option 2: have this tree be implicit and have the game loop process the connections by checking emits and subscription lists
		- i guess im trying this for now
		- option 2.1: have a list of subscribers
			- how will update ordering work??
				- first generators
				- then? build a list of observers to update
				- keep doing that but only allow things to be updated once per tick
					- otherwise we could have an infinite loop
			- how does unsubscribe work?
				- you remove the block and then??
				- i guess I can keep an subscribedTo field too and use that for unsub
					- and for things like merge this will have to be a list
		- option 2.2: have a subbedTo field

		- option 2.3: observable style function setup
			- howtf does this work???
				- i dont think its possible because i dont directly have the tree structure to update

- generator is just a function of time

- investigate world datastructure options?
	- user vctor from fsharpx for world??:
	- use zipper data structure for world

Sinks:
	- we need some way to test that the player has achieved the correct signal
		- single signal match
			- correct signal type
			- with timeout for repeating
		- match timing, any signal
		- match signal type and timing

	- how do I implement this?
		- kind of like an observable with a reference subject
			- implement a multi observer with compare then any subject i can think of???
			- implement an observer with a verify function

	- need to iron out what emitting means
		- can something emit forever?
			- no but maybe it's just a subject that emits every single frame

Door