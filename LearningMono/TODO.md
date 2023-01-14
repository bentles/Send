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

TODO:
- nicer toggling of debug sprites
- quick check for collision exclusion of far away objects
- simplify away the pre-sending potential collider objects since world+player are now one
- use the stopped animation
- finally add the carrying limits for big/small
- fix pick up and put down logic
- simplify all the things
	- entity and reactive are linked but not in the model



DOING:
- figure out if game idea is fun first
	- add a filter observer
	- add a repeating pattern observable

DONE:
- sprites for more directions pls
- concept of facing for the player
- make the world
	- entities vs blocks?
		- pick up entities mechanic

		- remove chunks until chunks are needed
- make transformation animation work correctly
	- need the concept of priority or some way to say this animation has precendence
- wrap vector2 with nicer operator overloads??
	- this was already a thing
- need the concept of looping and non-looping animations
	- need to move looping into image config because now it can become decoupled and get into weird states
		- clean up configs with a type: Big.Sprite, Big.WalkAni, Big.WalkSpeed etc.
- need an animation complete command...or message?... i think command
- how do children send messages up to parents??
- offsets so small and big are the the same Y level
idea 2: camera pos relative to player pos
- player belongs to world
- world also has a frame of reference
- player movement sends messages up??
	- nah the world gets message... moves player and camera based on message
- world and player joined together into 1???



