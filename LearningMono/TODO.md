
TODO:
- nicer toggling of debug sprites
- quick check for collision exclusion of far away objects
- simplify away the pre-sending potential collider objects since world+player are now one
- use the stopped animation
- simplify all the things
- prevent transform if carrying limits exceeded
- make it easier to create entities for carrying

DOING:
- levels and switching between them
	

DONE:
- create a sink of some kind
- entity and reactive are linked but not in the model
- fix pick up and put down logic
- finally add the carrying limits for big/small
- figure out if game idea is fun first
	- add a filter observer
	- add a repeating pattern observable
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



