
TODO:
- entities vs blocks?

DOING:
	- make the world
	- design by contract
		- put asserts wherever i have made assumptions

DONE:
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



