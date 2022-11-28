
TODO:
- wrap vector2 with nicer operator overloads??

DOING:
- make transformation animation work correctly
	- need the concept of looping and non-looping animations
		- need to move looping into image config because now it can become decoupled and get into weird states
			- clean up configs with a type: Big.Sprite, Big.WalkAni, Big.WalkSpeed etc.

DONE:
	- need an animation complete command...or message?... i think command
	- how do children send messages up to parents??
- offsets so small and big are the the same Y level
