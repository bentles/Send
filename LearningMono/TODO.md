THINKING:
- maybe the toggle bois give off their own state as their signal?

TODO:
- quick check for collision exclusion of far away objects
- simplify away the pre-sending potential collider objects since world+player are now one
- use the stopped animation on characters
- prevent transform if carrying limits exceeded
- button goes down on key down
- simplify all the things
	- make it easier to create entities for carrying
	- simplify interaction event stuffs - seems dumb
	- make it easier to switch animations
	- some of my partial active patterns could probs just be complete patterns
- open close chest sounds
- use tile to show which entities are permanent
- move entities to their own list
- figure out if i can make placement direction into a level
	- should it affect AABB??? (probably)
- feedback from session with ash
	- shift and arrows to change directions
	- lag spikes
	- boxes too many levels deep
	- pick up multiple things more obvious
	- more smaller obvious levels
	- more box levels
	- pick up some things still confusing
	- fix placements
		- add the player AABB to the can place check


DOING:

DONE:
- define ordering groups 0 = tile 0.1 = target 0.2 = entities, 0.3 = emitting etc.
- fix player behind entity redering order
- add debug collision rendering back
- fix player offset madness
- map and filter need to convey to the user wtf is going on
	- might need a better version of this
- map and filter need activation objects
- unit signal
- just use facing to determine observable target
- figure out a nicer way to create levels

```fsharp
let level = [
	[xx;xx;xx;xx;xx];
	[xx;as;as;as;xx];
	[xx;as;as;as;xx];
	[xx;as;as;as;xx];
	[xx;xx;xx;xx;xx];
]
```
- make buttons work using same mechanic as box?
	- might be simpler to not do this and use button + map
- win condition for box level?
	- just put the button in nested boxes :P
- make the box work :)
	- place items into with place item button
	- remove item with remove button
	- action button toggle open/closedness of box
	- some way to see what's inside??
- interaction menus :'(
	- might be possible to avoid making this by using action button and other items
- move away from viewables as much as possible -- too much GC
- option --> voption
- button press should be on keypress not key down
- fix weird jumping when switching placement orientation
- add simple music and sfx
- Toggle observable that switches between collidable and not when triggered
- create level 3
- created toggle but not tested yet
- levels and switching between them
	- levels need borders
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



