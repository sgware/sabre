# Sabre Narrative Planner

A narrative planner is like a puppet master; it tries to tell a story the author
wants out of actions that make sense for the story's characters. You describe
the things that exist in the world, their initial configuration, the kinds of
events that can happen, a score for the author, and a score for each character.
Sabre finds a plan (a sequence of actions) that improves the author's score, but
every time a character takes an action that character must believe the action
can help them improve their own score. Characters can have wrong beliefs, and
they have an arbitrarily nested theory of mind (what I believe you believe I
believe, and so on).

To be a bit more technical, Sabre is a deterministic state-space planning
algorithm that uses forward heuristic search to solve a multi-agent coordination
problem. A solution plan is a sequence of actions that improves the author's
utility and where every action is _explained_ for every agent who takes that
action. An action is _explained for an agent_ when, in the state before the
action is taken, there exists a plan which (1) can be executed in the state the
agent believes the world to be in, (2) would improve the agent's utility if
their beliefs are correct, and (3) is made only of actions that are explained
for all other agents who act.

A narrative planner is not meant to replace human authors but to assist them in
telling interactive stories. People are better than algorithms at telling
stories, and even if they weren't, storytelling is an important feature
of human culture that should not be automated away. The value of a narrative
planner is its ability to assist in procedural storytelling by re-generating
a story quickly at run time as part of some larger interactive narrative system.

Sample problems and code to compare different configurations of Sabre can be
found in a separate repository:
https://github.com/sgware/sabre-benchmarks

## Sabre Problems

An example problem:
```
/* Types */

type character : location; // Character is a built-in type but can be given more parent types.
type place : location;
type item; // By default, all new types extend entity.

/* Entities */

entity Tom : character;
entity Merchant : character;
entity Home : place;
entity Market : place;
entity MacGuffin : item;

/* Properties */

property alive(character : character): boolean; // Boolean is a built-in type.
property at(character : character) : place;
property money(character : character) : number; // Number is a built-in type.
property at(item : item) : location;

/* Initial State */

forall(c : character) // Example of a `forall` quantifier; the other is `exists`.
	alive(c);
at(Tom) = Home;
money(Tom) = 1;
at(Merchant) = Market;
at(MacGuffin) = Merchant;
believes(Merchant, at(Tom) = ?); // Merchant wrongly believes Tom is nowhere.

/* Actions */

// A character walks from one place to another.
action walk(character : character, from : place, to : place) {
	precondition:
		from != to &
		alive(character) &
		at(character) == from;
	effect:
		at(character) = to;
	consenting: character;
	observing(c : character): at(c) == from | at(c) == to;
};

// One character buys an item from the Merchant for 1 money.
action buy(buyer : character, item : item, Merchant) {
	precondition:
		buyer != Merchant &
		alive(buyer) &
		alive(Merchant) &
		at(buyer) == at(Merchant) & // Characters are in the same location.
		money(buyer) > 0 &
		at(item) == Merchant;
	effect:
		at(item) = buyer &
		money(buyer) = money(buyer) - 1 &
		money(Merchant) = money(Merchant) + 1;
	consenting: buyer, Merchant; // Buyer and merchant have to want to take this action.
	observing(c : character): at(c) == at(buyer); // Anyone at the buyer's location sees this happen.
};

/* Triggers */

// One character sees another character in the same place.
trigger see_character_at(viewer : character, other : character, place : place) {
	precondition:
		alive(viewer) &
		at(viewer) == place &
		at(other) == place &                  
		believes(viewer, at(other) != place); // Precondition should be made false
	effect:                                   // by effect or trigger runs forever.
		believes(viewer, at(other) = place);  // Beliefs can be updated explicitly.
};

// One character realizes a character they thought was at a place isn't there.
trigger see_character_gone(viewer : character, other : character, place : place) {
	precondition:
		alive(viewer) &
		at(viewer) == place &
		at(other) != place &
		believes(viewer, at(other) == place);
	effect:
		believes(viewer, at(other) = ?); // ? is unknown, equivalent to null.
};

/* Utilities */

// Author utility is the one the planner must improve.
// The author wants Tom to have the MacGuffin.
utility():
	at(MacGuffin) == Tom; // Boolean utilities are translated into "if x 1 else 0".

// Characters take actions they think can improve their utilities.
// Tom wants the MacGuffin.
utility(Tom):
	if(!alive(Tom))
		0
	elseif(at(MacGuffin) == Tom)
		2
	else
		1;

// The merchant wants to have as much money as possible as long as they are alive.
utility(Merchant):
	if(!alive(Merchant))
		0
	else
		money(Merchant);
```

A problem defines seven things:
- *Types*: Kinds of things that can exist. Four types are pre-defined. `boolean`
and `number` cannot have parent or child types. Types you define extend `entity`
by default, and `entity` cannot have parent types. The type `character` denotes
special objects who can have beliefs and intentions; it extends `entity`.
- *Entities*: Objects that exist. Some objects, like `True`, `False`, and
numbers exist in every problem. Objects you define must be of type `entity`
either directly or by inheritance. Objects can have multiple types.
- *Properties*: Relationships between entities. Properties have a value of some
type. A world state is defined by the values assigned to each property. The
default initial value for `boolean` properties is `False`, for `number`
properties is `0`, and for `entities` is `?` (the equivalent of null). Initial
values can be set in the initial state, and their values can be changed by
actions and triggers.
- *Initial State*: Free-floating logical expressions define the initial values
of properties. Later expressions override earlier ones.
- *Actions*: Actions are events which the author or characters can choose to
take. An action has four parts:
	- *Precondition*: A logical expression which must be true before the action
	can occur.
	- *Effect*: A deterministic list of assignments of values to properties that
	describe how the action changes the state. Effects can be conditional.
	- *Consenting Characters*: A list of zero, one, or many `character`s who
	must have a reason to take the action. No consenting characters means the
	author can take the action at any time. Characters cannot anticipate actons
	with no consenting characters. When there are two or more characters, each
	must have a reason, but they can have different reasons.
	- *Observing Characters*: A function which defines a `boolean` expression
	for each `character`. If it is true in the state before the action occurs,
	the character will see the action occur and know about its effects.
	Characters who do not observe the action will not update their beliefs
	(unless the action's effect has explcit belief updates for that character).
- *Triggers*: Events that must occur when their preconditions are met. A trigger
defines a precondition and effect like an action. Triggers do not define
consenting characters, because they must happen when they can. They do not
define observing characters because every character believes that when their
preconditions are met their effects occur. Note that if a character does not
believe that a trigger's precondition is met, they will not observe the trigger.
Make sure the effect of a trigger makes the precondition false, or else the same
trigger will apply over and over, causing an infinite loop.
- *Utility Expressions*: Numeric scoring functions for the author and each
`character` that rank how good each state is for them. If a utility is defined
as a `boolean` expression `x`, it will be treated as `if(x) 1 else 0`.

## Basic Usage

To see a list of command line parameters:

```
java -jar sabre.jar -help
```

Assuming you have the example problem above in a file `macguffin.txt` in the
same directory as the Sabre JAR file:

```
java -jar sabre.jar -p macguffin.txt
```

If Sabre succeeds, the output will be a plan that improves the author's utility
using only actions that makes sense for the characters who take them. For this
problem, the solution is for Tom to walk to the market and buy the MacGuffin
from the merchant.

```
walk(Tom, Home, Market) buy(Tom, MacGuffin, Merchant)
```

Note that the merchant cannot walk to Tom's house to sell the MacGuffin, because
the merchant does not know where Tom is.

Here is a more advanced search. `-v` means verbose mode, which shows detailed
information about the search and its result. This example uses Bonet and
Geffner's additive heuristic. It also sets the _epistemic limit_ to 2, which
means the planner will only reason 2 layers deep in the theory of mind. In other
words, it will reason about what Tom believes (1 layer), what Tom believes the
Merchant believes (2 layers), but not what Tom believes the Merchant believes
Tom believes (3 layers).

```
java -jar sabre.jar -v -p macguffin.txt -h h+ -el 2
```

The output shows 6 nodes visited, which means the planner considered 6 partially
complete versions of the story before it found the solution.

Here is a complete search of the problem to depth 3. This example uses
_explanation-first search_, which means an action must be explained for all
other characters before the planner can consider taking another action. For
example, if the author imagines taking `walk(Tom, Home, Market)` as the first
action in a plan, the planner must find a reason why Tom would want to go to the
market before the author can consider a second action. The `-g 2` option sets
the goal to 2, which cannot be achieved, so the search will run until it has
visited all nodes. `-h 0` uses no heuristic to speed up the search. `-atl 3`
sets the _author temporal limit_ to 3, which means that solutions may contain up
to 3 actions. `-ctl 3` sets the _character temporal limit_ to 3, which means
characters can form plans of up to 3 actions when trying to figure out if an
action makes sense for them to take.

```
java -jar sabre.jar -v -p macguffin.txt -g 2 -m ef -h 0 -atl 3 -ctl 3
```

This search visits 46 nodes, which means there are 46 possible states that the
planner has to consider before concluding that no solution exists. The planner
did find a way to improve author utility to 1, so that suboptimal solution is
given.

A verbose solution looks like this:

```
walk(Tom, Home, Market)
| buy(Tom, MacGuffin, Merchant)
| | goal(Merchant, money(Merchant) == 1)
| goal(Tom, at(MacGuffin) == Tom)
buy(Tom, MacGuffin, Merchant)
| goal(Tom, at(MacGuffin) == Tom)
| goal(Merchant, money(Merchant) == 1)
goal(at(MacGuffin) == Tom)
```

It means the author achieved its goal of Tom having the MacGuffin by Tom walking
to the market and buying it from the merchant. The actions indented with `|` are
the explanation for Tom's action. Tom is willing to walk to the market because
he can then buy the MacGuffin. The merchant will consent to Tom buying the
MacGuffin because she wants the money.

## Ownership and License

The Sabre Narrative Planner was developed by Stephen G. Ware PhD, Associate
Professor of Computer Science at the University of Kentucky. Development of
Sabre was sponsored in part by two grants from the US National Science
Foundation, #2145153 and #1911053.

Sabre is released under the Creative Commons Attribution-NonCommercial 4.0
International license. This means you are free to share, remix, and add to this
software for non-commercial projects as long as you give credit to the original
creators. See the license file for details. The University of Kentucky retains
all right not specifically granted.

To license Sabre for a commercial project, contact the University of Kentucky
Office of Technology Commercialization at <otcinfo@uky.edu>.

Special thanks to Rachelyn Farrell PhD and Cory Siler for their help with
testing and development.

## Version History

- Version 0.7: First public release.

## Citation

Please cite this paper when referring to Sabre:

> Stephen G. Ware and Cory Siler, "Sabre: A Narrative Planner Supporting
> Intention and Deep Theory of Mind," in Proceedings of the 17th AAAI Conference
> on Artificial Intelligence and Interactive Digital Entertainment, pp. 99-106,
> 2021.

BiBTeX entry:

```
@inproceedings{ware2021sabre,
  title={Sabre: A Narrative Planner Supporting Intention and Deep Theory of Mind},
  author={Ware, Stephen G. and Siler, Cory},
  booktitle={Proceedings of the 17th AAAI International Conference on Artificial Intelligence and Interactive Digital Entertainment},
  year={2021},
  pages={99--106}
}
```