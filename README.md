# SugarscapeR

A simple implementation of sugarscape in R. Sugarscape is a model developed by Epstein and Axtell (1999). See [Wikipedia](https://www.wikiwand.com/en/Sugarscape) for more details

![Agents roaming the sugarscape](https://raw.githubusercontent.com/demirev/sugarscapeR/master/demo.gif)

The model consists of a gridworld, some cells of which are spawning 'sugar' in different quantities. 

Agents inhabiting the sugarscape need sugar in order to survive. They move to the cllosest field with sugar that they can see, and harvest its resources. They also burn off sugar at a constant rate.

In the gif above, dark areas represent sugar fields. Agents either quickly find their way to the fields, or wander into the 'wilderness' and slowly die out.

There are multiple possible extensions to the simple baseline model, some of which I hope to implement at some point. Most importantly, agents in sugarscape are not inteligent (they follow pre-defined rules), and adding some (small) degree of autonomy in decision making should be straight-forward.