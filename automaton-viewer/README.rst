Automaton Viewer
----------------

This directory contains a visualizer for automata produced by
DaeDaLus, as an interactive alternative to GraphViz-produced PDFs. To
use it, run::

    $ racket graph.rkt FILE.json

Use
===

Click on a state to select it, or drag a "rubber band" to select
multiple states. Details about the selected states and their outgoing
gransitions will be displayed in the lower pane. Use the ``+`` and
``-`` buttons to zoom the display.

Limitations
===========
 - Start states are not graphically indicated at present
 - There is no way to save a particular layout of nodes for later
 - DaeDaLus does not yet emit the necessary JSON format

File Format
===========

The automaton should be encoded in JSON as follows:
An automaton is an object with 3 keys:
 - ``name``: The name of the automaton (e.g. based on its DDL)
 - ``states``: A JSON array of states (see later)
 - ``transitions``: A JSON array of transitions

A JSON state is an object with the following keys:
 - ``name``: The name of the state (must be unique)
 - ``start``: A Boolean - whether the state is a start state
 - ``accepting``: A Boolean - whether the state is an accepting state
 - ``details`` (optional): Extra information to show about the state when clicked on

A JSON transition is an object with the following keys:
 - ``source``: the name of the source state
 - ``target``: the name of the target state
 - ``summary``: the label to show in the viewer
 - ``details`` (optional): Extra info to show when the transition's source state is clicked on

Example::

    {"name": "My automaton",
     "states": [{"name": "S0", "start": true, "accepting": true, "details": "blah blah blah"}],
     "transitions": [{"source": "S0", "target": S0, "summary": "ϵ", "details": "Go!"}]}

See file ``test-graph.json`` for a larger randomly-generated example.


