Talos
=====

Talos is a tool that can generate valid inputs for formats defined in the Daedalus_ data description language.

.. _Daedalus: https://github.com/GaloisInc/daedalus

Talos is an early prototype tool and many Daedalus features are currently unsupported. 

Building and Running Talos
--------------------------

In order to build Talos, clone the current repository, and then run the following commands: 

.. code-block:: bash 

  git submodule update --init
  cabal build exe:talos 

This instructs ``cabal`` to build the required submodules, including Daedalus, and then to build the Talos executable itself. 

To run ``talos`` you need a Daedalus specification describing the parser. Talos will then generate example inputs that would be successfully parsed. For example, you could try the following: 

.. code-block:: bash 

  cabal run talos -- -n 4 tests/T003.ddl

The ``-n`` flag controls how many example inputs are generated. In this example, we generate four possible inputs that satisfy the parser. 
