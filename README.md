gore-and-ash-demo
=================

The repo contains proof-of-concept implementation of simple game with [Gore&Ash](https://github.com/Teaspot-Studio/gore-and-ash) engine.

Installation
============

1. Install `stack` from [the site](https://haskell-lang.org/get-started);
2. Install C dependencies:
    * `SDL2`
    * `SDL2_ttf`
2. Run `stack install` from root directory of the repo;
3. Start server `gore-and-ash-demo-server --port 5556`;
4. Start several clients with `gore-and-ash-demo-client --host localhost --port 5556`.


Screenshots
===========

![Screenshot 1](screens/screenshot_001.png)

![Screenshot 2](screens/screenshot_002.png)
