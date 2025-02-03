Sixty Three
===========

From [Wikipedia](https://en.wikipedia.org/wiki/Sixty-three_(card_game))

> Sixty-three is a card game...named after the number of points which can be taken in a hand.

This is a web application implementation of the game that can be played online.

Development
===========

The backend is written in Haskell, using Stack for dependency management. You'll want to install [ghcup](haskell.org/ghcup), and then `stack build` in the `backend` directory.

The frontend is written in [Elm](elm-lang.org). You will need [nodejs](nodejs.org) installed and should `npm ci && npm run elm-app start` in the `frontend` directory.

There are also some acceptance tests written using JavaScript in the `browser_automation` directory. `npm ci && node index.js` in there to run those.
