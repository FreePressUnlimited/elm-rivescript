# elm-rivescript

**An Elm RiveScript library** This library provides an Elm interface for RiveScript built on top of the [rivescript-js](https://github.com/aichaos/rivescript-js) public API.

## Internet Freedom edition v2.0.0

![Internet Freedom release](iff2018-logo.png)

This release was prepared at the [IFF Internet Freedom Festival](https://internetfreedomfestival.org/) 2018 in Valencia, Spain.

* Cleaner, more powerful RiveScript Bot API
* Support for RiveScript error trapping
* Support for directions (see [What Are Direcrtions?](http://docs.rundexter.com/writing/bot/directions/))

## Installation instructions

**Work in progress** Detailed installation instructions will follow.

- [ ] Wrap [javascript interop](src/elm-rivescript.js) in an ES6 module.
- [ ] Include [RiveScript-JS](https://www.npmjs.com/package/rivescript) as a dependency.
- [ ] Package this interop and publish to NPM.

When these steps are completed, installation should look something like:

1. Install `elm-rivescript` as a dependency of your Elm app.

    `elm package install elm-rivescript`

2. Install `elm-rivescript` as a dependency of your project.

    `npm install elm-rivescript`

3. Initialise your bot from Javascript.

    *detailed instructions to folllow*

4. Include both your bundled javascript and your Elm app in your HTML source.

## Documentation

* This packages source files are well documented. See the documentation at [Elm Packages](http://package.elm-lang.org/packages/publeaks/elm-rivescript/latest).
* Also see the comments inline to `src/elm-rivescript.js` on [Github](https://github.com/Publeaks/elm-rivescript/blob/master/src/elm-rivescript.js).
* The repo contains a worked example at `examples/example.html`.

## License

[MIT License](https://choosealicense.com/licenses/mit/#)

Copyright &copy; 2018 [Stichting Free Press Unlimited](https://freepressunlimited.org)

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
