# Cardano diagnosis program (DD)

This is a diagnosis program for Cardano-SL. The goal of this program is to produce an diagnosis program for Cardano-SL that can be easily used by the end user.

## What is this for

This is for anyone who has issues with their Daedalus wallet and want to run diagnosis on it.

## How it works

- Progam will analyze given log file and try to identify the issue based on the knowledge base which is a csv file.

- It then generates an static Html file which describes the issue of your node and provide possible solution to it.

- Because we're using an csv file instead of web APIs, you can analyze the logs offline which is a lot safer.

- Also it scales very nicely since all you need to do is add a new record to CSV file and add few lines of code to the CSV parser.

## Before using

- Log file does not contain any sensitive information so it's safe to read them.

- I'd highly recommend you to run this while disconnected to the internet.

- This is because the biggest threat for Daedalus user is someone releasing malicious log-classifier which will steal all the sensitive informations (i.e private keys, passwords) and send it somewhere. Obviously, this program does not do such things.

- All this program does is read log file, analyze it, then generate a html file, nothing else.

- This will tell you what could be a issue your node is having and provide possible solutions.

- It will not try to fix the issue by itself. (**This way too dangerous**).

- This cannot catch error that is unknown. (How can you catch something you don't know what it is?)

## Installation

> `stack build`

## How to use

You'll need a zipped/archived Cardano log folder then run the command below
> `stack exec diagnosis "Path to logs"`

## Working environments

- [x] Windows
- [x] MacOS
- [x] Linux

It worked on all the machine that I have, but I do still want more testing on it..

## Issue

- I'd have to admit, the analysis is not done in nice way (close to brute forcing the logs). This is causing performance issues. If there's anyone who has knowledge of haskell, please take a look at `Classifier.hs`

## Future plans

- Ask for haskellers for feedback

- Release beta version before 1.2

- Release improved version after Cardano 1.2 has been released (from end of May to early June).

- Integrate similiar system to Daedalus (**eta Unknown. Maybe, possibly, hopefully.**). What I would say is that the functionality of this program is completely straight forward. You can probably implement this in any language (But haskell is the easiest)

## Todo

- [x] Read zip file
- [x] Use state monad
- [x] Test on windows, mac
- [x] Provide more descriptive solution
- [x] Analyze Daedalus log file
- [x] Pretty print the diagnosis
- [x] Find the way to facilitate the file execution
- [x] Refactor coding style according to cardano-coding-style [here](https://github.com/input-output-hk/cardano-sl-style-guides/blob/master/haskell-style-guide.md)
- [x] Use blaze-html to render out the output
- [x] Collect any lines that is associated with known bugs
- [x] Implement naive way of rendering solution in blaze html
- [x] Ask for permission to create github repo.
- [x] In `renderSolution`, hightlight ceratin keyword to make it more readable
- [x] Find a way to access log file in stable way.
- [x] Put more description on Readme (plans)
- [x] Use safe-exception to handle exception
- [x] Change Debugger so that No Error and Unknown Error have different output.
- [x] Create bash script for setting up the executable
- [ ] Ask design team for css stylesheet
- [ ] Count the occurance of error
- [ ] Create test cases
- [ ] Use regex to catch errors more nicely
- [ ] Provide analysis result based on locale