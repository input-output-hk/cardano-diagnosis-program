# cardano-log-parser

This is a log classifer for Cardano logs.

## How it works

- Classifier will analyze given log file and try to identify the issue based on the knowledge base which is a csv file.

- Your log file does not contain any sensitive information so it's safe to read them.

- Because we're using an csv file instead of web APIs, you can analyze the logs offline which is a lot safer.

- Also it scales very nicely since all you have to do is add a record when new issue is found.

## Before using

- While you can use this with PC that has Daedalus installed, I'd highly recommend you to use this with an PC that does not have Daedalus and run it while offline for extra security.

- This is because the biggest threat for Daedalus user is someone releasing malicious log-classifier which will steal all the sensitive informations (i.e private keys, passwords).

- All this program does is read log file then generate html file, nothing else.

- This will tell you what could be a issue your node is having and provide possible solutions.

- It will not try to fix the issue by itself. (**This way too dangerous**).

- This cannot catch error that is unknown. (How can you catch something you don't know what it is?)

## How to use

WIP

## Working environments

- [ ] Windows
- [ ] MacOS
- [ ] Linux

## Todo

- [x] Read zip file
- [x] Use vector for efficiency
- [x] Use reader monad
- [x] Test on windows, mac
- [x] Provide more descriptive solution
- [x] Analyze Daedalus log file
- [x] Pretty print the diagnosis
- [x] Find the way to facilitate the file execution
- [x] Refactor coding style according to cardano-coding-style [here](https://github.com/input-output-hk/cardano-sl-style-guides/blob/master/haskell-style-guide.md)
- [x] Use blaze-html to render out the output
- [x] Collect any lines that is associated with known bugs
- [ ] Use pandoc to read text as markdown then convert into Html
- [ ] Ask for permission to create github repo.
- [ ] Find a way to access log file in stable way.
- [ ] Put more description on Readme (plans)
- [ ] Ask design team for css stylesheet
- [ ] Count the occurance of error
- [ ] Create test cases
- [ ] Use regex to catch errors more nicely
- [ ] Provide analysis based on locale

## Issue

- I'd have to admit, the analysis is not done in nice way (close to brute forcing the logs). This is causing performance issues. If there's anyone who has knowledge of haskell, please take a look at `Classifiers.hs

## Future plans

- Ask for haskellers for feedback

- Release beta version before 1.2

- Release decent version after Cardano 1.2 has been released (from end of May to early June).

- Integrate similiar system to Daedalus (**eta Unknown. Maybe, possibly, hopefully.**). What I would say is that the functionality of this program is completely straight forward. You can probably implement this in any language.