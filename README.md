# cardano-log-parser

This is a log classifer for Cardano logs.

## How it works

- Classifier will read through given zipped log file and try to identify the issue based on the knowledge base which is a csv file.

- Because we're using an csv file instead of web APIs, you can analyze the logs offline which is a lot safer.

- Also it scales very nicely since all you have to do is add a record when new issue is found.

## Before using

- While you can use this with PC that has Daedalus installed, I'd highly recommend you to use this with an PC that does not have Daedalus and run it while offline for extra security.

- This is because the biggest threat for Daedalus user is someone releasing malicious log-classifier which will steal all the sensitive informations (i.e private keys, passwords).

- This will tell you what could be a issue your node is having and provide possible solutions.

- It will not try to fix the issue by itself (This way too dangerous).

## How to use

WIP

## Todo

- [x] Read zip file
- [x] Use vector for efficiency
- [x] Use reader monad
- [x] Test on windows, mac
- [ ] Provide more descriptive solution
- [ ] Catch suspcious lines and report them
- [ ] Analyze Daedalus log file
- [ ] Count the occurance of errors
- [ ] Pretty print the diagnosis
- [ ] Create test cases
- [ ] Use regex to catch errors more nicely
- [ ] (Maybe) use better format for knowledge base