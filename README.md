# cardano-log-parser

This is a log classifer for Cardano logs.<br>
What it does is run through given zipped log file and try to identify the issue
based on the knowledge base which is a csv file.<br>
Because we're using an csv file instead of web APIs, you can analyze the logs offline
which is a lot safer.

## Caution

- I'd highly recommend you to use this with an PC that does not have Daedalus installed and run it while offline.

- Biggest threat is someone releasing malicious log-classifier which will steal all your sensitive information (i.e private keys, passwords).

## Before using

- This will tell you **what** could be a issue your node is having and provide possible solutions.

- It will not try to fix the issue by itself (This is way too dangerous).

## How to use

WIP

## Todo

- [x] Read zip file
- [x] Use vector for efficiency
- [ ] Use reader
- [ ] Use regex to catch errors more nicely
- [ ] Provide more descriptive solution
- [ ] Catch suspcious lines and report them
- [ ] Analyze Daedalus log file
- [ ] Count the occurance of errors
- [ ] Pretty print the diagnosis
- [ ] Create test cases