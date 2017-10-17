# Condenser
## A Streaming statistics computation engine for Twitter

### Ryan Delucchi, nary.io || fp.engineering

#### Implemented in Scala, utilizing all the right stuff.


The Condenser gathers interesting statistics by observing the Twitter sample stream and continuously updating them 
in real-time.  Amoung these statistics are information related to an emoji definition file, included here
(Thanks to the kind contribution of the [The Emoji Data Project](https://github.com/iamcal/emoji-data)).  

Launching the Condenser is a simple matter of running SBT with the necessary arguments to authenticate through your Twitter account:

```
sbt "run <consumer key> <consumer secret> <access token> <access secret>"
```

Please consult the [Twitter Developer's documentation](https://developer.twitter.com/en/docs/basics/getting-started) for more details.

Once the Condenser is up and running, you can observe a live dashboard of statistics (which refresh every second), as they are being computed, simply by pointing your browser to the following URL:

```
localhost:9000/tweets/statistics
```
