This is a Haskell library and a couple of sample binaries to parse data from
New Jersey Transit's [Departure Vision](http://m.njtransit.com/mo/mo_servlet.srv?hdnPageAction=DvTo).

The real problem we're solving here is that Departure Vision will tell you
when a train is expected, or that it HAS departed, but not WHEN it departed.
The way to gain that information is to watch the live data, wait for a train to
change from expected to departed, and figure out when the change happened.

To get the very basics, you can build ParseTrainFile and run it on any
Departure Vision train status page. That doesn't tell you more than you could
learn just by reading the HTML page. But it demonstrates the data in
NJTHistory's powerful algebraic datatype.

But here's how you can really learn something. Run this before or during the run
of, for example, train #1624:

$ ./track_train 1624

That will run for 2 and a half hours. After a few minutes, you can run:

$ ParseMultipleFiles datadir/1624-20170403*.html

That will output the historical performance of train 1624.

But the formatting is a little awkward. What does it mean when you see "("New York Penn Station",Departed (Just 2017-04-21 12:48:52 UTC) (Just 2017-04-21 12:49:52 UTC) (Just 2017-04-21 12:48:00 UTC))"

That's because the software has 3 possible guesses for when the train departed.
The first is the last timestamp it saw before the train departed. The
second is the first timestamp it saw after the train departed. The train was
listed as departed sometime in between these two times. The third timestamp is
the last "expected at" time listed. Which of these timestamps is the "true"
departure time is up to the user and how much benefit of the doubt she wants to
give NJT.

