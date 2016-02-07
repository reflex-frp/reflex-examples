NASA Astronomy Picture of the Day
=================================

`workshop.hs` is the result of a live-coding workshop held in New York on Feb 6, 2015 during the [Compose Unconference](http://www.composeconference.org/2016/unconference/).

We use Reflex.Dom and the NASA Open API and  to build a (very) simple viewer for NASA's [Astronomy Picture of the Day](http://apod.nasa.gov/apod/astropix.html) archive.

To compile the project, open a try-reflex shell and run `ghcjs workshop.hs` (if you are on Linux, you can run `ghc workshop.hs` as well).

To actually use the app, you'll need to get a [free API key from NASA](https://api.nasa.gov/index.html#apply-for-an-api-key). Enter the API key in the first text input element on the page and press submit. You should now see the current Astronomy Picture of the Day image from NASA. Entering a new date in the text input below the image should load the image for that date.

Caveat: During the workshop, we discovered that sometimes the JSON format used by NASA is different from the one specified herein, but we didn't bother dealing with that.
