
# Reflex Host examples

It is a bit trickier to build an event loop in `reflex` than in `reactive-banana`.
This is because the facilities provided by `reflex` are lower-level and more fine-grained.

If you want something higher level you can use, there is [`refex-host`](https://github.com/bennofs/reflex-host).

If you want to have a look at how to use the various low-level pieces and various things that you can do with them, this is the folder for you.

The canonical example is [host.hs](https://github.com/reflex-frp/reflex-platform/blob/develop/examples/host.hs), and much of the work here is derived from that.

## [Host1](./src/Host1.hs)

The simplest example rigs up an event loop for a pure event network which has an `Event` as an input and a `Behavior` as an output.

## [Host2](./src/Host2.hs)

The next example rigs up an event loop for a pure event network which has an `Event` as an input and a `Event` as an output.

## [Host3](./src/Host3.hs)

We now change the example so that we have multiple events for both the inputs and the outputs. 

## [Host4](./src/Host4.hs)

We add `PostBuild` here, to give us easy access to an event which fires when our event loop starts.

## [Host5](./src/Host5.hs)

We add `PerformEvent` here, so that we can do `IO` inside of our event network instead of bolting it on afterwards.

