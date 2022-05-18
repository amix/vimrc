defmodule GenStage do
  @moduledoc ~S"""
  Stages are data-exchange steps that send and/or receive data
  from other stages.

  When a stage sends data, it acts as a producer. When it receives
  data, it acts as a consumer. Stages may take both producer and
  consumer roles at once.

  ## Stage types

  Besides taking both producer and consumer roles, a stage may be
  called "source" if it only produces items or called "sink" if it
  only consumes items.

  For example, imagine the stages below where A sends data to B
  that sends data to C:

      [A] -> [B] -> [C]

  we conclude that:

    * A is only a producer (and therefore a source)
    * B is both producer and consumer
    * C is only a consumer (and therefore a sink)

  As we will see in the upcoming Examples section, we must
  specify the type of the stage when we implement each of them.

  To start the flow of events, we subscribe consumers to
  producers. Once the communication channel between them is
  established, consumers will ask the producers for events.
  We typically say the consumer is sending demand upstream.
  Once demand arrives, the producer will emit items, never
  emitting more items than the consumer asked for. This provides
  a back-pressure mechanism.

  A consumer may have multiple producers and a producer may have
  multiple consumers. When a consumer asks for data, each producer
  is handled separately, with its own demand. When a producer
  receives demand and sends data to multiple consumers, the demand
  is tracked and the events are sent by a dispatcher. This allows
  producers to send data using different "strategies". See
  `GenStage.Dispatcher` for more information.

  Many developers tend to create layers of stages, such as A, B and
  C, for achieving concurrency. If all you want is concurrency, using
  processes is enough. They are the primitive for achieving concurrency
  in Elixir and the VM does all of the work of multiplexing them.
  Instead, layers in GenStage must be created when there is a need for
  back-pressure or to route the data in different ways.

  For example, if you need the data to go over multiple steps but
  without a need for back-pressure or without a need to break the
  data apart, do not design it as such:

      [Producer] -> [Step 1] -> [Step 2] -> [Step 3]

  Instead it is better to design it as:

                   [Consumer]
                  /
      [Producer]-<-[Consumer]
                  \
                   [Consumer]

  where "Consumer" are multiple processes that subscribe to the same
  "Producer" and run exactly the same code, with all of transformation
  steps from above. In such scenarios, you may even find the
  `Task.async_stream/2` function that ships as part of Elixir to be
  enough or achieve the flexibility you need with the `ConsumerSupervisor`
  functionality that is included as part of `GenStage`.

  ## Example

  Let's define the simple pipeline below:

      [A] -> [B] -> [C]

  where A is a producer that will emit items starting from 0,
  B is a producer-consumer that will receive those items and
  multiply them by a given number and C will receive those events
  and print them to the terminal.

  Let's start with A. Since A is a producer, its main
  responsibility is to receive demand and generate events.
  Those events may be in memory or an external queue system.
  For simplicity, let's implement a simple counter starting
  from a given value of `counter` received on `init/1`:

      defmodule A do
        use GenStage

        def start_link(number) do
          GenStage.start_link(A, number)
        end

        def init(counter) do
          {:producer, counter}
        end

        def handle_demand(demand, counter) when demand > 0 do
          # If the counter is 3 and we ask for 2 items, we will
          # emit the items 3 and 4, and set the state to 5.
          events = Enum.to_list(counter..counter+demand-1)
          {:noreply, events, counter + demand}
        end
      end

  B is a producer-consumer. This means it does not explicitly
  handle the demand because the demand is always forwarded to
  its producer. Once A receives the demand from B, it will send
  events to B which will be transformed by B as desired. In
  our case, B will receive events and multiply them by a number
  given on initialization and stored as the state:

      defmodule B do
        use GenStage

        def start_link(number) do
          GenStage.start_link(B, number)
        end

        def init(number) do
          {:producer_consumer, number}
        end

        def handle_events(events, _from, number) do
          events = Enum.map(events, & &1 * number)
          {:noreply, events, number}
        end
      end

  C will finally receive those events and print them every second
  to the terminal:

      defmodule C do
        use GenStage

        def start_link() do
          GenStage.start_link(C, :ok)
        end

        def init(:ok) do
          {:consumer, :the_state_does_not_matter}
        end

        def handle_events(events, _from, state) do
          # Wait for a second.
          Process.sleep(1000)

          # Inspect the events.
          IO.inspect(events)

          # We are a consumer, so we would never emit items.
          {:noreply, [], state}
        end
      end

  Now we can start and connect them:

      {:ok, a} = A.start_link(0)  # starting from zero
      {:ok, b} = B.start_link(2)  # multiply by 2
      {:ok, c} = C.start_link()   # state does not matter

      GenStage.sync_subscribe(c, to: b)
      GenStage.sync_subscribe(b, to: a)

  Typically, we subscribe from bottom to top. Since A will
  start producing items only when B connects to it, we want this
  subscription to happen when the whole pipeline is ready. After
  you subscribe all of them, demand will start flowing upstream and
  events downstream.

  When implementing consumers, we often set the `:max_demand` and
  `:min_demand` on subscription. The `:max_demand` specifies the
  maximum amount of events that must be in flow while the `:min_demand`
  specifies the minimum threshold to trigger for more demand. For
  example, if `:max_demand` is 1000 and `:min_demand` is 750,
  the consumer will ask for 1000 events initially and ask for more
  only after it receives at least 250.

  In the example above, B is a `:producer_consumer` and therefore
  acts as a buffer. Getting the proper demand values in B is
  important: making the buffer too small may make the whole pipeline
  slower, making the buffer too big may unnecessarily consume
  memory.

  When such values are applied to the stages above, it is easy
  to see the producer works in batches. The producer A ends-up
  emitting batches of 50 items which will take approximately
  50 seconds to be consumed by C, which will then request another
  batch of 50 items.

  ## `init` and `:subscribe_to`

  In the example above, we have started the processes A, B, and C
  independently and subscribed them later on. But most often it is
  simpler to subscribe a consumer to its producer on its `c:init/1`
  callback. This way, if the consumer crashes, restarting the consumer
  will automatically re-invoke its `c:init/1` callback and resubscribe
  it to the supervisor.

  This approach works as long as the producer can be referenced when
  the consumer starts--such as by name (for a named process) or by pid
  for a running unnamed process. For example, assuming the process
  `A` and `B` are started as follows:

      # Let's call the stage in module A as A
      GenStage.start_link(A, 0, name: A)
      # Let's call the stage in module B as B
      GenStage.start_link(B, 2, name: B)
      # No need to name consumers as they won't be subscribed to
      GenStage.start_link(C, :ok)

  We can now change the `c:init/1` callback for C to the following:

      def init(:ok) do
        {:consumer, :the_state_does_not_matter, subscribe_to: [B]}
      end

  or:

      def init(:ok) do
        {:consumer, :the_state_does_not_matter, subscribe_to: [{B, options}]}
      end

  And we will no longer need to call `sync_subscribe/2`.

  Another advantage of this approach is that it makes it straight-forward
  to leverage concurrency by simply starting multiple consumers that subscribe
  to their producer (or producer-consumer). This can be done in the example above
  by simply calling start link multiple times:

      # Start 4 consumers
      GenStage.start_link(C, :ok)
      GenStage.start_link(C, :ok)
      GenStage.start_link(C, :ok)
      GenStage.start_link(C, :ok)

  In a supervision tree, this is often done by starting multiple workers:

      children = [
        worker(A, [0]),
        worker(B, [2]),
        worker(C, []),
        worker(C, []),
        worker(C, []),
        worker(C, [])
      ]

      Supervisor.start_link(children, strategy: :one_for_one)

  In fact, having multiple consumers is often the easiest and simplest way to
  leverage concurrency in a GenStage pipeline, especially if events can
  be processed out of order. For example, imagine a scenario where you
  have a stream of incoming events and you need to access a number of
  external services per event. Instead of building complex stages that
  route events through those services, one simple mechanism to leverage
  concurrency is to start a producer and N consumers and invoke the external
  services directly for each event in each consumer. N is typically the
  number of cores (as returned by `System.schedulers_online/0`) but can
  likely be increased if the consumers are mostly waiting on IO.

  Another alternative to the scenario above is to use a `ConsumerSupervisor`
  for consuming the events instead of N consumers. The `ConsumerSupervisor`
  will start a separate supervised process per event where the number of children
  is at most `max_demand` and the average amount of children is
  `(max_demand - min_demand) / 2`.

  ## Buffering

  In many situations, producers may attempt to emit events while no consumers
  have yet subscribed. Similarly, consumers may ask producers for events
  that are not yet available. In such cases, it is necessary for producers
  to buffer events until a consumer is available or buffer the consumer
  demand until events arrive, respectively. As we will see next, buffering
  events can be done automatically by `GenStage`, while buffering the demand
  is a case that must be explicitly considered by developers implementing
  producers.

  ### Buffering events

  Due to the concurrent nature of Elixir software, sometimes a producer
  may dispatch events without consumers to send those events to. For example,
  imagine a `:consumer` B subscribes to `:producer` A. Next, the consumer B
  sends demand to A, which starts producing events to satisfy the demand.
  Now, if the consumer B crashes, the producer may attempt to dispatch the
  now produced events but it no longer has a consumer to send those events to.
  In such cases, the producer will automatically buffer the events until another
  consumer subscribes.

  The buffer can also be used in cases where external sources only send
  events in batches larger than asked for. For example, if you are
  receiving events from an external source that only sends events
  in batches of 1000 and the internal demand is smaller than
  that, the buffer allows you to always emit batches of 1000 events
  even when the consumer has asked for less.

  In all of those cases when an event cannot be sent immediately by
  a producer, the event will be automatically stored and sent the next
  time consumers ask for events. The size of the buffer is configured
  via the `:buffer_size` option returned by `init/1` and the default
  value is `10_000`. If the `buffer_size` is exceeded, an error is logged.
  See the documentation for `c:init/1` for more detailed infromation about
  the `:buffer_size` option.

  ### Buffering demand

  In case consumers send demand and the producer is not yet ready to
  fill in the demand, producers must buffer the demand until data arrives.

  As an example, let's implement a producer that broadcasts messages
  to consumers. For producers, we need to consider two scenarios:

    1. what if events arrive and there are no consumers?
    2. what if consumers send demand and there are not enough events?

  One way to implement such a broadcaster is to simply rely on the internal
  buffer available in `GenStage`, dispatching events as they arrive, as explained
  in the previous section:

      defmodule Broadcaster do
        use GenStage

        @doc "Starts the broadcaster."
        def start_link() do
          GenStage.start_link(__MODULE__, :ok, name: __MODULE__)
        end

        @doc "Sends an event and returns only after the event is dispatched."
        def sync_notify(event, timeout \\ 5000) do
          GenStage.call(__MODULE__, {:notify, event}, timeout)
        end

        def init(:ok) do
          {:producer, :ok, dispatcher: GenStage.BroadcastDispatcher}
        end

        def handle_call({:notify, event}, _from, state) do
          {:reply, :ok, [event], state} # Dispatch immediately
        end

        def handle_demand(_demand, state) do
          {:noreply, [], state} # We don't care about the demand
        end
      end

  By always sending events as soon as they arrive, if there is any demand,
  we will serve the existing demand, otherwise the event will be queued in
  `GenStage`'s internal buffer. In case events are being queued and not being
  consumed, a log message will be emitted when we exceed the `:buffer_size`
  configuration.

  While the implementation above is enough to solve the constraints above,
  a more robust implementation would have tighter control over the events
  and demand by tracking this data locally, leaving the `GenStage` internal
  buffer only for cases where consumers crash without consuming all data.

  To handle such cases, we will use a two-element tuple as the broadcaster state
  where the first elemeent is a queue and the second element is the pending
  demand.  When events arrive and there are no consumers, we will store the
  event in the queue alongside information about the process that broadcasted
  the event. When consumers send demand and there are not enough events, we will
  increase the pending demand.  Once we have both data and demand, we
  acknowledge the process that has sent the event to the broadcaster and finally
  broadcast the event downstream.

      defmodule QueueBroadcaster do
        use GenStage

        @doc "Starts the broadcaster."
        def start_link() do
          GenStage.start_link(__MODULE__, :ok, name: __MODULE__)
        end

        @doc "Sends an event and returns only after the event is dispatched."
        def sync_notify(event, timeout \\ 5000) do
          GenStage.call(__MODULE__, {:notify, event}, timeout)
        end

        ## Callbacks

        def init(:ok) do
          {:producer, {:queue.new, 0}, dispatcher: GenStage.BroadcastDispatcher}
        end

        def handle_call({:notify, event}, from, {queue, pending_demand}) do
          queue = :queue.in({from, event}, queue)
          dispatch_events(queue, pending_demand, [])
        end

        def handle_demand(incoming_demand, {queue, pending_demand}) do
          dispatch_events(queue, incoming_demand + pending_demand, [])
        end

        defp dispatch_events(queue, 0, events) do
          {:noreply, Enum.reverse(events), {queue, 0}}
        end

        defp dispatch_events(queue, demand, events) do
          case :queue.out(queue) do
            {{:value, {from, event}}, queue} ->
              GenStage.reply(from, :ok)
              dispatch_events(queue, demand - 1, [event | events])
            {:empty, queue} ->
              {:noreply, Enum.reverse(events), {queue, demand}}
          end
        end
      end

  Let's also implement a consumer that automatically subscribes to the
  broadcaster on `c:init/1`. The advantage of doing so on initialization
  is that, if the consumer crashes while it is supervised, the subscription
  is automatically re-established when the supervisor restarts it.

      defmodule Printer do
        use GenStage

        @doc "Starts the consumer."
        def start_link() do
          GenStage.start_link(__MODULE__, :ok)
        end

        def init(:ok) do
          # Starts a permanent subscription to the broadcaster
          # which will automatically start requesting items.
          {:consumer, :ok, subscribe_to: [QueueBroadcaster]}
        end

        def handle_events(events, _from, state) do
          for event <- events do
            IO.inspect {self(), event}
          end
          {:noreply, [], state}
        end
      end

  With the broadcaster in hand, now let's start the producer as well
  as multiple consumers:

      # Start the producer
      QueueBroadcaster.start_link()

      # Start multiple consumers
      Printer.start_link()
      Printer.start_link()
      Printer.start_link()
      Printer.start_link()

  At this point, all consumers must have sent their demand which we were not
  able to fulfill. Now by calling `QueueBroadcaster.sync_notify/1`, the event
  shall be broadcasted to all consumers at once as we have buffered the demand
  in the producer:

      QueueBroadcaster.sync_notify(:hello_world)

  If we had called `QueueBroadcaster.sync_notify(:hello_world)` before any
  consumer was available, the event would also have been buffered in our own
  queue and served only when demand had been received.

  By having control over the demand and queue, the broadcaster has
  full control on how to behave when there are no consumers, when the
  queue grows too large, and so forth.

  ## Asynchronous work and `handle_subscribe`

  Both `:producer_consumer` and `:consumer` stages have been designed to do
  their work in the `c:handle_events/3` callback. This means that, after
  `c:handle_events/3` is invoked, both `:producer_consumer` and `:consumer`
  stages will immediately send demand upstream and ask for more items, as the
  stage that produced the events assumes events have been fully processed by
  `c:handle_events/3`.

  Such default behaviour makes `:producer_consumer` and `:consumer` stages
  unfeasible for doing asynchronous work. However, given `GenStage` was designed
  to run with multiple consumers, it is not a problem to perform synchronous or
  blocking actions inside `handle_events/3` as you can then start multiple
  consumers in order to max both CPU and IO usage as necessary.

  On the other hand, if you must perform some work asynchronously,
  `GenStage` comes with an option that manually controls how demand
  is sent upstream, avoiding the default behaviour where demand is
  sent after `c:handle_events/3`. Such can be done by implementing
  the `c:handle_subscribe/4` callback and returning `{:manual, state}`
  instead of the default `{:automatic, state}`. Once the producer mode
  is set to `:manual`, developers must use `GenStage.ask/3` to send
  demand upstream when necessary.

  Note that when `:max_demand` and `:min_demand` must be manually respected when
  manually asking for demand through `GenStage.ask/3`.

  For example, the `ConsumerSupervisor` module processes events
  asynchronously by starting a process for each event and this is achieved by
  manually sending demand to producers. `ConsumerSupervisor`
  can be used to distribute work to a limited amount of
  processes, behaving similar to a pool where a new process is
  started for each event. See the `ConsumerSupervisor` docs for more
  information.

  Setting the demand to `:manual` in `c:handle_subscribe/4` is not
  only useful for asynchronous work but also for setting up other
  mechanisms for back-pressure. As an example, let's implement a
  consumer that is allowed to process a limited number of events
  per time interval. Those are often called rate limiters:

      defmodule RateLimiter do
        use GenStage

        def init(_) do
          # Our state will keep all producers and their pending demand
          {:consumer, %{}}
        end

        def handle_subscribe(:producer, opts, from, producers) do
          # We will only allow max_demand events every 5000 milliseconds
          pending = opts[:max_demand] || 1000
          interval = opts[:interval] || 5000

          # Register the producer in the state
          producers = Map.put(producers, from, {pending, interval})
          # Ask for the pending events and schedule the next time around
          producers = ask_and_schedule(producers, from)

          # Returns manual as we want control over the demand
          {:manual, producers}
        end

        def handle_cancel(_, from, producers) do
          # Remove the producers from the map on unsubscribe
          {:noreply, [], Map.delete(producers, from)}
        end

        def handle_events(events, from, producers) do
          # Bump the amount of pending events for the given producer
          producers = Map.update!(producers, from, fn {pending, interval} ->
            {pending + length(events), interval}
          end)

          # Consume the events by printing them.
          IO.inspect(events)

          # A producer_consumer would return the processed events here.
          {:noreply, [], producers}
        end

        def handle_info({:ask, from}, producers) do
          # This callback is invoked by the Process.send_after/3 message below.
          {:noreply, [], ask_and_schedule(producers, from)}
        end

        defp ask_and_schedule(producers, from) do
          case producers do
            %{^from => {pending, interval}} ->
              # Ask for any pending events
              GenStage.ask(from, pending)
              # And let's check again after interval
              Process.send_after(self(), {:ask, from}, interval)
              # Finally, reset pending events to 0
              Map.put(producers, from, {0, interval})
            %{} ->
              producers
          end
        end
      end

  Let's subscribe the `RateLimiter` above to the
  producer we have implemented at the beginning of the module
  documentation:

      {:ok, a} = GenStage.start_link(A, 0)
      {:ok, b} = GenStage.start_link(RateLimiter, :ok)

      # Ask for 10 items every 2 seconds
      GenStage.sync_subscribe(b, to: a, max_demand: 10, interval: 2000)

  Although the rate limiter above is a consumer, it could be made a
  producer-consumer by changing `c:init/1` to return a `:producer_consumer`
  and then forwarding the events in `c:handle_events/3`.

  ## Notifications

  `GenStage` also supports the ability to send notifications to all
  consumers. Those notifications are sent as regular messages outside
  of the demand-driven protocol but respecting the event ordering.
  See `sync_notify/3` and `async_notify/2`.

  Notifications are useful for out-of-band information, for example,
  to notify consumers the producer has sent all events it had to
  process or that a new batch of events is starting.

  Note the notification system should not be used for broadcasting
  events; for such, consider using `GenStage.BroadcastDispatcher`.

  ## Callbacks

  `GenStage` is implemented on top of a `GenServer` with a few additions.
  Besides exposing all of the `GenServer` callbacks, it also provides
  `handle_demand/2` to be implemented by producers and `handle_events/3` to be
  implemented by consumers, as shown above, as well as subscription-related
  callbacks. Furthermore, all the callback responses have been modified to
  potentially emit events. See the callbacks documentation for more
  information.

  By adding `use GenStage` to your module, Elixir will automatically
  define all callbacks for you except for the following ones:

    * `init/1` - must be implemented to choose between `:producer`, `:consumer`, or `:producer_consumer` stages
    * `handle_demand/2` - must be implemented by `:producer` stages
    * `handle_events/3` - must be implemented by `:producer_consumer` and `:consumer` stages

  Although this module exposes functions similar to the ones found in
  the `GenServer` API, like `call/3` and `cast/2`, developers can also
  rely directly on GenServer functions such as `GenServer.multi_call/4`
  and `GenServer.abcast/3` if they wish to.

  ### Name registration

  `GenStage` is bound to the same name registration rules as a `GenServer`.
  Read more about it in the `GenServer` docs.

  ## Message protocol overview

  This section will describe the message protocol implemented
  by stages. By documenting these messages, we will allow
  developers to provide their own stage implementations.

  ### Back-pressure

  When data is sent between stages, it is done by a message
  protocol that provides back-pressure. The first step is
  for the consumer to subscribe to the producer. Each
  subscription has a unique reference.

  Once subscribed, the consumer may ask the producer for messages
  for the given subscription. The consumer may demand more items
  whenever it wants to. A consumer must never receive more data
  than it has asked for from any given producer stage.

  A consumer may have multiple producers, where each demand is managed
  individually (on a per-subscription basis). A producer may have multiple
  consumers, where the demand and events are managed and delivered according to
  a `GenStage.Dispatcher` implementation.

  ### Producer messages

  The producer is responsible for sending events to consumers
  based on demand. These are the messages that consumers can
  send to producers:

    * `{:"$gen_producer", from :: {consumer_pid, subscription_tag}, {:subscribe, current, options}}` -
      sent by the consumer to the producer to start a new subscription.

      Before sending, the consumer MUST monitor the producer for clean-up
      purposes in case of crashes. The `subscription_tag` is unique to
      identify the subscription. It is typically the subscriber monitoring
      reference although it may be any term.

      Once sent, the consumer MAY immediately send demand to the producer.

      The `current` field, when not `nil`, is a two-item tuple containing a
      subscription that must be cancelled with the given reason before the
      current one is accepted.

      Once received, the producer MUST monitor the consumer. However, if
      the subscription reference is known, it MUST send a `:cancel` message
      to the consumer instead of monitoring and accepting the subscription.

    * `{:"$gen_producer", from :: {consumer_pid, subscription_tag}, {:cancel, reason}}` -
      sent by the consumer to cancel a given subscription.

      Once received, the producer MUST send a `:cancel` reply to the
      registered consumer (which may not necessarily be the one received
      in the tuple above). Keep in mind, however, there is no guarantee
      such messages can be delivered in case the producer crashes before.
      If the pair is unknown, the producer MUST send an appropriate cancel
      reply.

    * `{:"$gen_producer", from :: {consumer_pid, subscription_tag}, {:ask, demand}}` -
      sent by consumers to ask demand for a given subscription (identified
      by `subscription_tag`).

      Once received, the producer MUST send data up to the demand. If the
      pair is unknown, the producer MUST send an appropriate cancel reply.

  ### Consumer messages

  The consumer is responsible for starting the subscription
  and sending demand to producers. These are the messages that
  producers can send to consumers:

    * `{:"$gen_consumer", from :: {producer_pid, subscription_tag}, {:notification, message}}` -
      notifications sent by producers.

    * `{:"$gen_consumer", from :: {producer_pid, subscription_tag}, {:cancel, reason}}` -
      sent by producers to cancel a given subscription.

      It is used as a confirmation for client cancellations OR
      whenever the producer wants to cancel some upstream demand.

    * `{:"$gen_consumer", from :: {producer_pid, subscription_tag}, events :: [event, ...]}` -
      events sent by producers to consumers.

      `subscription_tag` identifies the subscription. The third argument
      is a non-empty list of events. If the subscription is unknown, the
      events must be ignored and a cancel message must be sent to the producer.

  """

  defstruct [:mod, :state, :type, :dispatcher_mod, :dispatcher_state, :buffer,
             :buffer_config, events: :forward, monitors: %{}, producers: %{}, consumers: %{}]

  @typedoc "The supported stage types."
  @type type :: :producer | :consumer | :producer_consumer

  @typedoc "The supported init options."
  @type options :: keyword()

  @typedoc "The stage."
  @type stage :: pid | atom | {:global, term} | {:via, module, term} | {atom, node}

  @typedoc "The term that identifies a subscription."
  @opaque subscription_tag :: reference

  @typedoc "The term that identifies a subscription associated with the corresponding producer/consumer."
  @type from :: {pid, subscription_tag}

  @doc """
  Invoked when the server is started.

  `start_link/3` (or `start/3`) will block until this callback returns.
  `args` is the argument term (second argument) passed to `start_link/3`
  (or `start/3`).

  In case of successful start, this callback must return a tuple
  where the first element is the stage type, which is one of:

    * `:producer`
    * `:consumer`
    * `:producer_consumer` (if the stage is acting as both)

  For example:

      def init(args) do
        {:producer, some_state}
      end

  The returned tuple may also contain 3 or 4 elements. The third
  element may be the `:hibernate` atom or a set of options defined
  below.

  Returning `:ignore` will cause `start_link/3` to return `:ignore`
  and the process will exit normally without entering the loop or
  calling `terminate/2`.

  Returning `{:stop, reason}` will cause `start_link/3` to return
  `{:error, reason}` and the process to exit with reason `reason`
  without entering the loop or calling `terminate/2`.

  ## Options

  This callback may return options. Some options are specific to
  the chosen stage type while others are shared across all types.

  ### `:producer` options

    * `:demand` - when `:forward`, the demand is always forwarded to
      the `c:handle_demand/2` callback. When `:accumulate`, demand is
      accumulated until its mode is set to `:forward` via `demand/2`.
      This is useful as a synchronization mechanism, where the demand
      is accumulated until all consumers are subscribed. Defaults to
      `:forward`.

  ### `:producer` and `:producer_consumer` options

    * `:buffer_size` - the size of the buffer to store events without
      demand. Can be `:infinity` to signal no limit on the buffer size. Check
      the "Buffer events" section of the module documentation. Defaults to
      `10_000` for `:producer`, `:infinity` for `:producer_consumer`.

    * `:buffer_keep` - returns whether the `:first` or `:last` entries
      should be kept on the buffer in case the buffer size is exceeded.
      Defaults to `:last`.

    * `:dispatcher` - the dispatcher responsible for handling demands.
      Defaults to `GenStage.DemandDispatch`. May be either an atom
      representing a dispatcher module or a two-element tuple with
      the dispatcher module and the dispatcher options.

  ### `:consumer` and `:producer_consumer` options

    * `:subscribe_to` - a list of producers to subscribe to. Each element
      represents either the producer module or a tuple with the producer module
      and the subscription options (as defined in `sync_subscribe/2`).

  """
  @callback init(args :: term) ::
    {type, state} |
    {type, state, options} |
    :ignore |
    {:stop, reason :: any} when state: any

  @doc """
  Invoked on `:producer` stages.

  This callback is invoked on `:producer` stages with the demand from
  consumers/dispatcher. The producer that implements this callback must either
  store the demand, or return the amount of requested events.

  Must always be explicitly implemented by `:producer` stages.

  ## Examples

      def handle_demand(demand, state) do
        # We check if we're able to satisfy the demand and fetch
        # events if we aren't.
        events =
          if length(state.events) >= demand do
            events
          else
            fetch_events()
          end

        # We dispatch only the requested number of events.
        {to_dispatch, remaining} = Enum.split(events, demand)

        {:noreply, to_dispatch, %{state | events: remaining}}
      end

  """
  @callback handle_demand(demand :: pos_integer, state :: term) ::
    {:noreply, [event], new_state} |
    {:noreply, [event], new_state, :hibernate} |
    {:stop, reason, new_state} when new_state: term, reason: term, event: term

  @doc """
  Invoked when a consumer subscribes to a producer.

  This callback is invoked in both producers and consumers.
  `producer_or_consumer` will be `:producer` when this callback is
  invoked on a consumer that subscribed to a producer, and `:consumer`
  if when this callback is invoked on producers a consumer subscribed to.

  For consumers, successful subscriptions must return one of:

    * `{:automatic, new_state}` - means the stage implementation will take care
      of automatically sending demand to producers. This is the default.

    * `{:manual, state}` - means that demand must be sent to producers
      explicitly via `ask/3`. `:manual` subscriptions must be cancelled when
      `c:handle_cancel/3` is called. `:manual` can be used when a special
      behaviour is desired (for example, `ConsumerSupervisor` uses `:manual`
      demand in its implementation).

  For producers, successful subscriptions must always return
  `{:automatic, new_state}`. `:manual` mode is not supported.

  If this callback is not implemented, the default implementation by
  `use GenStage` will return `{:automatic, state}`.

  ## Examples

  Let's see an example where we define this callback in a consumer that will use
  `:manual` mode. In this case, we'll store the subscription (`from`) in the
  state in order to be able to use it later on when asking demand via `ask/3`.

      def handle_subscribe(:producer, _options, from, state) do
        new_state = %{state | subscription: from}
        {:manual, new_state
      end

  """
  @callback handle_subscribe(producer_or_consumer :: :producer | :consumer, options, from, state :: term) ::
    {:automatic | :manual, new_state} |
    {:stop, reason, new_state} when new_state: term, reason: term

  @doc """
  Invoked when a consumer is no longer subscribed to a producer.

  It receives the cancellation reason, the `from` tuple representing the
  cancelled subscription and the state.  The `cancel_reason` will be a
  `{:cancel, _}` tuple if the reason for cancellation was a `GenStage.cancel/2`
  call. Any other value means the cancellation reason was due to an EXIT.

  If this callback is not implemented, the default implementation by
  `use GenStage` will return `{:noreply, [], state}`.

  Return values are the same as `c:handle_cast/2`.
  """
  @callback handle_cancel(cancellation_reason :: {:cancel | :down, reason :: term}, from, state :: term) ::
    {:noreply, [event], new_state} |
    {:noreply, [event], new_state, :hibernate} |
    {:stop, reason, new_state} when event: term, new_state: term, reason: term

  @doc """
  Invoked on `:producer_consumer` and `:consumer` stages to handle events.

  Must always be explicitly implemented by such types.

  Return values are the same as `c:handle_cast/2`.
  """
  @callback handle_events(events :: [event], from, state :: term) ::
    {:noreply, [event], new_state} |
    {:noreply, [event], new_state, :hibernate} |
    {:stop, reason, new_state} when new_state: term, reason: term, event: term

  @doc """
  Invoked to handle synchronous `call/3` messages.

  `call/3` will block until a reply is received (unless the call times out or
  nodes are disconnected).

  `request` is the request message sent by a `call/3`, `from` is a two-element tuple
  containing the caller's PID and a term that uniquely identifies the call, and
  `state` is the current state of the `GenStage`.

  Returning `{:reply, reply, [events], new_state}` sends the response `reply`
  to the caller after events are dispatched (or buffered) and continues the
  loop with new state `new_state`. In case you want to deliver the reply before
  processing events, use `reply/2` and return `{:noreply, [event],
  state}`.

  Returning `{:noreply, [event], new_state}` does not send a response to the
  caller and processes the given events before continuing the loop with new
  state `new_state`. The response must be sent with `reply/2`.

  Hibernating is also supported as an atom to be returned from either
  `:reply` and `:noreply` tuples.

  Returning `{:stop, reason, reply, new_state}` stops the loop and `terminate/2`
  is called with reason `reason` and state `new_state`. Then the `reply` is sent
  as the response to the call and the process exits with reason `reason`.

  Returning `{:stop, reason, new_state}` is similar to
  `{:stop, reason, reply, new_state}` except that no reply is sent to the caller.

  If this callback is not implemented, the default implementation by
  `use GenStage` will return `{:stop, {:bad_call, request}, state}`.
  """
  @callback handle_call(request :: term, from :: GenServer.from, state :: term) ::
    {:reply, reply, [event], new_state} |
    {:reply, reply, [event], new_state, :hibernate} |
    {:noreply, [event], new_state} |
    {:noreply, [event], new_state, :hibernate} |
    {:stop, reason, reply, new_state} |
    {:stop, reason, new_state} when reply: term, new_state: term, reason: term, event: term

  @doc """
  Invoked to handle asynchronous `cast/2` messages.

  `request` is the request message sent by a `cast/2` and `state` is the current
  state of the `GenStage`.

  Returning `{:noreply, [event], new_state}` dispatches the events and continues
  the loop with new state `new_state`.

  Returning `{:noreply, [event], new_state, :hibernate}` is similar to
  `{:noreply, new_state}` except the process is hibernated before continuing the
  loop. See the return values for `c:GenServer.handle_call/3` for more information
  on hibernation.

  Returning `{:stop, reason, new_state}` stops the loop and `terminate/2` is
  called with the reason `reason` and state `new_state`. The process exits with
  reason `reason`.

  If this callback is not implemented, the default implementation by
  `use GenStage` will return `{:stop, {:bad_cast, request}, state}`.
  """
  @callback handle_cast(request :: term, state :: term) ::
    {:noreply, [event], new_state} |
    {:noreply, [event], new_state, :hibernate} |
    {:stop, reason :: term, new_state} when new_state: term, event: term

  @doc """
  Invoked to handle all other messages.

  `message` is the message and `state` is the current state of the `GenStage`. When
  a timeout occurs the message is `:timeout`.

  If this callback is not implemented, the default implementation by
  `use GenStage` will return `{:noreply, [], state}`.

  Return values are the same as `c:handle_cast/2`.
  """
  @callback handle_info(message :: term, state :: term) ::
    {:noreply, [event], new_state} |
    {:noreply, [event], new_state, :hibernate} |
    {:stop, reason :: term, new_state} when new_state: term, event: term

  @doc """
  The same as `c:GenServer.terminate/2`.
  """
  @callback terminate(reason, state :: term) ::
    term when reason: :normal | :shutdown | {:shutdown, term} | term

  @doc """
  The same as `c:GenServer.code_change/3`.
  """
  @callback code_change(old_vsn, state :: term, extra :: term) ::
    {:ok, new_state :: term} |
