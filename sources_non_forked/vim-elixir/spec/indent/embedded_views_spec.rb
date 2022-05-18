# frozen_string_literal: true

require 'spec_helper'

describe 'Indenting embedded views' do
  i <<~EOF
    def render(assigns) do
      ~L"""
      <div>
        Some content
      </div>
      """
    end
  EOF

  i <<~EOF
    def render(assigns) do
      ~H"""
      <div class="theres a-/ in the class names from tailwind">
        <div class="some more classes">
          This is immediately nested
          <div>
            <input type="number" value="2" />
            There's a self-closing tag
          </div>
        </div>
      </div>
      """
    end
  EOF

  i <<~EOF
    def render(assigns) do
      ~L"""
      <div id="123456">
        Some content
      </div>
      """
    end
  EOF

  i <<~EOF
    def render(assigns) do
      ~L"""
      <div
        id="123456"
      >
        Some content
      </div>
      """
    end
  EOF

  i <<~EOF
    def render(assigns) do
      ~L"""
      <div />
      <p>Some paragraph</p>
      """
    end
  EOF

  i <<~EOF
    def render(assigns) do
      ~L"""
      <div>
        it
        <div>
          keeps
          <div>
            nesting
          </div>
        </div>
      </div>
      """
    end
  EOF

  i <<~EOF
    def render(assgins) do
      ~L"""
      <div>
        <%= for i <- iter do %>
          <div><%= i %></div>
        <% end %>
      </div>
      """
    end
  EOF

  i <<~EOF
    def render(assigns) do
      ~L"""
      <%= live_component @socket,
        Component,
        id: "<%= @id %>",
        user: @user do
      %>

        <main>
          <header>
            <h1>Some Header</h1>
          </header>
          <section>
            <h1>Some Section</h1>
            <p>
              I'm some text
            </p>
          </section>
        </main>

      <% end %>
      """
    end
  EOF

  i <<~EOF
    def render(assigns) do
      ~L"""
      <%= render_component,
        @socket,
        Component do %>

        <p>Multi-line opening eex tag that takes a block</p>
      <% end %>
      """
    end
  EOF

  i <<~EOF
    def render(assigns) do
      ~L"""
      <div>
        <%= render_component,
          @socket,
          Component %>
      </div>

      <%= render_component,
        @socket,
        Component %>
      <p>Multi-line single eex tag</p>
      """
    end
  EOF

  i <<~EOF
    def render(assigns) do
      ~H"""
      <Component
        foo={{
          foo: [
            'one',
            'two',
            'three'
          ],
          bar: %{
            "foo" => "bar"
          }
        }}
      />
      """
    end
  EOF

  i <<~EOF
    def render(assigns) do
      ~L"""
      <%= live_component @socket,
        Component,
        id: "<%= @id %>",
        team: @team do
      %>

        <div>
          <div>
            <div>
              A deeply nested tree
              <div>
                with trailing whitespace

              </div>
            </div>
          </div>
        </div>

        <div id="id-ends-with-greater-than->"
          propWithEexTag="<%= @id %>"
          anotherProp="foo"
        />

        <%= for i <- iter do %>
          <div><%= i %></div>
        <% end %>

        <div
          opts={{
            opt1: "optA",
            opt2: "optB"
          }}
          id="hi"
          bye="hi" />

        <ul>
          <li :for={{ item <- @items }}>
            {{ item }}
          </li>
        </ul>

        <div id="hi">
          Hi <p>hi</p>
          I'm ok, ok?
          <div>
            hi there!
          </div>
          <div>
            <div>
              <p>hi</p>
              <hr />
            </div>
          </div>
        </div>

        <Some.Surface.Component />

        <Another
          prop="prop"
          prop2="prop2"
        >
          <div>content</div>
        </Another>

        <div foo />

        <div>hi</div>

        <div>
          <div>
            content
          </div>
          <div />
          <div>
            content in new div after a self-closing div
          </div>
        </div>

        <p
          id="<%= @id %>"
          class="multi-line opening single letter p tag"
        >
          <%= @solo.eex_tag %>
          <Nested
            prop="nested"
          >
            content
          </Nested>
        </p>

      <% end %>
      """
    end
  EOF
end
