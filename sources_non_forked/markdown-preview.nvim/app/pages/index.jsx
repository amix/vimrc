import React from 'react'
import Head from 'next/head'
import io from 'socket.io-client'
import MarkdownIt from 'markdown-it'
import hljs from 'highlight.js'
import emoji from 'markdown-it-emoji'
import taskLists from 'markdown-it-task-lists'
import footnote from 'markdown-it-footnote'
import markdownItAnchor from 'markdown-it-anchor'
import markdownItToc from 'markdown-it-toc-done-right'
import markdownDeflist from 'markdown-it-deflist'

import mk from './katex'
import chart from './chart'
import mkitMermaid from './mermaid'
import linenumbers from './linenumbers'
import image from './image'
import diagram, { renderDiagram } from './diagram'
import flowchart, { renderFlowchart } from './flowchart'
import dot, { renderDot } from './dot'
import blockUml from './blockPlantuml'
import codeUml from './plantuml'
import scrollToLine from './scroll'
import { meta } from './meta';
import markdownImSize from './markdown-it-imsize'
import { escape} from './utils';

const anchorSymbol = '<svg class="octicon octicon-link" viewBox="0 0 16 16" version="1.1" width="16" height="16" aria-hidden="true"><path fill-rule="evenodd" d="M4 9h1v1H4c-1.5 0-3-1.69-3-3.5S2.55 3 4 3h4c1.45 0 3 1.69 3 3.5 0 1.41-.91 2.72-2 3.25V8.59c.58-.45 1-1.27 1-2.09C10 5.22 8.98 4 8 4H4c-.98 0-2 1.22-2 2.5S3 9 4 9zm9-3h-1v1h1c1 0 2 1.22 2 2.5S13.98 12 13 12H9c-.98 0-2-1.22-2-2.5 0-.83.42-1.64 1-2.09V6.25c-1.09.53-2 1.84-2 3.25C6 11.31 7.55 13 9 13h4c1.45 0 3-1.69 3-3.5S14.5 6 13 6z"></path></svg>'

const DEFAULT_OPTIONS = {
  mkit: {
    // Enable HTML tags in source
    html: true,
    // Use '/' to close single tags (<br />).
    // This is only for full CommonMark compatibility.
    xhtmlOut: true,
    // Convert '\n' in paragraphs into <br>
    breaks: false,
    // CSS language prefix for fenced blocks. Can be
    // useful for external highlighters.
    langPrefix: 'language-',
    // Autoconvert URL-like text to links
    linkify: true,
    // Enable some language-neutral replacement + quotes beautification
    typographer: true,
    // Double + single quotes replacement pairs, when typographer enabled,
    // and smartquotes on. Could be either a String or an Array.
    //
    // For example, you can use '«»„“' for Russian, '„“‚‘' for German,
    // and ['«\xA0', '\xA0»', '‹\xA0', '\xA0›'] for French (including nbsp).
    quotes: '“”‘’',
    // Highlighter function. Should return escaped HTML,
    // or '' if the source string is not changed and should be escaped externally.
    // If result starts with <pre... internal wrapper is skipped.
    highlight: function (str, lang) {
      if (lang && hljs.getLanguage(lang)) {
        try {
          return `<pre class="hljs"><code>${
            hljs.highlight(lang, str, true).value
          }</code></pre>`;
        } catch (__) {}
      }

      return `<pre class="hljs"><code>${escape(str)}</code></pre>`;
    },
  },
  katex: {
    'throwOnError': false,
    'errorColor': ' #cc0000'
  },
  uml: {},
  toc: {
    listType: 'ul'
  }
}

export default class PreviewPage extends React.Component {
  constructor(props) {
    super(props)

    this.preContent = ''
    this.timer = undefined

    this.state = {
      name: '',
      cursor: '',
      content: '',
      pageTitle: '',
      theme: '',
      themeModeIsVisible: false,
      contentEditable: false,
      disableFilename: 1
    }
    this.showThemeButton = this.showThemeButton.bind(this)
    this.hideThemeButton = this.hideThemeButton.bind(this)
    this.handleThemeChange = this.handleThemeChange.bind(this)
  }

  handleThemeChange() {
    this.setState((state) => ({
      theme: state.theme === 'light' ? 'dark' : 'light',
    }))
  }

  showThemeButton() {
    this.setState({ themeModeIsVisible: true })
  }

  hideThemeButton() {
    this.setState({ themeModeIsVisible: false })
  }


  componentDidMount() {
    const socket = io({
      query: {
        bufnr: window.location.pathname.split('/')[2]
      }
    })

    window.socket = socket

    socket.on('connect', this.onConnect.bind(this))

    socket.on('disconnect', this.onDisconnect.bind(this))

    socket.on('close', this.onClose.bind(this))

    socket.on('refresh_content', this.onRefreshContent.bind(this))

    socket.on('close_page', this.onClose.bind(this))
  }

  onConnect() {
    console.log('connect success')
  }

  onDisconnect() {
    console.log('disconnect')
  }

  onClose() {
    console.log('close')
    window.close()
  }

  onRefreshContent({
    options = {},
    isActive,
    winline,
    winheight,
    cursor,
    pageTitle = '',
    theme,
    name = '',
    content
  }) {
    if (!this.md) {
      const {
        mkit = {},
        katex = {},
        uml = {},
        hide_yaml_meta: hideYamlMeta = 1,
        sequence_diagrams: sequenceDiagrams = {},
        flowchart_diagrams: flowchartDiagrams = {},
        toc = {}
      } = options
      // markdown-it
      this.md = new MarkdownIt({
        ...DEFAULT_OPTIONS.mkit,
        ...mkit
      })
      if (hideYamlMeta === 1) {
        this.md.use(meta([['---', '\\.\\.\\.'], ['---', '\\.\\.\\.']]))
      }
      // katex
      this.md
        .use(mk, {
          ...DEFAULT_OPTIONS.katex,
          ...katex
        })
        .use(blockUml, {
          ...DEFAULT_OPTIONS.uml,
          ...uml
        })
        .use(codeUml, {
          ...DEFAULT_OPTIONS.uml,
          ...uml
        })
        .use(emoji)
        .use(taskLists)
        .use(markdownDeflist)
        .use(footnote)
        .use(image)
        .use(markdownImSize)
        .use(linenumbers)
        .use(mkitMermaid)
        .use(chart.chartPlugin)
        .use(diagram, {
          ...sequenceDiagrams
        })
        .use(flowchart, flowchartDiagrams)
        .use(dot)
        .use(markdownItAnchor, {
          permalink: true,
          permalinkBefore: true,
          permalinkSymbol: anchorSymbol,
          permalinkClass: 'anchor'
        })
        .use(markdownItToc, {
          ...DEFAULT_OPTIONS.toc,
          ...toc
        })
    }

    // Theme already applied
    if (this.state.theme) {
      theme = this.state.theme
    }
    // Define the theme according to the preferences of the system
    else if (!theme || !['light', 'dark'].includes(theme)) {
      if (
        window.matchMedia &&
        window.matchMedia('(prefers-color-scheme: dark)').matches
      ) {
        theme = 'dark'
      }
    }

    const newContent = content.join('\n')
    const refreshContent = this.preContent !== newContent
    this.preContent = newContent

    const refreshScroll = () => {
      if (isActive && !options.disable_sync_scroll) {
        scrollToLine[options.sync_scroll_type || 'middle']({
          cursor: cursor[1],
          winline,
          winheight,
          len: content.length
        })
      }
    }

    const refreshRender = () => {
      this.setState({
        cursor,
        name: ((name) => {
          let tokens = name.split(/\\|\//).pop().split('.');
          return tokens.length > 1 ? tokens.slice(0, -1).join('.') : tokens[0];
        })(name),
        ...(
          refreshContent
          ? { content: this.md.render(newContent) }
          : {}
        ),
        pageTitle,
        theme,
        contentEditable: options.content_editable,
        disableFilename: options.disable_filename
      }, () => {
        if (refreshContent) {
          try {
            // eslint-disable-next-line
            mermaid.initialize(options.maid || {})
            // eslint-disable-next-line
            mermaid.init(undefined, document.querySelectorAll('.mermaid'))
          } catch (e) { }

          chart.render()
          renderDiagram()
          renderFlowchart()
          renderDot()
        }
        refreshScroll()
      })
    }

    if (!this.preContent) {
      refreshRender()
    } else {
      if (!refreshContent) {
        refreshScroll()
      } else {
        if (this.timer) {
          clearTimeout(this.timer)
        }
        this.timer = setTimeout(() => {
          refreshRender()
        }, 16);
      }
    }
  }

  render() {
    const {
      theme,
      content,
      name,
      pageTitle,
      themeModeIsVisible,
      contentEditable,
      disableFilename,
    } = this.state

    return (
      <React.Fragment>
        <Head>
          <title>{(pageTitle || '').replace(/\$\{name\}/, name)}</title>
          <link rel="shortcut icon" type="image/ico" href="/_static/favicon.ico" />
          <link rel="stylesheet" href="/_static/page.css" />
          <link rel="stylesheet" href="/_static/markdown.css" />
          <link rel="stylesheet" href="/_static/highlight.css" />
          <link rel="stylesheet" href="/_static/katex@0.15.3.css" />
          <link rel="stylesheet" href="/_static/sequence-diagram-min.css" />
          <script type="text/javascript" src="/_static/underscore-min.js"></script>
          <script type="text/javascript" src="/_static/webfont.js"></script>
          <script type="text/javascript" src="/_static/snap.svg.min.js"></script>
          <script type="text/javascript" src="/_static/tweenlite.min.js"></script>
          <script type="text/javascript" src="/_static/mermaid.min.js"></script>
          <script type="text/javascript" src="/_static/sequence-diagram-min.js"></script>
          <script type="text/javascript" src="/_static/katex@0.15.3.js"></script>
          <script type="text/javascript" src="/_static/mhchem.min.js"></script>
          <script type="text/javascript" src="/_static/raphael@2.3.0.min.js"></script>
          <script type="text/javascript" src="/_static/flowchart@1.13.0.min.js"></script>
          <script type="text/javascript" src="/_static/viz.js"></script>
          <script type="text/javascript" src="/_static/full.render.js"></script>
        </Head>
        <main data-theme={this.state.theme}>
          <div id="page-ctn" contentEditable={contentEditable ? 'true' : 'false'}>
            { disableFilename == 0 &&
              <header
                id="page-header"
                onMouseEnter={this.showThemeButton}
                onMouseLeave={this.hideThemeButton}
              >
                <h3>
                  <svg
                    viewBox="0 0 16 16"
                    version="1.1"
                    width="16"
                    height="16"
                    aria-hidden="true"
                  >
                    <path
                      fill-rule="evenodd"
                      d="M3 5h4v1H3V5zm0 3h4V7H3v1zm0 2h4V9H3v1zm11-5h-4v1h4V5zm0 2h-4v1h4V7zm0 2h-4v1h4V9zm2-6v9c0 .55-.45 1-1 1H9.5l-1 1-1-1H2c-.55 0-1-.45-1-1V3c0-.55.45-1 1-1h5.5l1 1 1-1H15c.55 0 1 .45 1 1zm-8 .5L7.5 3H2v9h6V3.5zm7-.5H9.5l-.5.5V12h6V3z"
                    >
                    </path>
                  </svg>
                  {name}
                </h3>
                {themeModeIsVisible && (
                  <label id="toggle-theme" for="theme">
                    <input
                      id="theme"
                      type="checkbox"
                      checked={theme === "dark"}
                      onChange={this.handleThemeChange}
                    />
                    <span>Dark Mode</span>
                  </label>
               )}
              </header>
            }
            <section
              className="markdown-body"
              dangerouslySetInnerHTML={{
                __html: content
              }}
            />
          </div>
        </main>
      </React.Fragment>
    )
  }
}
