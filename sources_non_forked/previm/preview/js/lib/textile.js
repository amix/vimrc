/***
 * Textile parser for JavaScript
 *
 * Copyright (c) 2012 Borgar Þorsteinsson (MIT License).
 *
 */
/*jshint
  laxcomma:true
  laxbreak:true
  eqnull:true
  loopfunc:true
  sub:true
*/
;(function(){
"use strict";

  /***
   * Regular Expression helper methods
   * 
   * This provides the `re` object, which contains several helper
   * methods for working with big regular expressions (soup).
   *
   */
  var re = {
    _cache: {}
  , pattern: {
      'punct': "[!-/:-@\\[\\\\\\]-`{-~]"
    , 'space': '\\s'
    }
  , escape: function ( src ) {
      return src.replace( /[\-\[\]\{\}\(\)\*\+\?\.\,\\\^\$\|\#\s]/g, "\\$&" );
    }
  , collapse: function ( src ) {
      return src.replace( /(?:#.*?(?:\n|$))/g, '' )
                .replace( /\s+/g, '' )
                ;
    }
  , expand_patterns: function ( src ) {
      // TODO: provide escape for patterns: \[:pattern:] ?
      return src.replace( /\[\:\s*(\w+)\s*\:\]/g, function ( m, k ) {
          return ( k in re.pattern )
              ? re.expand_patterns( re.pattern[ k ] )
              : k
              ;
        })
        ;
    }
  , isRegExp: function ( r ) {
      return Object.prototype.toString.call( r ) === "[object RegExp]";
    }
  , compile: function ( src, flags ) {
      if ( re.isRegExp( src ) ) {
        if ( arguments.length === 1 ) { // no flags arg provided, use the RegExp one
          flags = ( src.global     ? 'g' : '' ) +
                  ( src.ignoreCase ? 'i' : '' ) +
                  ( src.multiline  ? 'm' : '' );
        }
        src = src.source;
      }
      // don't do the same thing twice
      var ckey = src + ( flags || '' );
      if ( ckey in re._cache ) { return re._cache[ ckey ]; }
      // allow classes
      var rx = re.expand_patterns( src );
      // allow verbose expressions
      if ( flags && /x/.test( flags ) ) {
        rx = re.collapse( rx );
      }
      // allow dotall expressions
      if ( flags && /s/.test( flags ) ) {
        rx = rx.replace( /([^\\])\./g, '$1[^\\0]' );
      }
      // TODO: test if MSIE and add replace \s with [\s\u00a0] if it is?
      // clean flags and output new regexp
      flags = ( flags || '' ).replace( /[^gim]/g, '' );
      return ( re._cache[ ckey ] = new RegExp( rx, flags ) );
    }
  };




  /***
   * JSONML helper methods - http://www.jsonml.org/
   * 
   * This provides the `JSONML` object, which contains helper
   * methods for rendering JSONML to HTML.
   *
   * Note that the tag ! is taken to mean comment, this is however
   * not specified in the JSONML spec.
   *
   */
  var JSONML = {
    escape: function ( text, esc_quotes ) {
      return text.replace( /&(?!(#\d{2,}|#x[\da-fA-F]{2,}|[a-zA-Z][a-zA-Z1-4]{1,6});)/g, "&amp;" )
                 .replace( /</g, "&lt;" )
                 .replace( />/g, "&gt;" )
                 .replace( /"/g, esc_quotes ? "&quot;" : '"' )
                 .replace( /'/g, esc_quotes ? "&#39;"  : "'" )
                 ;
    }
  , toHTML: function ( jsonml ) {

      jsonml = jsonml.concat();

      // basic case
      if ( typeof jsonml === "string" ) {
        return JSONML.escape( jsonml );
      }

      var tag = jsonml.shift()
        , attributes = {}
        , content = []
        , tag_attrs = ""
        , a
        ;
      if ( jsonml.length && typeof jsonml[ 0 ] === "object" && !_isArray( jsonml[ 0 ] ) ) {
        attributes = jsonml.shift();
      }

      while ( jsonml.length ) {
        content.push( JSONML.toHTML( jsonml.shift() ) );
      }

      for ( a in attributes ) {
        tag_attrs += ( attributes[ a ] == null )
                ? " " + a
                : " " + a + '="' + JSONML.escape( attributes[ a ], true ) + '"'
                ;
      }

      // be careful about adding whitespace here for inline elements
      if ( tag == "!" ) {
        return "<!--" + content.join( "" ) + "-->";
      }
      else if ( tag === "img" || tag === "br" || tag === "hr" || tag === "input" ) {
        return "<" + tag + tag_attrs + " />";
      }
      else {
        return "<" + tag + tag_attrs + ">" + content.join( "" ) + "</" + tag + ">";
      }
    }
  };


  // merge object b properties into obect a
  function merge ( a, b ) {
    for ( var k in b ) {
      a[ k ] = b[ k ];
    }
    return a;
  }


  var _isArray = Array.isArray || function ( a ) { return Object.prototype.toString.call(a) === '[object Array]'; };

  /* expressions */
  re.pattern[ 'blocks'    ] = '(?:b[qc]|div|notextile|pre|h[1-6]|fn\\d+|p|###)';
  re.pattern[ 'pba_class' ] = '\\([^\\)]+\\)';
  re.pattern[ 'pba_style' ] = '\\{[^\\}]+\\}';
  re.pattern[ 'pba_lang'  ] = '\\[[^\\[\\]]+\\]';
  re.pattern[ 'pba_align' ] = '(?:<>|<|>|=)';
  re.pattern[ 'pba_pad'   ] = '[\\(\\)]+';
  re.pattern[ 'pba_attr'  ] = '(?:[:pba_class:]|[:pba_style:]|[:pba_lang:]|[:pba_align:]|[:pba_pad:])*';
  re.pattern[ 'url_punct' ] = '[.,«»″‹›!?]';
  re.pattern[ 'html_id'   ] = '[a-zA-Z][a-zA-Z\\d:]*';
  re.pattern[ 'html_attr' ] = '(?:"[^"]+"|\'[^\']+\'|[^>\\s]+)';
  re.pattern[ 'tx_urlch'  ] = '[\\w"$\\-_.+!*\'(),";\\/?:@=&%#{}|\\\\^~\\[\\]`]';
  re.pattern[ 'tx_cite'   ] = ':((?:[^\\s()]|\\([^\\s()]+\\)|[()])+?)(?=[!-\\.:-@\\[\\\\\\]-`{-~]+(?:$|\\s)|$|\\s)';
  re.pattern[ 'ucaps'     ] = "A-Z"+
                              // Latin extended À-Þ
                              "\u00c0-\u00d6\u00d8-\u00de"+ 
                              // Latin caps with embelishments and ligatures...
                              "\u0100\u0102\u0104\u0106\u0108\u010a\u010c\u010e\u0110\u0112\u0114\u0116\u0118\u011a\u011c\u011e\u0120\u0122\u0124\u0126\u0128\u012a\u012c\u012e\u0130\u0132\u0134\u0136\u0139\u013b\u013d\u013f"+
                              "\u0141\u0143\u0145\u0147\u014a\u014c\u014e\u0150\u0152\u0154\u0156\u0158\u015a\u015c\u015e\u0160\u0162\u0164\u0166\u0168\u016a\u016c\u016e\u0170\u0172\u0174\u0176\u0178\u0179\u017b\u017d"+
                              "\u0181\u0182\u0184\u0186\u0187\u0189-\u018b\u018e-\u0191\u0193\u0194\u0196-\u0198\u019c\u019d\u019f\u01a0\u01a2\u01a4\u01a6\u01a7\u01a9\u01ac\u01ae\u01af\u01b1-\u01b3\u01b5\u01b7\u01b8\u01bc"+
                              "\u01c4\u01c7\u01ca\u01cd\u01cf\u01d1\u01d3\u01d5\u01d7\u01d9\u01db\u01de\u01e0\u01e2\u01e4\u01e6\u01e8\u01ea\u01ec\u01ee\u01f1\u01f4\u01f6-\u01f8\u01fa\u01fc\u01fe"+
                              "\u0200\u0202\u0204\u0206\u0208\u020a\u020c\u020e\u0210\u0212\u0214\u0216\u0218\u021a\u021c\u021e\u0220\u0222\u0224\u0226\u0228\u022a\u022c\u022e\u0230\u0232\u023a\u023b\u023d\u023e"+
                              "\u0241\u0243-\u0246\u0248\u024a\u024c\u024e"+
                              "\u1e00\u1e02\u1e04\u1e06\u1e08\u1e0a\u1e0c\u1e0e\u1e10\u1e12\u1e14\u1e16\u1e18\u1e1a\u1e1c\u1e1e\u1e20\u1e22\u1e24\u1e26\u1e28\u1e2a\u1e2c\u1e2e\u1e30\u1e32\u1e34\u1e36\u1e38\u1e3a\u1e3c\u1e3e\u1e40"+
                              "\u1e42\u1e44\u1e46\u1e48\u1e4a\u1e4c\u1e4e\u1e50\u1e52\u1e54\u1e56\u1e58\u1e5a\u1e5c\u1e5e\u1e60\u1e62\u1e64\u1e66\u1e68\u1e6a\u1e6c\u1e6e\u1e70\u1e72\u1e74\u1e76\u1e78\u1e7a\u1e7c\u1e7e"+
                              "\u1e80\u1e82\u1e84\u1e86\u1e88\u1e8a\u1e8c\u1e8e\u1e90\u1e92\u1e94\u1e9e\u1ea0\u1ea2\u1ea4\u1ea6\u1ea8\u1eaa\u1eac\u1eae\u1eb0\u1eb2\u1eb4\u1eb6\u1eb8\u1eba\u1ebc\u1ebe"+
                              "\u1ec0\u1ec2\u1ec4\u1ec6\u1ec8\u1eca\u1ecc\u1ece\u1ed0\u1ed2\u1ed4\u1ed6\u1ed8\u1eda\u1edc\u1ede\u1ee0\u1ee2\u1ee4\u1ee6\u1ee8\u1eea\u1eec\u1eee\u1ef0\u1ef2\u1ef4\u1ef6\u1ef8\u1efa\u1efc\u1efe"+
                              "\u2c60\u2c62-\u2c64\u2c67\u2c69\u2c6b\u2c6d-\u2c70\u2c72\u2c75\u2c7e\u2c7f"+
                              "\ua722\ua724\ua726\ua728\ua72a\ua72c\ua72e\ua732\ua734\ua736\ua738\ua73a\ua73c\ua73e"+
                              "\ua740\ua742\ua744\ua746\ua748\ua74a\ua74c\ua74e\ua750\ua752\ua754\ua756\ua758\ua75a\ua75c\ua75e\ua760\ua762\ua764\ua766\ua768\ua76a\ua76c\ua76e\ua779\ua77b\ua77d\ua77e"+
                              "\ua780\ua782\ua784\ua786\ua78b\ua78d\ua790\ua792\ua7a0\ua7a2\ua7a4\ua7a6\ua7a8\ua7aa";

  var re_block          = re.compile( /^([:blocks:])/ )
    , re_block_se       = re.compile( /^[:blocks:]$/ )
    , re_block_normal   = re.compile( /^(.*?)($|\n(?:\s*\n|$)+)/, 's' )
    , re_block_extended = re.compile( /^(.*?)($|\n+(?=[:blocks:][:pba_attr:]\.))/, 's' )
    , re_ruler          = /^(\-\-\-+|\*\*\*+|___+)(\n\s+|$)/
    , re_list           = re.compile( /^((?:[\t ]*[\#\*]+[:pba_attr:] .+?(?:\n|$))+)(\s*\n)?/ )
    , re_list_item      = /^([\#\*]+)(.+?)(\n|$)/
    , re_table          = re.compile( /^((?:table[:pba_attr:]\.\n)?(?:(?:[:pba_attr:]\.[^\n\S]*)?\|.*?\|[^\n\S]*(?:\n|$))+)([^\n\S]*\n)?/, 's' )
    , re_table_head     = /^table(_?)([^\n]+)\.\s?\n/
    , re_table_row      = re.compile( /^([:pba_attr:]\.[^\n\S]*)?\|(.*?)\|[^\n\S]*(\n|$)/, 's' )
    , re_fenced_phrase  = /^\[(__?|\*\*?|\?\?|[\-\+\^~@%])([^\n]+)\1\]/
    , re_phrase         = /^([\[\{]?)(__?|\*\*?|\?\?|[\-\+\^~@%])/
    , re_text           = re.compile( /^.+?(?=[\\<!\[_\*`]|\n|$)/, 's' )
    , re_image          = re.compile( /^!(?!\s)([:pba_attr:](?:\.[^\n\S]|\.(?:[^\.\/]))?)([^!\s]+?) ?(?:\(((?:[^\(\)]+|\([^\(\)]+\))+)\))?!(?::([^\s]+?(?=[!-\.:-@\[\\\]-`{-~](?:$|\s)|\s|$)))?/ )
    , re_image_fenced   = re.compile( /^\[!(?!\s)([:pba_attr:](?:\.[^\n\S]|\.(?:[^\.\/]))?)([^!\s]+?) ?(?:\(((?:[^\(\)]+|\([^\(\)]+\))+)\))?!(?::([^\s]+?(?=[!-\.:-@\[\\\]-`{-~](?:$|\s)|\s|$)))?\]/ )
    // NB: there is an exception in here to prevent matching "TM)"
    , re_caps           = re.compile( /^((?!TM\)|tm\))[[:ucaps:]](?:[[:ucaps:]\d]{1,}(?=\()|[[:ucaps:]\d]{2,}))(?:\((.*?)\))?(?=\W|$)/ )
    , re_link           = re.compile( /^"(?!\s)((?:[^\n"]|"(?![\s:])[^\n"]+"(?!:))+)"[:tx_cite:]/ )
    , re_link_fenced    = /^\["([^\n]+?)":((?:\[[a-z0-9]*\]|[^\]])+)\]/
    , re_link_ref       = re.compile( /^\[([^\]]+)\]((?:https?:\/\/|\/)\S+)(?:\s*\n|$)/ )
    , re_link_title     = /\s*\(((?:\([^\(\)]*\)|[^\(\)]+)+)\)$/
    , re_footnote_def   = /^fn\d+$/
    , re_footnote       = /^\[(\d+)\]/

    // HTML
    , re_html_tag_block = re.compile( /^\s*<([:html_id:](?::[a-zA-Z\d]+)*)((?:\s[^=\s\/]+(?:\s*=\s*[:html_attr:])?)+)?\s*(\/?)>(\n*)/ )
    , re_html_tag       = re.compile( /^<([:html_id:])((?:\s[^=\s\/]+(?:\s*=\s*[:html_attr:])?)+)?\s*(\/?)>(\n*)/ )
    , re_html_comment   = re.compile( /^<!--(.+?)-->/, 's' )
    , re_html_end_tag   = re.compile( /^<\/([:html_id:])([^>]*)>/ )
    , re_html_attr      = re.compile( /^\s*([^=\s]+)(?:\s*=\s*("[^"]+"|'[^']+'|[^>\s]+))?/ )
    , re_entity         = /&(#\d\d{2,}|#x[\da-fA-F]{2,}|[a-zA-Z][a-zA-Z1-4]{1,6});/

    // glyphs
    , re_dimsign        = /([\d\.,]+['"]? ?)x( ?)(?=[\d\.,]['"]?)/g
    , re_emdash         = /(^|[\s\w])--([\s\w]|$)/g
    , re_trademark      = /(\b ?|\s|^)(?:\((?:TM|tm)\)|\[(?:TM|tm)\])/g
    , re_registered     = /(\b ?|\s|^)(?:\(R\)|\[R\])/gi
    , re_copyright      = /(\b ?|\s|^)(?:\(C\)|\[C\])/gi
    , re_apostrophe     = /(\w)\'(\w)/g
    , re_double_prime   = re.compile( /(\d*[\.,]?\d+)"(?=\s|$|[:punct:])/g )
    , re_single_prime   = re.compile( /(\d*[\.,]?\d+)'(?=\s|$|[:punct:])/g )
    , re_closing_dquote = re.compile( /([^\s\[\(])"(?=$|\s|[:punct:])/g )
    , re_closing_squote = re.compile( /([^\s\[\(])'(?=$|\s|[:punct:])/g )

    // pba
    , re_pba_classid    = /^\(([^\(\)\n]+)\)/
    , re_pba_padding_l  = /^([\(]+)/
    , re_pba_padding_r  = /^([\)]+)/
    , re_pba_align_blk  = /^(<>|<|>|=)/
    , re_pba_align_img  = /^(<|>|=)/
    , re_pba_valign     = /^(~|\^|\-)/
    , re_pba_colspan    = /^\\(\d+)/
    , re_pba_rowspan    = /^\/(\d+)/
    , re_pba_styles     = /^\{([^\}]*)\}/
    , re_pba_css        = /^\s*([^:\s]+)\s*:\s*(.+)\s*$/
    , re_pba_lang       = /^\[([^\[\]]+)\]/
    ;

  var phrase_convert = {
    '*':  'strong'
  , '**': 'b'
  , '??': 'cite'
  , '_':  'em'
  , '__': 'i'
  , '-':  'del'
  , '%':  'span'
  , '+':  'ins'
  , '~':  'sub'
  , '^':  'sup'
  , '@':  'code'
  };

  // area, base, basefont, bgsound, br, col, command, embed, frame, hr, 
  // img, input, keygen, link, meta, param, source, track or wbr 
  var html_singletons = {
    'br': 1
  , 'hr': 1
  , 'img': 1
  , 'link': 1
  , 'meta': 1
  , 'wbr': 1
  , 'area': 1
  , 'param': 1
  , 'input': 1
  , 'option': 1
  , 'base': 1
  };

  var pba_align_lookup = {
    '<': 'left'
  , '=': 'center'
  , '>': 'right'
  , '<>': 'justify'
  };

  var pba_valign_lookup = {
    '~':'bottom'
  , '^':'top'
  , '-':'middle'
  };

  // HTML tags allowed in the document (root) level that trigger HTML parsing
  var allowed_blocktags = {
    'p': 0
  , 'hr': 0
  , 'ul': 1
  , 'ol': 0
  , 'li': 0
  , 'div': 1
  , 'pre': 0
  , 'object': 1
  , 'script': 0
  , 'noscript': 0
  , 'blockquote': 1
  , 'notextile': 1
  };


  function ribbon ( feed ) {
    var _slot = null
      , org = feed + ''
      , pos = 0
      ;
    return {
      save: function () {
        _slot = pos;
      }
    , load: function () {
        pos = _slot;
        feed = org.slice( pos );
      }
    , advance: function ( n ) {
        pos += ( typeof n === 'string' ) ? n.length : n;
        return ( feed = org.slice( pos ) );
      }
    , lookbehind: function ( nchars ) {
        nchars = nchars == null ? 1 : nchars;
        return org.slice( pos - nchars, pos );
      }
    , startsWith: function ( s ) {
        return feed.substring(0, s.length) === s;
      }
    , valueOf: function(){
        return feed;
      }
    , toString: function(){
        return feed;
      }
    };
  }


  function builder ( arr ) {
    var _arr = _isArray( arr ) ? arr : [];
    return {
      add: function ( node ) {
        if ( typeof node === 'string' &&
             typeof _arr[_arr.length - 1 ] === 'string' ) {
          // join if possible
          _arr[ _arr.length - 1 ] += node;
        }
        else if ( _isArray( node ) ) {
          var f = node.filter(function(s){ return s !== undefined; });
          _arr.push( f );
        }
        else if ( node ) {
          _arr.push( node );
        }
        return this;
      }
    , merge: function ( s ) {
        for (var i=0,l=s.length; i<l; i++) {
          this.add( s[i] );
        }
        return this;
      }
    , linebreak: function () {
        if ( _arr.length ) {
          this.add( '\n' );
        }
      }
    , get: function () {
        return _arr;
      }
    };
  }


  function copy_pba ( s, blacklist ) {
    if ( !s ) { return undefined; }
    var k, d = {};
    for ( k in s ) {
      if ( k in s && ( !blacklist || !(k in blacklist) ) ) {
        d[ k ] = s[ k ];
      }
    }
    return d;
  }


  function parse_html_attr ( attr ) {
    // parse ATTR and add to element
    var _attr = {}
      , m
      , val
      ;
    while ( (m = re_html_attr.exec( attr )) ) {
      _attr[ m[1] ] = ( typeof m[2] === 'string' )
          ? m[2].replace( /^(["'])(.*)\1$/, '$2' )
          : null
          ;
      attr = attr.slice( m[0].length );
    }
    return _attr;
  }


  // This "indesciminately" parses HTML text into a list of JSON-ML element
  // No steps are taken however to prevent things like <table><p><td> - user can still create nonsensical but "well-formed" markup
  function parse_html ( src, whitelist_tags ) {
    var org = src + ''
      , list = []
      , root = list
      , _stack = []
      , m
      , oktag = whitelist_tags ? function ( tag ) { return tag in whitelist_tags; } : function () { return true; }
      , tag
      ;
    src = (typeof src === 'string') ? ribbon( src ) : src;
    // loop
    do {

      if ( (m = re_html_comment.exec( src )) && oktag('!') ) {
        src.advance( m[0] );
        list.push( [ '!', m[1] ] );
      }

      // end tag
      else if ( (m = re_html_end_tag.exec( src )) && oktag(m[1]) ) {
        tag = m[1];
        var junk = m[2];
        if ( _stack.length ) {
          for (var i=_stack.length-1; i>=0; i--) {
            var head = _stack[i];
            if ( head[0] === tag ) {
              _stack.splice( i );
              list = _stack[ _stack.length - 1 ] || root;
              break;
            }
          }
        }
        src.advance( m[0] );
      }

      // open/void tag
      else if ( (m = re_html_tag.exec( src )) && oktag(m[1]) ) {
        src.advance( m[0] );
        tag = m[1];
        var single = m[3] || m[1] in html_singletons
          , tail = m[4]
          , element = [ tag ]
          ;

        // attributes
        if ( m[2] ) { element.push( parse_html_attr( m[2] ) ); }

        // tag
        if ( single ) { // single tag
          // let us add the element and continue our quest...
          list.push( element );
          if ( tail ) { list.push( tail ); }
        }
        else { // open tag
          if ( tail ) { element.push( tail ); }

          // TODO: some things auto close other things: <td>, <li>, <p>, <table>
          // if ( tag === 'p' && _stack.length ) {
          //   var seek = /^(p)$/;
          //   for (var i=_stack.length-1; i>=0; i--) {
          //     var head = _stack[i];
          //     if ( seek.test( head[0] ) /* === tag */ ) {
          //       //src.advance( m[0] );
          //       _stack.splice( i );
          //       list = _stack[i] || root;
          //     }
          //   }
          // }

          // TODO: some elements can move parser into "text" mode
          // style, xmp, iframe, noembed, noframe, textarea, title, script, noscript, plaintext
          //if ( /^(script)$/.test( tag ) ) { }

          _stack.push( element );
          list.push( element );
          list = element;

        }
      }
      else {

        // no match, move by all "uninteresting" chars
        m = /([^<]+|[^\0])/.exec( src );
        if ( m ) {
          list.push( m[0] );
        }
        src.advance( m ? m[0].length || 1 : 1 );

      }

    }
    while ( src.valueOf() );
    return root;
  }

  /* attribute parser */

  function parse_attr ( input, element, end_token ) {
    /*
    The attr bit causes massive problems for span elements when parens are used.
    Parens are a total mess and, unsurprisingly, causes trip ups:

     RC: `_{display:block}(span) span (span)_` -> `<em style="display:block;" class="span">(span) span (span)</em>`
     PHP: `_{display:block}(span) span (span)_` -> `<em style="display:block;">(span) span (span)</em>`

    PHP and RC seem to mostly solve this by not parsing a final attr parens on spans if the
    following character is a non-space. I've duplicated that: Class/ID is not matched on spans
    if it is followed by `end_token` or <space>.
    */
    input += '';
    if ( !input || element === 'notextile' ) { return undefined; }

    var m
      , st = {}
      , o = { 'style': st }
      , remaining = input
      , is_block  = element === 'table' || element === 'td' || re_block_se.test( element ) // "in" test would be better but what about fn#.?
      , is_img    = element === 'img'
      , is_phrase = !is_block && !is_img && element !== 'a'
      , re_pba_align = ( is_img ) ? re_pba_align_img : re_pba_align_blk
      ;

    do {

      if ( (m = re_pba_styles.exec( remaining )) ) {
        m[1].split(';').forEach(function(p){
          var d = p.match( re_pba_css );
          if ( d ) { st[ d[1] ] = d[2]; }
        });
        remaining = remaining.slice( m[0].length );
        continue;
      }

      if ( (m = re_pba_lang.exec( remaining )) ) {
        o['lang'] = m[1];
        remaining = remaining.slice( m[0].length );
        continue;
      }

      if ( (m = re_pba_classid.exec( remaining )) ) {
        var rm = remaining.slice( m[0].length );
        if (
            ( !rm && is_phrase ) ||
            ( end_token && (rm[0] === ' ' || end_token === rm.slice(0,end_token.length)) )
           ) {
          m = null;
          continue;
        }
        var bits = m[1].split( '#' );
        if ( bits[0] ) { o['class'] = bits[0]; }
        if ( bits[1] ) { o['id']    = bits[1]; }
        remaining = rm;
        continue;
      }

      if ( is_block ) {
        if ( (m = re_pba_padding_l.exec( remaining )) ) {
          st[ "padding-left" ] = ( m[1].length ) + "em";
          remaining = remaining.slice( m[0].length );
          continue;
        }
        if ( (m = re_pba_padding_r.exec( remaining )) ) {
          st[ "padding-right" ] = ( m[1].length ) + "em";
          remaining = remaining.slice( m[0].length );
          continue;
        }
      }

      // only for blocks: 
      if ( is_img || is_block ) {
        if ( (m = re_pba_align.exec( remaining )) ) {
          var align = pba_align_lookup[ m[1] ];
          if ( is_img ) {
            o[ 'align' ] = align;
          }
          else {
            st[ 'text-align' ] = align;
          }
          remaining = remaining.slice( m[0].length );
          continue;
        }
      }

      // only for table cells
      if ( element === 'td' || element === 'tr' ) {
        if ( (m = re_pba_valign.exec( remaining )) ) {
          st[ "vertical-align" ] = pba_valign_lookup[ m[1] ];
          remaining = remaining.slice( m[0].length );
          continue;
        }
      }
      if ( element === 'td' ) {
        if ( (m = re_pba_colspan.exec( remaining )) ) {
          o[ "colspan" ] = m[1];
          remaining = remaining.slice( m[0].length );
          continue;
        }
        if ( (m = re_pba_rowspan.exec( remaining )) ) {
          o[ "rowspan" ] = m[1];
          remaining = remaining.slice( m[0].length );
          continue;
        }
      }

    }
    while ( m );

    // collapse styles
    var s = [];
    for ( var v in st ) { s.push( v + ':' + st[v] ); }
    if ( s.length ) { o.style = s.join(';'); } else { delete o.style; }

    return remaining == input
              ? undefined
              : [ input.length - remaining.length, o ]
              ;
  }



  /* glyph parser */

  function parse_glyphs ( src ) {
    if ( typeof src !== 'string' ) { return src; }
    // NB: order is important here ...
    return src
      // arrow
      .replace( /([^\-]|^)->/, '$1&#8594;' ) // arrow
      // dimensions
      .replace( re_dimsign, '$1&#215;$2' ) // dimension sign
      // ellipsis
      .replace( /([^.]?)\.{3}/g, '$1&#8230;' ) // ellipsis
      // dashes
      .replace( re_emdash, '$1&#8212;$2' ) // em dash
      .replace( /( )-( )/g, '$1&#8211;$2' ) // en dash
      // legal marks
      .replace( re_trademark, '$1&#8482;' )   // trademark
      .replace( re_registered, '$1&#174;'  )   // registered
      .replace( re_copyright, '$1&#169;'  )   // copyright
      // double quotes
      .replace( re_double_prime, '$1&#8243;' ) // double prime
      .replace( re_closing_dquote, '$1&#8221;' ) // double closing quote
      .replace( /"/g, '&#8220;' ) // double opening quote
      // single quotes
      .replace( re_single_prime, '$1&#8242;' )  // single prime
      .replace( re_apostrophe, '$1&#8217;$2' )    // I'm an apostrophe
      .replace( re_closing_squote, '$1&#8217;' )     // single closing quote
      .replace( /'/g, '&#8216;' )
      ;
  }


  /* list parser */

  function parse_list ( src, options ) {

    src = ribbon( src.replace( /(^|\n)[\t ]+/, '$1' ) );
    var pad = function ( n ) {
          var s = '\n';
          while ( n-- ) { s += '\t'; }
          return s;
        }
      , stack = []
      , m
      , s
      ;

    while ( (m = re_list_item.exec( src )) ) {

      var item = [ 'li' ]
        , pba = parse_attr( m[2], 'li' )
        ;
      if ( pba ) {
        m[2] = m[2].slice( pba[0] );
        pba = pba[1];
      }

      var dest_level = m[1].length
        , type = m[1].substr(-1) === '#' ? 'ol' : 'ul'
        , eqlev = stack.length === dest_level
        , new_li = null
        , lst
        , par
        , r
        ;
      // create nesting until we have correct level
      while ( stack.length < dest_level ) {
        lst = [ type, pad( stack.length + 1 ), (new_li = [ 'li' ]) ];
        par = stack[ stack.length - 1 ];
        if ( par ) {
          par.li.push( pad( stack.length ) );
          par.li.push( lst );
        }
        stack.push({ ul: lst, li: new_li });
      }
      // remove nesting until we have correct level
      while ( stack.length > dest_level ) {
        r = stack.pop();
        r.ul.push( pad( stack.length ) );
      }
      par = stack[ stack.length - 1 ];
      if ( !new_li ) {
        par.ul.push( pad( stack.length ), item );
        par.li = item;
      }
      if ( pba ) { par.li.push( pba ); }
      Array.prototype.push.apply( par.li, parse_inline( m[2].trim(), options ) );

      src.advance( m[0] );
    }

    while ( stack.length ) {
      s = stack.pop();
      s.ul.push( pad( stack.length ) );
    }

    return s.ul;
  }



  /* table parser */

  function parse_table ( src, options ) {
    src = ribbon( src.trim() );
    var table = [ 'table' ]
      , row
      , inner
      , pba
      , more
      , m
      ;

    if ( (m = re_table_head.exec( src )) ) {
      // parse and apply table attr
      src.advance( m[0] );
      pba = parse_attr( m[2], 'table' );
      if ( pba ) {
        table.push( pba[1] );
      }
    }

    while ( (m = re_table_row.exec( src )) ) {
      row = [ 'tr' ];

      if ( m[1] && (pba = parse_attr( m[1], 'tr' )) ) {
        // FIXME: requires "\.\s?" -- else what ?
        row.push( pba[1] );
      }

      table.push( '\n\t', row );
      inner = ribbon( m[2] );

      do {
        inner.save();

        // cell loop
        var th = inner.startsWith( '_' )
          , cell = [ th ? 'th' : 'td' ]
          ;
        if ( th ) {
          inner.advance( 1 );
        }

        pba = parse_attr( inner, 'td' );
        if ( pba ) {
          inner.advance( pba[0] );
          cell.push( pba[1] ); // FIXME: don't do this if next text fails
        }

        if ( pba || th ) {
          var d = /^\.\s*/.exec( inner );
          if ( d ) {
            inner.advance( d[0] );
          }
          else {
            cell = [ 'td' ];
            inner.load();
          }
        }

        var mx = /^(==.*?==|[^\|])*/.exec( inner );
        cell = cell.concat( parse_inline( mx[0], options ) );
        row.push( '\n\t\t', cell );
        more = inner.valueOf().charAt( mx[0].length ) === '|';
        inner.advance( mx[0].length + 1 );

      }
      while ( more );

      row.push( '\n\t' );

      src.advance( m[0] );
    }
    table.push( '\n' );
    return table;

  }


  /* inline parser */

  function parse_inline ( src, options ) {

    src = ribbon( src );
    var list = builder()
      , m
      , pba
      ;

    // loop
    do {
      src.save();

      // linebreak -- having this first keeps it from messing to much with other phrases
      if ( src.startsWith( '\n' ) ) {
        src.advance( 1 );

        if ( options.breaks ) {
          list.add( [ 'br' ] );
        }
        list.add( '\n' );
        continue;
      }

      // inline notextile
      if ( (m = /^==(.*?)==/.exec( src )) ) {
        src.advance( m[0] );
        list.add( m[1] );
        continue;
      }

      // lookbehind => /([\s>.,"'?!;:])$/
      var behind = src.lookbehind( 1 );
      var boundary = !behind || /^[\s>.,"'?!;:()]$/.test( behind );
      // FIXME: need to test right boundary for phrases as well
      if ( (m = re_phrase.exec( src )) && ( boundary || m[1] ) ) {
        src.advance( m[0] );
        var tok = m[2]
          , fence = m[1]
          , phrase_type = phrase_convert[ tok ]
          , code = phrase_type === 'code'
          ;
        if ( (pba = !code && parse_attr( src, phrase_type, tok )) ) {
          src.advance( pba[0] );
          pba = pba[1];
        }
        // FIXME: if we can't match the fence on the end, we should output fence-prefix as normal text
        // seek end
        var m_mid;
        var m_end;
        if ( fence === '[' ) {
          m_mid = '^(.*?)';
          m_end = '(?:])';
        }
        else if ( fence === '{' ) {
          m_mid = '^(.*?)';
          m_end = '(?:})';
        }
        else {
          var t1 = re.escape( tok.charAt(0) );
          m_mid = ( code )
                    ? '^(\\S+|\\S+.*?\\S)'
                    : '^([^\\s' + t1 + ']+|[^\\s' + t1 + '].*?\\S('+t1+'*))'
                    ;
          m_end = '(?=$|[\\s.,"\'!?;:()«»„“”‚‘’])';
        }
        var rx = re.compile( m_mid + '(' + re.escape( tok ) + ')' + m_end );
        if ( (m = rx.exec( src )) && m[1] ) {
          src.advance( m[0] );
          if ( code ) {
            list.add( [ phrase_type, m[1] ] );
          }
          else {
            list.add( [ phrase_type, pba ].concat( parse_inline( m[1], options ) ) );
          }
          continue;
        }
        // else 
        src.load();
      }

      // image
      if ( (m = re_image.exec( src )) || (m = re_image_fenced.exec( src )) ) {
        src.advance( m[0] );

        pba = m[1] && parse_attr( m[1], 'img' );
        var attr = pba ? pba[1] : { 'src':'' }
          , img = [ 'img', attr ]
          ;
        attr.src = m[2];
        attr.alt = m[3] ? ( attr.title = m[3] ) : '';

        if ( m[4] ) { // +cite causes image to be wraped with a link (or link_ref)?
          // TODO: support link_ref for image cite
          img = [ 'a', { 'href': m[4] }, img ];
        }
        list.add( img );
        continue;
      }

      // html comment
      if ( (m = re_html_comment.exec( src )) ) {
        src.advance( m[0] );
        list.add( [ '!', m[1] ] );
        continue;
      }
      // html tag
      // TODO: this seems to have a lot of overlap with block tags... DRY?
      if ( (m = re_html_tag.exec( src )) ) {
        src.advance( m[0] );
        var tag = m[1]
          , single = m[3] || m[1] in html_singletons
          , element = [ tag ]
          , tail = m[4]
          ;
        if ( m[2] ) {
          element.push( parse_html_attr( m[2] ) );
        }
        if ( single ) { // single tag
          list.add( element ).add( tail );
          continue;
        }
        else { // need terminator
          // gulp up the rest of this block... 
          var re_end_tag = re.compile( "^(.*?)(</" + tag + "\\s*>)", 's' );
          if ( (m = re_end_tag.exec( src )) ) {
            src.advance( m[0] );
            if ( tag === 'code' ) {
              element.push( tail, m[1] );
            }
            else if ( tag === 'notextile' ) {
              list.merge( parse_inline( m[1], options ) );
              continue;
            }
            else {
              element = element.concat( parse_inline( m[1], options ) );
            }
            list.add( element );
            continue;
          }
          // end tag is missing, treat tag as normal text...
        }
        src.load();
      }

      // footnote
      if ( (m = re_footnote.exec( src )) ) {
        src.advance( m[0] );
        list.add( [ 'sup', { 'class': 'footnote', 'id': 'fnr' + m[1] },
                    [ 'a', { href: '#fn' + m[1] }, m[1] ]
                  ] );
        continue;
      }

      // caps / abbr
      if ( (m = re_caps.exec( src )) ) {
        src.advance( m[0] );
        var caps = [ 'span', { 'class': 'caps' }, m[1] ];
        if ( m[2] ) {
          caps = [ 'acronym', { 'title': m[2] }, caps ]; // FIXME: use <abbr>, not acronym!
        }
        list.add( caps );
        continue;
      }

      // links
      if ( (boundary && (m = re_link.exec( src ))) || (m = re_link_fenced.exec( src )) ) {
        src.advance( m[0].length );
        var title = m[1].match( re_link_title )
          , inner = ( title ) ? m[1].slice( 0, m[1].length - title[0].length ) : m[1]
          ;
        if ( (pba = parse_attr( inner, 'a' )) ) {
          inner = inner.slice( pba[0] );
          pba = pba[1];
        }
        else {
          pba = {};
        }
        if ( title && !inner ) { inner = title[0]; title = ""; }
        pba.href = m[2];
        if ( title ) { pba.title = title[1]; }
        list.add( [ 'a', pba ].concat( parse_inline( inner.replace( /^(\.?\s*)/, '' ), options ) ) );
        continue;
      }

      // no match, move by all "uninteresting" chars
      m = /([a-zA-Z0-9,.':]+|\s+|[^\0])/.exec( src );
      if ( m ) {
        list.add( m[0] );
      }
      src.advance( m ? m[0].length || 1 : 1 );

    }
    while ( src.valueOf() );

    return list.get().map( parse_glyphs );
  }


  /* block parser */

  function parse_blocks ( src, options ) {

    var list = builder()
      , paragraph = function ( s, tag, pba, linebreak ) {
          tag = tag || 'p';
          var out = [];
          s.split( /\n\n+/ ).forEach(function( bit, i ) {
            if ( tag === 'p' && /^\s/.test( bit ) ) {
              // no-paragraphs
              // WTF?: Why does Textile not allow linebreaks in spaced lines
              bit = bit.replace( /\n[\t ]/g, ' ' ).trim();
              out = out.concat( parse_inline( bit, options ) );
            }
            else {
              if ( linebreak && i ) { out.push( linebreak ); }
              out.push( pba ? [ tag, pba ].concat( parse_inline( bit, options ) )
                            : [ tag      ].concat( parse_inline( bit, options ) ) );
            }
          });
          return out;
        }
      , link_refs = {}
      , m
      ;
    src = ribbon( src.replace( /^( *\n)+/, '' ) );

    // loop
    while ( src.valueOf() ) {
      src.save();

      // link_ref -- this goes first because it shouldn't trigger a linebreak
      if ( (m = re_link_ref.exec( src )) ) {
        src.advance( m[0] );
        link_refs[ m[1] ] = m[2];
        continue;
      }

      // add linebreak
      list.linebreak();

      // named block
      if ( (m = re_block.exec( src )) ) {
        src.advance( m[0] );
        var block_type = m[0]
          , pba = parse_attr( src, block_type )
          ;
        if ( pba ) {
          src.advance( pba[0] );
          pba = pba[1];
        }
        if ( (m = /\.(\.?)(?:\s|(?=:))/.exec( src )) ) {
          // FIXME: this whole copy_pba seems rather strange?
          // slurp rest of block
          var extended = !!m[1];
          m = ( extended ? re_block_extended : re_block_normal ).exec( src.advance( m[0] ) );
          src.advance( m[0] );
          // bq | bc | notextile | pre | h# | fn# | p | ###
          if ( block_type === 'bq' ) {
            var cite, inner = m[1];
            if ( (m = /^:(\S+)\s+/.exec( inner )) ) {
              if ( !pba ) { pba = {}; }
              pba.cite = m[1];
              inner = inner.slice( m[0].length );
            }
            // RedCloth adds all attr to both: this is bad because it produces duplicate IDs
            list.add( [ 'blockquote', pba, '\n' ].concat(
                    paragraph( inner, 'p', copy_pba(pba, { 'cite':1, 'id':1 }), '\n' )
                  ).concat(['\n']) );
          }
          else if ( block_type === 'bc' ) {
            var sub_pba = ( pba ) ? copy_pba(pba, { 'id':1 }) : null;
            list.add( [ 'pre', pba, ( sub_pba ? [ 'code', sub_pba, m[1] ] : [ 'code', m[1] ] ) ] );
          }
          else if ( block_type === 'notextile' ) {
            list.merge( parse_html( m[1] ) );
          }
          else if ( block_type === '###' ) {
            // ignore the insides
          }
          else if ( block_type === 'pre' ) {
            // I disagree with RedCloth, but agree with PHP here:
            // "pre(foo#bar).. line1\n\nline2" prevents multiline preformat blocks
            // ...which seems like the whole point of having an extended pre block?
            list.add( [ 'pre', pba, m[1] ] );
          }
          else if ( re_footnote_def.test( block_type ) ) { // footnote
            // Need to be careful: RedCloth fails "fn1(foo#m). footnote" -- it confuses the ID
            var fnid = block_type.replace( /\D+/g, '' );
            if ( !pba ) { pba = {}; }
            pba['class'] = ( pba['class'] ? pba['class'] + ' ' : '' ) + 'footnote';
            pba['id'] = 'fn' + fnid;
            list.add( [ "p", pba, [ 'a', { 'href': '#fnr' + fnid }, [ 'sup', fnid ] ], ' ' ].concat( parse_inline( m[1], options ) ) );
          }
          else { // heading | paragraph
            list.merge( paragraph( m[1], block_type, pba, '\n' ) );
          }
          continue;
        }
        else {
          src.load();
        }
      }

      // HTML comment
      if ( (m = re_html_comment.exec( src )) ) {
        src.advance( m[0] + (/(?:\s*\n+)+/.exec( src ) || [])[0] );
        list.add( [ '!', m[1] ] );
        continue;
      }

      // block HTML
      if ( (m = re_html_tag_block.exec( src )) ) {
        var tag    = m[1]
          , single = m[3] || tag in html_singletons
          , tail   = m[4]
          ;
        // Unsurprisingly, all Textile implementations I have tested have trouble parsing simple HTML:
        //
        //    "<div>a\n<div>b\n</div>c\n</div>d"
        //
        // I simply match them here as there is no way anyone is using nested HTML today, or if they
        // are, then this will at least output less broken HTML as redundant tags will get quoted.

        // Is block tag? ... 
        if ( tag in allowed_blocktags ) {
          src.advance( m[0] );

          var element = [ tag ];

          if ( m[2] ) {
            element.push( parse_html_attr( m[2] ) );
          }

          if ( single ) { // single tag
            // let us add the element and continue our quest...
            list.add( element );
            continue;
          }
          else { // block
            
            // gulp up the rest of this block... 
            var re_end_tag = re.compile( "^(.*?)(\\s*)(</" + tag + "\\s*>)(\\s*)", 's' );
            if ( (m = re_end_tag.exec( src )) ) {
              src.advance( m[0] );
              if ( tag === 'pre' ) {
                element.push( tail );
                element = element.concat( parse_html( m[1].replace( /\n+$/, '' ), { 'code': 1 } ) );
                if ( m[2] ) { element.push( m[2] ); }
                list.add( element );
              }
              else if ( tag === 'notextile' ) {
                element = parse_html( m[1].trim() );
                list.merge( element );
              }
              else if ( tag === 'script' || tag === 'noscript' ) {
                //element = parse_html( m[1].trim() );
                element.push( tail + m[1] );
                list.add( element );
              }
              else {
                // These strange (and unnecessary) linebreak tests are here to get the
                // tests working perfectly. In reality, this doesn't matter one bit.
                if ( /\n/.test( tail ) ) { element.push( '\n' ); }
                if ( /\n/.test( m[1] )  ) {
                  element = element.concat( parse_blocks( m[1], options ) );
                }
                else {
                  element = element.concat( parse_inline( m[1].replace( /^ +/, '' ), options ) );
                }
                if ( /\n/.test( m[2] ) ) { element.push( '\n' ); }

                list.add( element );
              }
              continue;
            }
            /*else {
              // end tag is missing, treat tag as normal text...
            }*/
          }
        }
        src.load();
      }

      // ruler
      if ( (m = re_ruler.exec( src )) ) {
        src.advance( m[0] );
        list.add( [ 'hr' ] );
        continue;
      }

      // list
      if ( (m = re_list.exec( src )) ) {
        src.advance( m[0] );
        list.add( parse_list( m[0], options ) );
        continue;
      }

      // table
      if ( (m = re_table.exec( src )) ) {
        src.advance( m[0] );
        list.add( parse_table( m[1], options ) );
        continue;
      }

      // paragraph
      m = re_block_normal.exec( src );
      list.merge( paragraph( m[1], 'p', undefined, "\n" ) );
      src.advance( m[0] );

    }

    return list.get().map( fix_links, link_refs );
  }


  // recurse the tree and swap out any "href" attributes 
  function fix_links ( jsonml ) {
    if ( _isArray( jsonml ) ) {
      if ( jsonml[0] === 'a' ) { // found a link
        var attr = jsonml[1];
        if ( typeof attr === "object" && 'href' in attr && attr.href in this ) {
          attr.href = this[ attr.href ];
        }
      }
      for (var i=1,l=jsonml.length; i<l; i++) {
        if ( _isArray( jsonml[i] ) ) {
          fix_links.call( this, jsonml[i] );
        }
      }
    }
    return jsonml;
  }



  /* exposed */

  function textile ( txt, opt ) {
    // get a throw-away copy of options
    opt = merge( merge( {}, textile.defaults ), opt || {} );
    // run the converter
    return parse_blocks( txt, opt ).map( JSONML.toHTML ).join( '' );
  }
  textile.defaults

  // options
  textile.defaults = {
    'breaks': true   // single-line linebreaks are converted to <br> by default
  };
  textile.setOptions = textile.setoptions = function ( opt ) {
    merge( textile.defaults, opt );
    return this;
  };


  textile.parse = textile.convert = textile;
  textile.html_parser = parse_html;
  textile.jsonml = function ( txt, opt ) {
    // get a throw-away copy of options
    opt = merge( merge( {}, textile.defaults ), opt || {} );
    // parse and return tree
    return [ 'html' ].concat( parse_blocks( txt, opt ) );
  };
  textile.serialize = JSONML.toHTML;

  if ( typeof module !== 'undefined' && module.exports ) {
    module.exports = textile;
  }
  else {
    this.textile = textile;
  }


}).call(function() {
  return this || (typeof window !== 'undefined' ? window : global);
}());
