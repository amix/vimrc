// Process block-level uml diagrams

const plantumlEncoder = require("plantuml-encoder");

module.exports = function umlPlugin(md, options) {

  function generateSourceDefault(umlCode, pluginOptions) {
    var imageFormat = pluginOptions.imageFormat || 'img';
    var diagramName = pluginOptions.diagramName || 'uml';
    var server = pluginOptions.server || 'https://www.plantuml.com/plantuml';
    var zippedCode = plantumlEncoder.encode(umlCode);

    return server + '/' + imageFormat + '/' + zippedCode;
  }

  options = options || {};

  var openMarker = options.openMarker || '@startuml',
      openChar = openMarker.charCodeAt(0),
      closeMarker = options.closeMarker || '@enduml',
      closeChar = closeMarker.charCodeAt(0),
      render = options.render || md.renderer.rules.image,
      generateSource = options.generateSource || generateSourceDefault;

  function uml(state, startLine, endLine, silent) {
    var nextLine, markup, params, token, i,
        autoClosed = false,
        start = state.bMarks[startLine] + state.tShift[startLine],
        max = state.eMarks[startLine];

    // Check out the first character quickly,
    // this should filter out most of non-uml blocks
    //
    if (openChar !== state.src.charCodeAt(start)) { return false; }

    // Check out the rest of the marker string
    //
    for (i = 0; i < openMarker.length; ++i) {
      if (openMarker[i] !== state.src[start + i]) { return false; }
    }

    markup = state.src.slice(start, start + i);
    params = state.src.slice(start + i, max);

    // Since start is found, we can report success here in validation mode
    //
    if (silent) { return true; }

    // Search for the end of the block
    //
    nextLine = startLine;

    for (;;) {
      nextLine++;
      if (nextLine >= endLine) {
        // unclosed block should be autoclosed by end of document.
        // also block seems to be autoclosed by end of parent
        break;
      }

      start = state.bMarks[nextLine] + state.tShift[nextLine];
      max = state.eMarks[nextLine];

      if (start < max && state.sCount[nextLine] < state.blkIndent) {
        // non-empty line with negative indent should stop the list:
        // - ```
        //  test
        break;
      }

      if (closeChar !== state.src.charCodeAt(start)) {
        // didn't find the closing fence
        continue;
      }

      if (state.sCount[nextLine] > state.sCount[startLine]) {
        // closing fence should not be indented with respect of opening fence
        continue;
      }

      var closeMarkerMatched = true;
      for (i = 0; i < closeMarker.length; ++i) {
        if (closeMarker[i] !== state.src[start + i]) {
          closeMarkerMatched = false;
          break;
        }
      }

      if (!closeMarkerMatched) {
        continue;
      }

      // make sure tail has spaces only
      if (state.skipSpaces(start + i) < max) {
        continue;
      }

      // found!
      autoClosed = true;
      break;
    }

    var contents = state.src
      .split('\n')
      .slice(startLine + 1, nextLine)
      .join('\n');

    // We generate a token list for the alt property, to mimic what the image parser does.
    var altToken = [];
    // Remove leading space if any.
    var alt = params ? params.slice(1) : 'uml diagram';
    state.md.inline.parse(
      alt,
      state.md,
      state.env,
      altToken
    );

    token = state.push('uml_diagram', 'img', 0);
    // alt is constructed from children. No point in populating it here.
    token.attrs = [ [ 'src', generateSource(contents, options) ], [ 'alt', '' ] ];
    token.block = true;
    token.children = altToken;
    token.info = params;
    token.map = [ startLine, nextLine ];
    token.markup = markup;

    state.line = nextLine + (autoClosed ? 1 : 0);

    return true;
  }

  md.block.ruler.before('fence', 'uml_diagram', uml, {
    alt: [ 'paragraph', 'reference', 'blockquote', 'list' ]
  });
  md.renderer.rules.uml_diagram = render;
};
