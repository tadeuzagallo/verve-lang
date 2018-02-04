Prism.languages.verve = {
  'comment': [
    {
      pattern: /(^|[^\\])\/\*[\w\W]*?\*\//,
      lookbehind: true
    },
    {
      pattern: /(^|[^\\:])\/\/.*/,
      lookbehind: true
    }
  ],
  'string': [
    /"(?:\\?.)*?"\1/,
    /'(?:\\?.)'/
  ],
  'keyword': /\b(?:case|class|else|enum|fn|if|interface|implementation|let|match|operator|self|type)\b/,

  'attribute': {
    pattern: /#\[.+?\]/,
    alias: 'attr-name'
  },

  'function': {
    pattern: /[a-z0-9_]+(?=\s*\()/i,
    inside: {
      punctuation: /:|,|->/,
    }
  },

	'number': /\b(0x[\dA-Fa-f]+|0b[01]+|0o[0-7]+|\d*\.?\d+([Ee][+-]?\d+)?)\b/,

  'punctuation': /[{}[\];(),:]/,
  'operator': /[:!#$%&*+./<=>?@\\^|\-~]+/,
};
