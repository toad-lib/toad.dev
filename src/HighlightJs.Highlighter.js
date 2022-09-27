const {HLJS: makeHljs} = require('highlight.js/lib/core');

export const make = makeHljs({});

export const registerLanguage_ =
  ({languageAliasString}) =>
  alias =>
  lang =>
  hljs =>
  () => {
    hljs.registerLanguage(
      languageAliasString(alias),
      lang,
    );
  };

export const highlight_ =
  ({ left
   , right
   , rawHtml
   , rawHtmlString
   , languageAliasString
   }) =>
  alias =>
  html =>
  hljs =>
  () => {
    try {
      const result = hljs.highlight(languageAliasString(alias), rawHtmlString(html));

      if (result.errorRaised) {
        throw result.errorRaised;
      }

      return right(rawHtml(result.value));
    } catch (error) {
      return left(error);
    }
  };
