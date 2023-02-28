hljs.registerLanguage("lustre", (hljs) => ({
    name: "Lustre",
    keywords: {
        $pattern: /[a-z]+|->/,
        keyword: "include package model provides node function returns let tel assert pre fby",
        // operator: ["->"],
        built_in: "int real bool",
        literal: "false true",
    },
    contains: [
        hljs.QUOTE_STRING_MODE,
        hljs.C_NUMBER_MODE,
        {
            scope: "string",
            begin: '"',
            end: '"',
            contains: [{ begin: "\\\\." }],
        },
        hljs.COMMENT("--", "\n|$"),
        hljs.COMMENT("\\(\\*", "\\*\\)"),
    ],
}));

// mdbook doesn't let us add new languages to Highlight.js so we have to cheat a bit
// https://github.com/rust-lang/mdBook/issues/657
// https://github.com/pen-lang/pen/pull/395
hljs.initHighlightingOnLoad();
