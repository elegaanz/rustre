use case::CaseExt;
use std::collections::HashMap;
use std::{fs::File, io::Write};
use std::path::PathBuf;
use ungrammar::{Grammar, Rule};

fn main() {
    println!("cargo:rerun-if-changed=lustre.ungram");
    let grammar = std::fs::read_to_string("lustre.ungram").unwrap();

    let out_path = PathBuf::from(std::env::var("OUT_DIR").unwrap()).join("ast_generated.rs");
    let out = File::create(out_path).unwrap();
    let grammar: Grammar = grammar.parse().unwrap();

    Generator::new(out).generate(&grammar);
}

struct Generator {
    out: File,
    nodes: Vec<Node>,
    current_struct: Option<Struct>,
}

struct Node {
    name: String,
    def: Adt,
    traits: Vec<String>,
}

enum Adt {
    Struct(Struct),
    Enum(Vec<String>),
}

#[derive(Default)]
struct Struct {
    is_token: bool,
    unique_fields: Vec<(String, String, bool)>,
    list_fields: Vec<(String, String, bool)>,
    optional_fields: Vec<(String, String, bool)>,
    is_fields: Vec<(String, bool)>,
}

impl Generator {
    fn new(out: File) -> Self {
        Self {
            nodes: Vec::new(),
            out,
            current_struct: None,
        }
    }

    fn generate(mut self, grammar: &Grammar) {
        self.write_header();
        for node in grammar.iter() {
            let name = &grammar[node].name;
            let rule = &grammar[node].rule;
            match rule {
                Rule::Labeled { .. }
                | Rule::Node(_)
                | Rule::Token(_)
                | Rule::Seq(_)
                | Rule::Opt(_)
                | Rule::Rep(_) => {
                    self.current_struct = Some(Struct::default());
                    self.handle_rule(grammar, rule, None);
                    self.nodes.push(Node {
                        name: name.to_owned(),
                        def: Adt::Struct(self.current_struct.take().unwrap()),
                        traits: Vec::new(),
                    })
                }
                Rule::Alt(alternatives) => {
                    let enu = Adt::Enum(
                        alternatives
                            .iter()
                            .map(|rule| {
                                self.infer_name(grammar, &rule)
                            })
                            .collect(),
                    );
                    self.nodes.push(Node {
                        name: name.to_owned(),
                        def: enu,
                        traits: Vec::new(),
                    });
                }
            }
        }
        for tok in grammar.tokens() {
            self.nodes.push(Node {
                def: Adt::Struct(Struct{
                    is_token: true,
                    unique_fields: Vec::new(),
                    list_fields: Vec::new(),
                    optional_fields: Vec::new(),
                    is_fields: Vec::new(),
                }),
                name: grammar[tok].name.to_camel(),
                traits: Vec::new(),
            })
        }
        self.write_nodes();
    }

    fn handle_rule(&mut self, grammar: &Grammar, rule: &Rule, name: Option<String>) {
        let requested_name = name.is_some();
        let name = name.unwrap_or_else(|| self.infer_name(grammar, rule));
        let current_struct = self.current_struct.as_mut().unwrap();
        match rule {
            Rule::Labeled { label, rule } => {
                self.handle_rule(grammar, rule, Some(label.to_owned()));
            }
            Rule::Node(node) => {
                let node_name = grammar[*node].name.clone();
                current_struct.unique_fields.push((name.to_snake(), node_name, false));
            }
            Rule::Token(tok) => {
                let tok_name = grammar[*tok].name.clone();
                current_struct.unique_fields.push((name, Self::translate_token(&tok_name), true));
            }
            Rule::Seq(rules) => {
                if requested_name {
                    panic!("Labeled sequences are not supported");
                }
                for rule in rules {
                    self.handle_rule(grammar, rule, None);
                }
            }
            Rule::Opt(rule) => match **rule {
                Rule::Node(node) => {
                    let ty_name = grammar[node].name.clone();
                    current_struct.optional_fields.push((name.to_snake(), ty_name, false));
                }
                Rule::Token(tok) => {
                    let ty_name = grammar[tok].name.clone().to_camel();
                    match &ty_name[..] {
                        "Extern" | "Function" | "Node" | "Unsafe" | "True" | "False" => current_struct.is_fields.push((Self::translate_token(&name), true)),
                        _ => {},
                    }
                    current_struct.optional_fields.push((name.to_snake(), ty_name, true));
                }
                Rule::Labeled { .. }
                | Rule::Seq(_)
                | Rule::Alt(_)
                | Rule::Opt(_)
                | Rule::Rep(_) => todo!("not supported in options"),
            },
            Rule::Rep(rule) => match **rule {
                Rule::Node(node) => {
                    let ty_name = grammar[node].name.clone();
                    current_struct.list_fields.push((name.to_snake(), ty_name, false));
                }
                Rule::Token(tok) => {
                    let ty_name = grammar[tok].name.to_capitalized();
                    current_struct.list_fields.push((name.to_snake(), ty_name, true));
                }
                Rule::Labeled { .. }
                | Rule::Seq(_)
                | Rule::Alt(_)
                | Rule::Opt(_)
                | Rule::Rep(_) => todo!("not supported in repetitions"),
            },
            Rule::Alt(_) => todo!("Nested enums are not supported (in {})", name),
        }
    }

    fn write_header(&mut self) {
        writeln!(self.out, "// Auto-@generated file, do not edit manually.").ok();
        writeln!(
            self.out,
            "// If you want to make changes, either add a new impl block"
        )
        .ok();
        writeln!(
            self.out,
            "// outside of this file, or edit the build.rs file to generate"
        )
        .ok();
        writeln!(self.out, "// code differently.").ok();
        writeln!(self.out).ok();
        writeln!(self.out, "use crate::SyntaxNode;").ok();
        writeln!(self.out, "use crate::SyntaxToken;").ok();
        writeln!(self.out, "use crate::ast::AstNode;").ok();
        writeln!(self.out, "use crate::ast::AstToken;").ok();
        writeln!(self.out, "use crate::lexer::Token;").ok();
        writeln!(self.out, "use std::cmp::{{Ord, Ordering, PartialOrd}};").ok();
        writeln!(self.out, "use std::fmt::Debug;").ok();
        writeln!(self.out).ok();
    }

    fn write_nodes(mut self) {
        for node in self.nodes {
            let kind = &node.name;
            writeln!(self.out, "\n// {}\n", kind).ok();
            match node.def {
                Adt::Struct(struc) => {
                    let (ast, syntax, derive, needs_manual_debug) = if struc.is_token {
                        ("AstToken", "SyntaxToken", "Debug, ", false)
                    } else {
                        ("AstNode", "SyntaxNode", "", true)
                    };
                    if struc.is_token {
                        writeln!(self.out, "/// Token").ok();
                    }
                    writeln!(self.out, "#[derive({derive}Clone, PartialEq, Eq, Hash)]").ok();
                    writeln!(self.out, "pub struct {} {{", kind).ok();
                    writeln!(self.out, "    pub(crate) syntax: {},", syntax).ok();
                    writeln!(self.out, "}}").ok();
                    writeln!(self.out).ok();
                    for tr in node.traits {
                        writeln!(self.out, "impl {} for {} {{}}", tr, kind).ok();
                    }
                    writeln!(self.out, "impl {} for {} {{", ast, kind).ok();
                    writeln!(self.out, "    fn can_cast(kind: Token) -> bool {{").ok();
                    writeln!(self.out, "        kind == Token::{}", kind).ok();
                    writeln!(self.out, "    }}").ok();
                    writeln!(self.out, "").ok();
                    writeln!(
                        self.out,
                        "    fn cast(syntax: {}) -> Option<Self> {{",
                        syntax
                    )
                    .ok();
                    writeln!(self.out, "        if Self::can_cast(syntax.kind()) {{").ok();
                    writeln!(self.out, "            Some(Self {{ syntax }})").ok();
                    writeln!(self.out, "        }} else {{").ok();
                    writeln!(self.out, "            None").ok();
                    writeln!(self.out, "        }}").ok();
                    writeln!(self.out, "    }}").ok();
                    writeln!(self.out, "").ok();
                    writeln!(self.out, "    fn syntax(&self) -> &{} {{", syntax).ok();
                    writeln!(self.out, "        &self.syntax").ok();
                    writeln!(self.out, "    }}").ok();
                    writeln!(self.out, "    fn expect(syntax: {}) -> Self {{", syntax).ok();
                    writeln!(self.out, "        Self::cast(syntax).expect(\"Failed to cast to {}\")", kind).ok();
                    writeln!(self.out, "    }}").ok();
                    writeln!(self.out, "}}").ok();

                    if needs_manual_debug {
                        // impl Debug
                        writeln!(self.out).ok();
                        writeln!(self.out, "impl std::fmt::Debug for {} {{", kind).ok();
                        writeln!(self.out, "    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {{").ok();
                        writeln!(self.out, "        super::debug_ast_node(self, f, {:?})", kind).ok();
                        writeln!(self.out, "    }}").ok();
                        writeln!(self.out, "}}").ok();
                    }

                    // impl Ord
                    writeln!(self.out).ok();
                    writeln!(self.out, "impl PartialOrd for {} {{", kind).ok();
                    writeln!(self.out, "    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {{").ok();
                    writeln!(self.out, "        self.syntax().index().partial_cmp(&other.syntax().index())").ok();
                    writeln!(self.out, "    }}").ok();
                    writeln!(self.out, "}}").ok();
                    writeln!(self.out).ok();
                    writeln!(self.out, "impl Ord for {} {{", kind).ok();
                    writeln!(self.out, "    fn cmp(&self, other: &Self) -> Ordering {{").ok();
                    writeln!(self.out, "        self.syntax().index().cmp(&other.syntax().index())").ok();
                    writeln!(self.out, "    }}").ok();
                    writeln!(self.out, "}}").ok();

                    if !struc.is_fields.is_empty() || !struc.list_fields.is_empty() || !struc.optional_fields.is_empty() || !struc.unique_fields.is_empty() {
                        writeln!(self.out).ok();
                        writeln!(self.out, "impl {} {{", kind).ok();
                        for (is_field, is_token) in struc.is_fields {
                            writeln!(self.out, "    pub fn is_{}(&self) -> bool {{", is_field.to_snake()).ok();
                            if is_token {
                                writeln!(self.out, "        self.syntax().children_with_tokens().filter_map(|it| it.into_token()).any(|s| {}::cast(s).is_some())", is_field).ok();
                            } else {
                                writeln!(self.out, "        self.syntax().children().any(|s| {}::cast(s).is_some())", is_field).ok();
                            }
                            writeln!(self.out, "    }}").ok();
                        }
                        for (list_field, ty, is_token) in struc.list_fields {
                            writeln!(self.out, "    pub fn {}(&self) -> impl Iterator<Item = {}> {{", Self::unreserv(list_field), ty).ok();
                            if is_token {
                                writeln!(self.out, "        self.syntax().children_with_tokens().filter_map(|it| it.into_token()).filter_map(|s| {}::cast(s))", ty).ok();
                            } else {
                                writeln!(self.out, "        self.syntax().children().filter_map({}::cast)", ty).ok();
                            }
                            writeln!(self.out, "    }}").ok();
                        }
                        for (opt_field, ty, is_token) in struc.optional_fields {
                            writeln!(self.out, "    pub fn {}(&self) -> Option<{}> {{", Self::unreserv(opt_field), ty).ok();
                            if is_token {
                                writeln!(self.out, "        self.syntax().children_with_tokens().filter_map(|it| it.into_token()).find_map({}::cast)", ty).ok();
                            } else {
                                writeln!(self.out, "        self.syntax().children().find_map({}::cast)", ty).ok();
                            }
                            writeln!(self.out, "    }}").ok();
                        }
                        let mut type_counter = HashMap::new();
                        for (unique_field, ty, is_token) in struc.unique_fields {
                            let type_count = type_counter.entry(ty.clone()).or_insert(0);
                            writeln!(self.out, "    pub fn {}(&self) -> Option<{}> {{", Self::unreserv(unique_field.to_snake()), ty).ok();
                            if is_token {
                                writeln!(self.out, "        self.syntax().children_with_tokens().filter_map(|it| it.into_token()).filter_map({}::cast).nth({})", ty, type_count).ok();
                            } else {
                                writeln!(self.out, "        self.syntax().children().filter_map({}::cast).nth({})", ty, type_count).ok();
                            }
                            writeln!(self.out, "    }}").ok();
                            *type_count += 1;
                        }
                        writeln!(self.out, "}}").ok();
                    }
                }
                Adt::Enum(variants) => {
                    writeln!(self.out, "#[derive(Clone, PartialEq, Eq, Hash)]").ok();
                    writeln!(self.out, "pub enum {} {{", kind).ok();
                    for name in &variants {
                        writeln!(self.out, "    {}({}),", name, name).ok();
                    }
                    writeln!(self.out, "}}").ok();
                    for tr in node.traits {
                        writeln!(self.out, "impl {} for {} {{}}", tr, kind).ok();
                    }
                    writeln!(self.out, "impl AstNode for {} {{", kind).ok();
                    writeln!(self.out, "    fn can_cast(kind: Token) -> bool {{").ok();
                    writeln!(self.out, "        {}", variants.iter().map(|name| format!("{}::can_cast(kind)", name)).collect::<Vec<_>>().join(" || ")).ok();
                    writeln!(self.out, "    }}").ok();
                    writeln!(self.out, "").ok();
                    writeln!(
                        self.out,
                        "    fn cast(syntax: SyntaxNode) -> Option<Self> {{"
                    )
                    .ok();
                    let mut vars = variants.iter();
                    if let Some(first_var) = vars.next() {
                        writeln!(self.out, "        {}::cast(syntax.clone()).map(Self::{})", first_var, first_var).ok();
                    }
                    for other_var in vars {
                        writeln!(self.out, "            .or_else(|| {}::cast(syntax.clone()).map(Self::{}))", other_var, other_var).ok();
                    }
                    writeln!(self.out, "    }}").ok();
                    writeln!(self.out, "").ok();
                    writeln!(self.out, "    fn syntax(&self) -> &SyntaxNode {{").ok();
                    writeln!(self.out, "        match self {{").ok();
                    for var in &variants {
                        writeln!(self.out, "            Self::{}(x) => x.syntax(),", var).ok();
                    }
                    writeln!(self.out, "        }}").ok();
                    writeln!(self.out, "    }}").ok();
                    writeln!(self.out, "    fn expect(syntax: SyntaxNode) -> Self {{").ok();
                    writeln!(self.out, "        Self::cast(syntax).expect(\"Failed to cast to {}\")", kind).ok();
                    writeln!(self.out, "    }}").ok();
                    writeln!(self.out, "}}").ok();

                    // impl Debug
                    writeln!(self.out).ok();
                    writeln!(self.out, "impl Debug for {} {{", kind).ok();
                    writeln!(self.out, "    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {{").ok();
                    writeln!(self.out, "        super::debug_ast_node(self, f, {:?})", kind).ok();
                    writeln!(self.out, "    }}").ok();
                    writeln!(self.out, "}}").ok();

                    // impl Ord
                    writeln!(self.out).ok();
                    writeln!(self.out, "impl PartialOrd for {} {{", kind).ok();
                    writeln!(self.out, "    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {{").ok();
                    writeln!(self.out, "        self.syntax().index().partial_cmp(&other.syntax().index())").ok();
                    writeln!(self.out, "    }}").ok();
                    writeln!(self.out, "}}").ok();
                    writeln!(self.out).ok();
                    writeln!(self.out, "impl Ord for {} {{", kind).ok();
                    writeln!(self.out, "    fn cmp(&self, other: &Self) -> Ordering {{").ok();
                    writeln!(self.out, "        self.syntax().index().cmp(&other.syntax().index())").ok();
                    writeln!(self.out, "    }}").ok();
                    writeln!(self.out, "}}").ok();

                    writeln!(self.out).ok();
                    writeln!(self.out, "impl {} {{", kind).ok();
                    for name in variants {
                        let lower_name = name.to_snake();
                        writeln!(self.out, "    pub fn is_{}(&self) -> bool {{", lower_name).ok();
                        writeln!(
                            self.out,
                            "        if let {}::{}(_) = *self {{ true }} else {{ false }}",
                            kind,
                            name,
                        )
                        .ok();
                        writeln!(self.out, "    }}").ok();
                        // unwrap
                        writeln!(
                            self.out,
                            "    pub fn unwrap_{}(&self) -> {} {{",
                            lower_name,
                            name.clone()
                        )
                        .ok();
                        writeln!(
                            self.out,
                            "        if let {}::{}(data) = self {{ data.clone() }} else {{ panic!(\"Failed to unwrap {} as {}\") }}",
                            kind,
                            name,
                            kind,
                            name,
                        )
                        .ok();
                        writeln!(self.out, "    }}").ok();
                    }
                    writeln!(self.out, "}}").ok();
                }
            }
        }
    }

    fn infer_name(&mut self, grammar: &Grammar, rule: &Rule) -> String {
        match rule {
            Rule::Labeled { label, .. } => label.to_owned(),
            Rule::Node(node) => grammar[*node].name.to_owned(),
            Rule::Token(tok) => grammar[*tok].name.to_capitalized(),
            Rule::Seq(s) => s
                .iter()
                .map(|r| self.infer_name(grammar, r))
                .collect::<Vec<_>>()
                .join("_"),
            Rule::Alt(alts) => alts
                .iter()
                .map(|r| self.infer_name(grammar, r))
                .collect::<Vec<_>>()
                .join("_or_"),
            Rule::Opt(rule) => self.infer_name(grammar, rule),
            Rule::Rep(rule) => format!("All{}", self.infer_name(grammar, rule)),
        }
    }

    fn translate_token(tok: &str) -> String {
        let name = match tok {
            "+" => "Plus",
            named => return named.to_camel()
        };
        name.to_owned()
    }

    fn unreserv(name: String) -> String {
        match &name[..] {
            "extern" | "unsafe" | "const" | "type" | "true" | "false" | "enum" | "struct" | "impl" | "if" | "else" | "mod" => format!("r#{}", name),
            _ => name,
        }
    }
}
