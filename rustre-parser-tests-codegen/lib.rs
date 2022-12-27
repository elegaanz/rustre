use proc_macro2::{Ident, Literal, Span, TokenStream, TokenTree};
use quote::quote;
use std::path::{Path, PathBuf};

fn build_test(path: PathBuf, should_fail: bool) -> TokenStream {
    let path_displayed = path.canonicalize().unwrap().display().to_string();
    let path_lit = Literal::string(&path_displayed);
    let test_name = path
        .file_stem()
        .unwrap()
        .to_str()
        .expect("path isn't UTF-8")
        .replace(['-', ' '], "_")
        .to_lowercase();

    let prefix = if should_fail {
        "test_fail"
    } else {
        "test_success"
    };

    let test_fn_name = Ident::new(&format!("{prefix}_{test_name}"), Span::call_site());

    if should_fail && path.extension() != Some("lus".as_ref()) {
        return quote! {
            #[test]
            fn #test_fn_name() {
                // File extension was not .lus, skipping
            }
        };
    }

    // TODO remove the concat! once we can lex end-of-file line comments
    let parse_quote = quote! {
        let source = concat!(include_str!(#path_lit), "\n");
        let Parse { errors, .. } = Parse::parse(source);
    };

    if !should_fail {
        quote! {
            #[test]
            fn #test_fn_name() {
                #parse_quote
                assert!(
                    errors.is_empty(),
                    "parsing file {:?} produced {} errors",
                    #path_lit,
                    errors.len(),
                );
            }
        }
    } else {
        quote! {
            #[test]
            fn #test_fn_name() {
                #parse_quote
                assert!(
                    !errors.is_empty(),
                    "parsing file {:?} didn't produce any errors, but should have",
                    #path_lit,
                );
            }
        }
    }
}

fn test_fns(lustre_v6_dir: &Path) -> impl Iterator<Item = TokenStream> {
    let should_work_dir = lustre_v6_dir.join("test").join("should_work");
    let should_fail_dir = lustre_v6_dir.join("test").join("should_fail");

    [(should_work_dir, false), (should_fail_dir, true)]
        .map(|(dir, should_fail)| {
            dir.read_dir().unwrap().filter_map(move |entry| {
                let entry = entry.unwrap();
                let path = entry.path();

                if matches!(path.metadata(), Ok(meta) if meta.is_file()) {
                    Some(build_test(path, should_fail))
                } else {
                    None
                }
            })
        })
        .into_iter()
        .flatten()
}

#[proc_macro]
pub fn include_lustre_tests(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = TokenStream::from(input);

    let tokens = input.into_iter().collect::<Vec<_>>();

    assert_eq!(tokens.len(), 2);
    assert!(matches!(&tokens[0], TokenTree::Ident(ident) if *ident == "mod"));
    let mod_name = if let TokenTree::Ident(id) = &tokens[1] {
        id
    } else {
        panic!("missing module name");
    };

    let lustre_v6_dir = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("..")
        .join("..")
        .join("lustre-v6");

    let test_fns = test_fns(&lustre_v6_dir);

    let quoted = quote! {
        mod #mod_name {
            use super::*;
            #(#test_fns)*
        }
    };

    quoted.into()
}
