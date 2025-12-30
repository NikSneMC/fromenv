use darling::{
    FromField, FromMeta,
    ast::NestedMeta,
    util::{Flag, Override},
};
use proc_macro2::Span;
use syn::{
    Attribute, ExprPath, GenericArgument, Ident, LitStr, PathArguments, Type, spanned::Spanned,
};

#[derive(Debug)]
pub struct FromEnvFieldReceiver {
    pub ident: Ident,
    pub ty: Type,
    pub option: Option<Type>,
    pub doc_attrs: Vec<Attribute>,
    pub env_attr: EnvAttribute,
}

#[derive(Debug)]
pub enum EnvAttribute {
    /// #[env(from = "...")]
    Flat {
        name: LitStr,
        from: Option<LitStr>,
        default: Option<LitStr>,
        with: Option<ExprPath>,
    },
    /// #[env(nested)]
    Nested,
    /// No config attr.
    None,
}

impl FromField for FromEnvFieldReceiver {
    fn from_field(field: &syn::Field) -> darling::Result<Self> {
        let mut accumulator = darling::Error::accumulator();
        let ident = field
            .ident
            .as_ref()
            .map(|f| f.to_owned())
            .expect("asserted the shape of the struct already");

        let ty = field.ty.to_owned();
        let option = parse_option(&ty).map(ToOwned::to_owned);

        let mut rename: Option<Override<LitStr>> = None;
        let mut from: Option<Override<LitStr>> = None;
        let mut default: Option<LitStr> = None;
        let mut with: Option<ExprPath> = None;
        let mut nested = Flag::default();
        let mut ignored = Flag::default();

        let mut default_path_span = Span::call_site();

        let mut doc_attrs = Vec::new();

        for attr in &field.attrs {
            if attr.path().is_ident("env") {
                let meta_list = match attr.meta.require_list() {
                    Ok(list) => list,
                    Err(e) => {
                        accumulator.push(e.into());
                        continue;
                    }
                };

                let nested_meta_list = match NestedMeta::parse_meta_list(meta_list.tokens.clone()) {
                    Ok(nested) => nested,
                    Err(e) => {
                        accumulator.push(e.into());
                        continue;
                    }
                };

                for meta in nested_meta_list {
                    let meta = match meta {
                        NestedMeta::Meta(meta) => meta,
                        NestedMeta::Lit(lit) => {
                            accumulator.push(darling::Error::unexpected_lit_type(&lit));
                            continue;
                        }
                    };

                    if meta.path().is_ident("from") {
                        match FromMeta::from_meta(&meta) {
                            Ok(v) => from = Some(v),
                            Err(e) => {
                                accumulator.push(e);
                            }
                        }
                    } else if meta.path().is_ident("rename") {
                        match FromMeta::from_meta(&meta) {
                            Ok(v) => rename = Some(v),
                            Err(e) => {
                                accumulator.push(e);
                            }
                        }
                    } else if meta.path().is_ident("default") {
                        default_path_span = meta.path().span();
                        match FromMeta::from_meta(&meta) {
                            Ok(v) => default = Some(v),
                            Err(e) => {
                                accumulator.push(e);
                            }
                        }
                    } else if meta.path().is_ident("with") {
                        match FromMeta::from_meta(&meta) {
                            Ok(v) => with = Some(v),
                            Err(e) => {
                                accumulator.push(e);
                            }
                        }
                    } else if meta.path().is_ident("nested") {
                        match FromMeta::from_meta(&meta) {
                            Ok(v) => nested = v,
                            Err(e) => {
                                accumulator.push(e);
                            }
                        }
                    } else if meta.path().is_ident("ignored") {
                        match FromMeta::from_meta(&meta) {
                            Ok(v) => ignored = v,
                            Err(e) => {
                                accumulator.push(e);
                            }
                        }
                    } else {
                        accumulator.push(
                            darling::Error::unknown_field_path(meta.path()).with_span(&meta.span()),
                        );
                    }
                }
            } else if attr.path().is_ident("doc") {
                doc_attrs.push(attr.clone());
            }
        }

        const IGNORED_CLASH: &str = "`ignored` cannot be used with other attributes";
        const NESTED_CLASH: &str = "`nested` cannot be used with other attributes";
        const OPTION_WIH_DEFAULT: &str = "Optional fields cannot have a default";

        match (
            from,
            rename,
            default,
            with,
            nested.is_present(),
            ignored.is_present(),
        ) {
            (Some(_), _, _, _, _, true)
            | (_, Some(_), _, _, _, true)
            | (_, _, Some(_), _, _, true)
            | (_, _, _, Some(_), _, true)
            | (_, _, _, _, true, true) => {
                accumulator.push(darling::Error::custom(IGNORED_CLASH).with_span(&ignored.span()));

                Err(accumulator.finish().unwrap_err())
            }
            (Some(_), _, _, _, true, _)
            | (_, Some(_), _, _, true, _)
            | (_, _, Some(_), _, true, _)
            | (_, _, _, Some(_), true, _) => {
                accumulator.push(darling::Error::custom(NESTED_CLASH).with_span(&nested.span()));

                Err(accumulator.finish().unwrap_err())
            }
            (None, None, None, None, false, true) => accumulator.finish_with(Self {
                ident,
                ty,
                option,
                doc_attrs,
                env_attr: EnvAttribute::None,
            }),
            (None, None, None, None, true, false) => accumulator.finish_with(Self {
                ident,
                ty,
                option,
                doc_attrs,
                env_attr: EnvAttribute::Nested,
            }),
            (from, rename, default, with, false, false) => {
                if option.is_some() && default.is_some() {
                    let err =
                        darling::Error::custom(OPTION_WIH_DEFAULT).with_span(&default_path_span);

                    accumulator.push(err);
                }

                let default_name = || LitStr::new(&ident.to_string().to_uppercase(), ident.span());
                let name = match rename {
                    None => default_name(),
                    Some(rename) => rename.unwrap_or_else(default_name),
                };

                let from = from.map(|from| from.unwrap_or_else(default_name));

                accumulator.finish_with(Self {
                    ident,
                    ty,
                    option,
                    doc_attrs,
                    env_attr: EnvAttribute::Flat {
                        name,
                        from,
                        default,
                        with,
                    },
                })
            }
        }
    }
}

fn parse_option(ty: &Type) -> Option<&Type> {
    let Type::Path(type_path) = ty else {
        return None;
    };

    let segment = type_path.path.segments.last()?;

    let generic_args = if segment.ident == "Option" {
        let PathArguments::AngleBracketed(generic_args) = &segment.arguments else {
            return None;
        };
        generic_args
    } else {
        return None;
    };

    if generic_args.args.len() == 1 {
        if let GenericArgument::Type(inner_type) = &generic_args.args[0] {
            return Some(inner_type);
        }
    }

    None
}
