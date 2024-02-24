use proc_macro2::Span;
use quote::{quote, ToTokens};
use syn::{spanned::Spanned, Ident};

#[derive(Debug, thiserror::Error)]
#[error("unsupported type")]
pub struct Unsupported(pub Span);

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    /// A path like `std::slice::Iter`, optionally qualified with a
    /// self-type as in `<Vec<T> as SomeTrait>::Associated`.
    Path(TypePath),
}

impl TryFrom<syn::Type> for Type {
    type Error = Unsupported;

    fn try_from(value: syn::Type) -> Result<Self, Self::Error> {
        match value {
            syn::Type::Path(p) => Ok(Self::Path(p.try_into()?)),
            other => Err(Unsupported(other.span())),
        }
    }
}

impl ToTokens for Type {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            Self::Path(p) => p.to_tokens(tokens),
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypePath {
    pub qself: Option<Box<Type>>,
    pub path: Path,
}

impl TryFrom<syn::TypePath> for TypePath {
    type Error = Unsupported;

    fn try_from(value: syn::TypePath) -> Result<Self, Self::Error> {
        Ok(Self {
            qself: value
                .qself
                .map(|q| (*q.ty).try_into().map(Box::new))
                .transpose()?,
            path: value.path.try_into()?,
        })
    }
}

impl ToTokens for TypePath {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match &self.qself {
            Some(qself) => {
                let path = &self.path;
                tokens.extend(quote!(<#qself as #path>))
            }
            None => self.path.to_tokens(tokens),
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Path {
    pub leading_colon: bool,
    pub segments: Vec<PathSegment>,
}

impl TryFrom<syn::Path> for Path {
    type Error = Unsupported;

    fn try_from(value: syn::Path) -> Result<Self, Self::Error> {
        Ok(Self {
            leading_colon: value.leading_colon.is_some(),
            segments: value
                .segments
                .into_iter()
                .map(TryInto::try_into)
                .collect::<Result<_, _>>()?,
        })
    }
}

impl ToTokens for Path {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        if self.leading_colon {
            tokens.extend(quote!(::))
        }

        for (i, segment) in self.segments.iter().enumerate() {
            if i > 0 {
                tokens.extend(quote!(::))
            }

            segment.to_tokens(tokens)
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PathSegment {
    pub ident: Ident,
    pub arguments: PathArguments,
}

impl TryFrom<syn::PathSegment> for PathSegment {
    type Error = Unsupported;

    fn try_from(value: syn::PathSegment) -> Result<Self, Self::Error> {
        Ok(Self {
            ident: value.ident,
            arguments: value.arguments.try_into()?,
        })
    }
}

impl ToTokens for PathSegment {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        self.ident.to_tokens(tokens);
        self.arguments.to_tokens(tokens)
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum PathArguments {
    None,

    /// The `<'a, T>` in `std::slice::iter<'a, T>`.
    AngleBracketed(AngleBracketedGenericArguments),
}

impl TryFrom<syn::PathArguments> for PathArguments {
    type Error = Unsupported;

    fn try_from(value: syn::PathArguments) -> Result<Self, Self::Error> {
        match value {
            syn::PathArguments::None => Ok(Self::None),
            syn::PathArguments::AngleBracketed(a) => Ok(Self::AngleBracketed(a.try_into()?)),
            other => Err(Unsupported(other.span())),
        }
    }
}

impl ToTokens for PathArguments {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            Self::None => (),
            Self::AngleBracketed(a) => a.to_tokens(tokens),
        }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct AngleBracketedGenericArguments {
    pub colon2_token: bool,
    pub args: Vec<GenericArgument>,
}

impl TryFrom<syn::AngleBracketedGenericArguments> for AngleBracketedGenericArguments {
    type Error = Unsupported;

    fn try_from(value: syn::AngleBracketedGenericArguments) -> Result<Self, Self::Error> {
        Ok(Self {
            colon2_token: value.colon2_token.is_some(),
            args: value
                .args
                .into_iter()
                .map(TryInto::try_into)
                .collect::<Result<_, _>>()?,
        })
    }
}

impl ToTokens for AngleBracketedGenericArguments {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        if self.colon2_token {
            tokens.extend(quote!(::))
        }

        tokens.extend(quote!(<));

        for (i, arg) in self.args.iter().enumerate() {
            if i > 0 {
                tokens.extend(quote!(,))
            }

            arg.to_tokens(tokens)
        }

        tokens.extend(quote!(>));
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum GenericArgument {
    /// A lifetime argument.
    Lifetime(syn::Lifetime),

    /// A type argument.
    Type(Type),
}

impl TryFrom<syn::GenericArgument> for GenericArgument {
    type Error = Unsupported;

    fn try_from(value: syn::GenericArgument) -> Result<Self, Self::Error> {
        match value {
            syn::GenericArgument::Lifetime(lft) => Ok(Self::Lifetime(lft)),
            syn::GenericArgument::Type(t) => Ok(Self::Type(t.try_into()?)),
            other => Err(Unsupported(other.span())),
        }
    }
}

impl ToTokens for GenericArgument {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        match self {
            Self::Lifetime(name) => name.to_tokens(tokens),
            Self::Type(t) => t.to_tokens(tokens),
        }
    }
}
