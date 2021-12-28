#![no_std]

use proc_macro::TokenStream;
use syn::parse_macro_input;

mod expand;

#[proc_macro]
pub fn log(args: TokenStream) -> TokenStream {
    let args = parse_macro_input!(args as expand::LogArgs);
    expand::log(args)
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}

#[proc_macro]
pub fn error(args: TokenStream) -> TokenStream {
    let args = parse_macro_input!(args as expand::LogLvlArgs);
    expand::error(args)
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}

#[proc_macro]
pub fn warn(args: TokenStream) -> TokenStream {
    let args = parse_macro_input!(args as expand::LogLvlArgs);
    expand::warn(args)
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}

#[proc_macro]
pub fn info(args: TokenStream) -> TokenStream {
    let args = parse_macro_input!(args as expand::LogLvlArgs);
    expand::info(args)
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}

#[proc_macro]
pub fn debug(args: TokenStream) -> TokenStream {
    let args = parse_macro_input!(args as expand::LogLvlArgs);
    expand::debug(args)
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}

#[proc_macro]
pub fn trace(args: TokenStream) -> TokenStream {
    let args = parse_macro_input!(args as expand::LogLvlArgs);
    expand::trace(args)
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}
