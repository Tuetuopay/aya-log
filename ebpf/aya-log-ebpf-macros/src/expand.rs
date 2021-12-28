use proc_macro2::TokenStream;
use quote::quote;
use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    Expr, Result, Token,
};

pub(crate) struct LogArgs {
    pub(crate) ctx: Expr,
    pub(crate) target: Option<Expr>,
    pub(crate) lvl: Expr,
    pub(crate) format_string: Expr,
    pub(crate) formatting_args: Option<Punctuated<Expr, Token![,]>>,
}

mod kw {
    syn::custom_keyword!(target);
}

impl Parse for LogArgs {
    fn parse(input: ParseStream) -> Result<Self> {
        let ctx: Expr = input.parse()?;
        input.parse::<Token![,]>()?;

        // Parse `target: &str`, which is an optional argument.
        let target: Option<Expr> = match input.peek(kw::target) {
            true => {
                input.parse::<kw::target>()?;
                input.parse::<Token![:]>()?;
                let t: Expr = input.parse()?;
                input.parse::<Token![,]>()?;
                Some(t)
            }
            false => None,
        };

        let lvl: Expr = input.parse()?;
        input.parse::<Token![,]>()?;
        let format_string: Expr = input.parse()?;

        // Parse variadic arguments.
        let formatting_args: Option<Punctuated<Expr, Token![,]>> = if input.is_empty() {
            None
        } else {
            input.parse::<Token![,]>()?;
            Some(Punctuated::parse_terminated(input)?)
        };

        Ok(Self {
            ctx: ctx,
            target: target,
            lvl: lvl,
            format_string: format_string,
            formatting_args: formatting_args,
        })
    }
}

pub(crate) fn log(args: LogArgs) -> Result<TokenStream> {
    let ctx = args.ctx;
    let target = match args.target {
        Some(t) => quote! { #t },
        None => quote! { module_path!() },
    };
    let lvl = args.lvl;
    let format_string = args.format_string;

    let (num_args, quote_args) = match args.formatting_args {
        Some(formatting_args) => {
            let formatting_exprs = formatting_args.iter();
            let num_args = formatting_exprs.len();

            let mut quote_args = quote! {
                use ::aya_log_ebpf::WriteToBuf;
            };
            for arg in formatting_exprs {
                quote_args = quote! {
                    #quote_args
                    match #arg.write(&mut buf.buf[record_len..]) {
                        Ok(len) => record_len += len,
                        Err(_) => return,
                    };
                }
            }

            (num_args, quote_args)
        }
        None => (0, quote! {}),
    };

    Ok(quote! {
        let f = || {
            if let Some(buf) = unsafe { ::aya_log_ebpf::AYA_LOG_BUF.get_mut(0) } {
                if let Ok(header_len) = ::aya_log_ebpf::write_record_header(
                    &mut buf.buf,
                    #target,
                    #lvl,
                    module_path!(),
                    file!(),
                    line!(),
                    #num_args,
                ) {
                    if let Ok(message_len) = ::aya_log_ebpf::write_record_message(
                        &mut buf.buf[header_len..],
                        #format_string
                    ) {
                        let mut record_len = header_len + message_len;

                        #quote_args

                        if record_len <= ::aya_log_common::LOG_BUF_CAPACITY {
                            let _ = unsafe { ::aya_log_ebpf::AYA_LOGS.output(
                                #ctx,
                                &buf.buf[..record_len], 0
                            )};
                        }
                    }
                }
            }
        };
        f()
    })
}

pub(crate) struct LogLvlArgs {
    pub(crate) ctx: Expr,
    pub(crate) target: Option<Expr>,
    pub(crate) format_string: Expr,
    pub(crate) formatting_args: Option<Punctuated<Expr, Token![,]>>,
}
impl Parse for LogLvlArgs {
    fn parse(input: ParseStream) -> Result<Self> {
        let ctx: Expr = input.parse()?;
        input.parse::<Token![,]>()?;

        // Parse `target: &str`, which is an optional argument.
        let target: Option<Expr> = match input.peek(kw::target) {
            true => {
                input.parse::<kw::target>()?;
                input.parse::<Token![:]>()?;
                let t: Expr = input.parse()?;
                input.parse::<Token![,]>()?;
                Some(t)
            }
            false => None,
        };

        let format_string: Expr = input.parse()?;
        let formatting_args: Option<Punctuated<Expr, Token![,]>> = if input.is_empty() {
            None
        } else {
            input.parse::<Token![,]>()?;
            Some(Punctuated::parse_terminated(input)?)
        };

        Ok(Self {
            ctx: ctx,
            target: target,
            format_string: format_string,
            formatting_args: formatting_args,
        })
    }
}

fn log_with_level(level: TokenStream, args: LogLvlArgs) -> Result<TokenStream> {
    let LogLvlArgs { ctx, format_string, formatting_args, .. } = args;

    let target = args.target.map(|t| quote! { target: #t, });

    Ok(quote! { ::aya_log_ebpf_macros::log!(#ctx, #target #level, #format_string, #formatting_args) })
}

pub(crate) fn error(args: LogLvlArgs) -> Result<TokenStream> {
    log_with_level(quote! { ::aya_log_common::Level::Error }, args)
}

pub(crate) fn warn(args: LogLvlArgs) -> Result<TokenStream> {
    log_with_level(quote! { ::aya_log_common::Level::Warn }, args)
}

pub(crate) fn info(args: LogLvlArgs) -> Result<TokenStream> {
    log_with_level(quote! { ::aya_log_common::Level::Info }, args)
}

pub(crate) fn debug(args: LogLvlArgs) -> Result<TokenStream> {
    log_with_level(quote! { ::aya_log_common::Level::Debug }, args)
}

pub(crate) fn trace(args: LogLvlArgs) -> Result<TokenStream> {
    log_with_level(quote! { ::aya_log_common::Level::Trace }, args)
}
