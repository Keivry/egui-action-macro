//! Procedural macros for automatic ActionArgs wrapper generation.
//!
//! This crate provides two complementary macros for the egui-action system:
//!
//! 1. **`#[impl_action]` attribute macro**: Generates `__action_xxx__` wrapper methods/functions
//!    that adapt existing APIs to accept `ActionArgs` instead of raw parameters. Wrappers handle
//!    parameter extraction from ActionArgs at call sites.
//!
//! 2. **`action!()` function macro**: Creates typed `Command` variants that wrap closures or
//!    function paths, bridging closure/path syntax to the action system. Supports multiple action
//!    modes (null, bool, usize, string, key, result).
//!
//! **Design Rationale**:
//! - Wrappers are intentionally simple to keep macro compile times low and error messages clear
//! - Type inference uses string matching (see `infer_converter`) to avoid heavyweight syn parsing
//! - Generated wrapper names use `__action_xxx__` prefix to prevent namespace collisions
//! - Both macros emit `use egui_action::...` imports to avoid polluting the caller's namespace

use {
    proc_macro::TokenStream,
    quote::quote,
    syn::{
        Expr,
        ExprClosure,
        FnArg,
        ImplItem,
        ImplItemFn,
        ItemFn,
        ItemImpl,
        Pat,
        Path,
        ReturnType,
        Signature,
        Token,
        Type,
        parse::{Parse, ParseStream},
        parse_macro_input,
        parse_quote,
        punctuated::Punctuated,
        spanned::Spanned,
    },
};

/// Procedural macro to generate ActionArgs-based wrapper methods/functions
///
/// Can be applied to:
/// 1. `impl` blocks - generates wrappers for all public methods
/// 2. Standalone functions - generates a single wrapper function
///
/// **Note:** Cannot be applied to individual methods within `impl` blocks due to Rust macro
/// limitations.
///
/// # Example 1: On impl block
///
/// ```ignore
/// #[impl_action]
/// impl MyApp {
///     pub fn set_volume(&mut self, volume: usize) {
///         self.volume = volume;
///     }
/// }
/// ```
///
/// Generates `__action_set_volume__(&mut self, args: &ActionArgs)` for all public methods
///
/// # Example 2: On standalone function
///
/// ```ignore
/// #[impl_action]
/// pub fn process_data(app: &mut App, data: &str) -> bool {
///     // implementation
/// }
/// ```
///
/// Generates both the original function and:
/// `pub fn __action_process_data__(app: &mut App, args: &ActionArgs) -> bool`
#[proc_macro_attribute]
pub fn impl_action(_attr: TokenStream, item: TokenStream) -> TokenStream {
    // Try parsing as impl block first
    if let Ok(input) = syn::parse::<ItemImpl>(item.clone()) {
        return handle_impl_block(input);
    }

    // Try parsing as function
    if let Ok(input) = syn::parse::<ItemFn>(item) {
        return handle_function(input);
    }

    // Not supported
    TokenStream::from(quote! {
        compile_error!("#[impl_action] can only be applied to impl blocks or standalone functions");
    })
}

/// Handle #[impl_action] on impl block - generates wrappers for all public methods
fn handle_impl_block(input: ItemImpl) -> TokenStream {
    let mut modified_impl = input.clone();
    let mut wrapper_methods = Vec::new();

    // Generate wrappers for public methods
    for item in &input.items {
        if let ImplItem::Fn(method) = item
            && is_public(method)
            && let Some(wrapper) = generate_method_wrapper(method)
        {
            wrapper_methods.push(wrapper);
        }
    }

    // Append generated wrapper methods to the impl block
    modified_impl.items.extend(wrapper_methods);

    TokenStream::from(quote! { #modified_impl })
}

/// Handle #[impl_action] on standalone function
fn handle_function(input: ItemFn) -> TokenStream {
    let original_fn = &input;
    let wrapper_fn = generate_function_wrapper(&input);

    TokenStream::from(quote! {
        #original_fn
        #wrapper_fn
    })
}

/// Check if method is public
fn is_public(method: &ImplItemFn) -> bool { matches!(&method.vis, syn::Visibility::Public(_)) }

/// Generate __action_xxx__ wrapper for method.
/// Wrapper names use `__action_xxx__` prefix to prevent collisions with user-defined methods
/// while remaining clearly associated with the original method name.
fn generate_method_wrapper(method: &ImplItemFn) -> Option<ImplItem> {
    let method_name = &method.sig.ident;
    let wrapper_name = syn::Ident::new(&format!("__action_{}__", method_name), method_name.span());

    // Extract parameters (skip &mut self / &self)
    let params: Vec<_> = extract_params(&method.sig);

    if params.is_empty() {
        // No parameters - simple passthrough
        let return_ty = extract_return_type(&method.sig);
        return Some(parse_quote! {
            pub fn #wrapper_name(&mut self, _args: &ActionArgs) -> #return_ty {
                self.#method_name()
            }
        });
    }

    // Generate parameter extraction code
    let extractions = generate_extractions(&params, method_name);
    let call_args = params.iter().map(|(name, _)| name);
    let return_ty = extract_return_type(&method.sig);

    Some(parse_quote! {
        pub fn #wrapper_name(&mut self, args: &ActionArgs) -> #return_ty {
            #(#extractions)*
            self.#method_name(#(#call_args),*)
        }
    })
}

/// Generate __action_xxx__ wrapper for standalone function
fn generate_function_wrapper(func: &ItemFn) -> proc_macro2::TokenStream {
    let func_name = &func.sig.ident;
    let wrapper_name = syn::Ident::new(&format!("__action_{}__", func_name), func_name.span());
    let vis = &func.vis;

    // Extract ALL parameters
    let all_params: Vec<_> = func
        .sig
        .inputs
        .iter()
        .filter_map(|arg| {
            if let FnArg::Typed(pat_type) = arg
                && let Pat::Ident(pat_ident) = &*pat_type.pat
            {
                return Some((pat_ident.ident.clone(), pat_type.ty.clone()));
            }
            None
        })
        .collect();

    if all_params.is_empty() {
        // No parameters at all
        let return_ty = extract_return_type(&func.sig);
        return quote! {
            #vis fn #wrapper_name(_args: &ActionArgs) -> #return_ty {
                #func_name()
            }
        };
    }

    // Strategy: First parameter is passed through, rest come from ActionArgs
    let (first_param_name, first_param_ty) = &all_params[0];
    let args_params: Vec<_> = all_params.iter().skip(1).cloned().collect();

    if args_params.is_empty() {
        // Only one parameter (typically app/context) - no ActionArgs needed
        let return_ty = extract_return_type(&func.sig);
        return quote! {
            #vis fn #wrapper_name(#first_param_name: #first_param_ty, _args: &ActionArgs) -> #return_ty {
                #func_name(#first_param_name)
            }
        };
    }

    // Generate extractions for remaining parameters
    let extractions = if args_params.len() == 1 {
        // Single arg from ActionArgs
        let (name, ty) = &args_params[0];
        let converter = infer_converter(ty);
        let error_msg = format!("Expected {} for {}", quote! { #ty }, func_name);

        vec![quote! {
            let #name = args.#converter().expect(#error_msg);
        }]
    } else {
        // Multiple args from ActionArgs - use idx()
        args_params
            .iter()
            .enumerate()
            .map(|(idx, (name, ty))| {
                let converter = infer_converter(ty);
                let error_msg = format!(
                    "Expected {} at index {} for {}",
                    quote! { #ty },
                    idx,
                    func_name
                );

                quote! {
                    let #name = args.idx(#idx)
                        .and_then(|a| a.#converter())
                        .expect(#error_msg);
                }
            })
            .collect()
    };

    let all_call_args = all_params.iter().map(|(name, _)| name);
    let return_ty = extract_return_type(&func.sig);

    quote! {
        #vis fn #wrapper_name(#first_param_name: #first_param_ty, args: &ActionArgs) -> #return_ty {
            #(#extractions)*
            #func_name(#(#all_call_args),*)
        }
    }
}

/// Extract parameters from function signature (excluding self)
fn extract_params(sig: &Signature) -> Vec<(syn::Ident, Box<Type>)> {
    sig.inputs
        .iter()
        .filter_map(|arg| {
            if let FnArg::Typed(pat_type) = arg
                && let Pat::Ident(pat_ident) = &*pat_type.pat
            {
                return Some((pat_ident.ident.clone(), pat_type.ty.clone()));
            }
            None
        })
        .collect()
}

/// Extract return type from signature
fn extract_return_type(sig: &Signature) -> proc_macro2::TokenStream {
    match &sig.output {
        ReturnType::Default => quote! { () },
        ReturnType::Type(_, ty) => quote! { #ty },
    }
}

/// Generate parameter extraction code.
///
/// Extraction strategy differs based on parameter count:
/// - **Single parameter**: Extract directly via `args.as_converter()` This is optimized for the
///   common case (one additional argument).
/// - **Multiple parameters**: Use indexed access via `args.idx(n).and_then(|a| a.as_converter())`
///   Allows passing multiple values through `ActionArgs::Multi` variant.
///
/// The distinction exists because ActionArgs optimizes for single arguments with direct
/// accessor methods, but can also hold multiple arguments in a Vec for complex cases.
fn generate_extractions(
    params: &[(syn::Ident, Box<Type>)],
    method_name: &syn::Ident,
) -> Vec<proc_macro2::TokenStream> {
    if params.len() == 1 {
        // Single parameter - direct extraction
        let (name, ty) = &params[0];
        let converter = infer_converter(ty);
        let error_msg = format!("Expected {} for {}", quote! { #ty }, method_name);

        vec![quote! {
            let #name = args.#converter().expect(#error_msg);
        }]
    } else {
        // Multiple parameters - use idx()
        params
            .iter()
            .enumerate()
            .map(|(idx, (name, ty))| {
                let converter = infer_converter(ty);
                let error_msg = format!(
                    "Expected {} at index {} for {}",
                    quote! { #ty },
                    idx,
                    method_name
                );

                quote! {
                    let #name = args.idx(#idx)
                        .and_then(|a| a.#converter())
                        .expect(#error_msg);
                }
            })
            .collect()
    }
}

/// Infer the appropriate ActionArgs converter method based on type.
///
/// This uses a simple string-matching heuristic on the type's token representation
/// rather than full syn parsing to keep macro overhead low. The approach trades
/// generality for compile speed and error clarity.
///
/// **Why string matching instead of full parsing?**
/// - Generic types like `&'a Pos2` need post-parsing normalization anyway
/// - syn::parse_macro_input for types is slower and generates less helpful error messages
/// - Most real-world types (Context, Key, String, Pos2, usize) have unique substrings
///
/// **Fallback behavior**: Unknown types default to `as_context` to minimize broken
/// generated code, but this may fail if the caller passes incompatible ActionArgs.
fn infer_converter(ty: &Type) -> syn::Ident {
    let type_str = quote! { #ty }.to_string();

    // Match common types to ActionArgs converters
    if type_str.contains("Context") {
        syn::Ident::new("as_context", ty.span())
    } else if type_str.contains("Key") {
        syn::Ident::new("as_key", ty.span())
    } else if type_str.contains("str") || type_str.contains("String") {
        syn::Ident::new("as_string", ty.span())
    } else if type_str.contains("usize") {
        syn::Ident::new("as_usize", ty.span())
    } else if type_str.contains("Pos2") {
        syn::Ident::new("as_pos2", ty.span())
    } else {
        // Default fallback - assume context
        syn::Ident::new("as_context", ty.span())
    }
}

/// Proc macro to create actions with different modes
///
/// Modes:
/// - `action!(null <closure|path>)` - Command::Null action
/// - `action!(bool <closure|path>)` - Command::Bool action
/// - `action!(usize <closure|path>)` - Command::Usize action
/// - `action!(string <closure|path>)` - Command::String action
/// - `action!(key <closure|path>)` - Command::Key action
/// - `action!(result <closure|path>)` - Command::Result action
/// - `action!(serial [expr, ...])` - Serial action (chained execution)
/// - `action!(<expr>)` - Default mode, any Execute implementor
///
/// Path syntax:
/// - `Type::method` - Calls `Type::__action_method__(args)`
/// - `function_name` - Calls `__action_function_name__(args)`
///
/// # Examples
///
/// ```ignore
/// // Closure syntax (original)
/// action!(null |app, arg| { app.do_something(); });
///
/// // Method path syntax (new)
/// action!(null MyApp::show_dialog);  // Calls __action_show_dialog__
///
/// // Function path syntax
/// action!(usize select_profile);  // Calls __action_select_profile__
///
/// // Serial execution
/// action!(serial [
///     action!(string MyApp::get_input),
///     action!(MyApp::process_input),
/// ]);
///
/// // Default mode (any Execute implementor)
/// action!(my_custom_command);
/// ```
#[proc_macro]
pub fn action(input: TokenStream) -> TokenStream {
    let action_input = parse_macro_input!(input as ActionInput);

    match action_input {
        ActionInput::TypedClosure { mode, closure } => generate_typed_closure(mode, closure),
        ActionInput::TypedPath { mode, path } => generate_typed_path(mode, path),
        ActionInput::Serial { actions } => generate_serial(actions),
        ActionInput::Default { expr } => generate_default(expr),
    }
}

enum ActionInput {
    TypedClosure {
        mode: ActionMode,
        closure: ExprClosure,
    },
    TypedPath {
        mode: ActionMode,
        path: Path,
    },
    Serial {
        actions: Vec<Expr>,
    },
    Default {
        expr: Expr,
    },
}

impl Parse for ActionInput {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let lookahead = input.lookahead1();

        if lookahead.peek(syn::Ident) {
            let ident: syn::Ident = input.fork().parse()?;
            if ident == "serial" {
                input.parse::<syn::Ident>()?;
                let content;
                syn::bracketed!(content in input);
                let actions = Punctuated::<Expr, Token![,]>::parse_terminated(&content)?;
                return Ok(ActionInput::Serial {
                    actions: actions.into_iter().collect(),
                });
            }

            if let Ok(mode) = ActionMode::try_from(&ident) {
                input.parse::<syn::Ident>()?;

                if input.peek(Token![|]) {
                    let closure: ExprClosure = input.parse()?;
                    return Ok(ActionInput::TypedClosure { mode, closure });
                } else {
                    let path: Path = input.parse()?;
                    return Ok(ActionInput::TypedPath { mode, path });
                }
            }
        }

        let expr: Expr = input.parse()?;
        Ok(ActionInput::Default { expr })
    }
}

#[derive(Clone, Copy)]
enum ActionMode {
    Null,
    Bool,
    Usize,
    String,
    Key,
    Result,
}

impl ActionMode {
    fn try_from(ident: &syn::Ident) -> syn::Result<Self> {
        match ident.to_string().as_str() {
            "null" => Ok(ActionMode::Null),
            "bool" => Ok(ActionMode::Bool),
            "usize" => Ok(ActionMode::Usize),
            "string" => Ok(ActionMode::String),
            "key" => Ok(ActionMode::Key),
            "result" => Ok(ActionMode::Result),
            _ => Err(syn::Error::new(
                ident.span(),
                format!("Unknown action mode: {}", ident),
            )),
        }
    }

    fn command_variant(&self) -> syn::Ident {
        match self {
            ActionMode::Null => syn::Ident::new("Null", proc_macro2::Span::call_site()),
            ActionMode::Bool => syn::Ident::new("Bool", proc_macro2::Span::call_site()),
            ActionMode::Usize => syn::Ident::new("Usize", proc_macro2::Span::call_site()),
            ActionMode::String => syn::Ident::new("String", proc_macro2::Span::call_site()),
            ActionMode::Key => syn::Ident::new("Key", proc_macro2::Span::call_site()),
            ActionMode::Result => syn::Ident::new("Result", proc_macro2::Span::call_site()),
        }
    }

    /// Returns the expected return type for each action mode's callback.
    /// This matches the `Callback<App, T>` type parameter in each `Command` variant.
    fn return_type(&self) -> proc_macro2::TokenStream {
        match self {
            ActionMode::Null => quote! { () },
            ActionMode::Bool => quote! { bool },
            ActionMode::Usize => quote! { Option<usize> },
            ActionMode::String => quote! { Option<String> },
            ActionMode::Key => quote! { Option<egui::Key> },
            ActionMode::Result => quote! { egui_action::Result<egui_action::ActionResult> },
        }
    }
}

/// Generate a Command wrapping a closure (original syntax).
/// The closure is passed directly without wrapper name resolution.
fn generate_typed_closure(mode: ActionMode, closure: ExprClosure) -> TokenStream {
    let variant = mode.command_variant();
    let return_type = mode.return_type();

    // Extract the type of the first parameter from the closure
    // Closure syntax: |app: &mut SAideApp, args: &ActionArgs| { ... }
    // We need to extract `SAideApp` from the first parameter's type annotation
    let first_param_type = closure.inputs.first().and_then(|pat| {
        if let Pat::Type(pat_type) = pat {
            Some(&pat_type.ty)
        } else {
            None
        }
    });

    if let Some(app_type) = first_param_type {
        // Extract the inner type from &mut T
        let inner_type = if let Type::Reference(type_ref) = &**app_type {
            &type_ref.elem
        } else {
            app_type
        };

        // Generate a shim function with explicit types, similar to generate_typed_path
        TokenStream::from(quote! {
            {
                #[allow(unused_imports)]
                use egui_action::{ActionArgs, Action, Command};

                fn __egui_action_closure_cb(app: &mut #inner_type, args: &ActionArgs) -> #return_type {
                    (#closure)(app, args)
                }

                Action::from(Command::#variant(Box::new(__egui_action_closure_cb)))
            }
        })
    } else {
        // Fallback: if we can't extract the type, return a compile error
        TokenStream::from(quote! {
            compile_error!("action! closure requires explicit type annotation on first parameter: |app: &mut YourType, args| {{ ... }}");
        })
    }
}

fn generate_typed_path(mode: ActionMode, path: Path) -> TokenStream {
    let variant = mode.command_variant();
    let return_type = mode.return_type();

    let last_segment = path
        .segments
        .last()
        .expect("Path must have at least one segment");
    let method_name = &last_segment.ident;
    let wrapper_name = syn::Ident::new(&format!("__action_{}__", method_name), method_name.span());

    if path.segments.len() > 1 {
        let ty_segments: Vec<_> = path.segments.iter().take(path.segments.len() - 1).collect();
        let ty_path = quote! { #(#ty_segments)::* };

        TokenStream::from(quote! {
            {
                #[allow(unused_imports)]
                use egui_action::{ActionArgs, Action, Command};

                fn __egui_action_cb(app: &mut #ty_path, args: &ActionArgs) -> #return_type {
                    app.#wrapper_name(args)
                }

                Action::from(Command::#variant(Box::new(__egui_action_cb)))
            }
        })
    } else {
        TokenStream::from(quote! {
            {
                #[allow(unused_imports)]
                use egui_action::{ActionArgs, Action, Command};
                Action::from(Command::#variant(Box::new(#wrapper_name)))
            }
        })
    }
}

fn generate_serial(actions: Vec<Expr>) -> TokenStream {
    TokenStream::from(quote! {
        {
            #[allow(unused_imports)]
            use egui_action::{Action, IntoAction};
            Action::from(vec![#(#actions),*])
        }
    })
}

fn generate_default(expr: Expr) -> TokenStream {
    TokenStream::from(quote! {
        {
            #[allow(unused_imports)]
            use egui_action::IntoAction;
            IntoAction::into_action(#expr)
        }
    })
}
