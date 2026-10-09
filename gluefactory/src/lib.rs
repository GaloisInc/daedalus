extern crate proc_macro;
use proc_macro2::Ident;
use proc_macro2::TokenStream as TokenStream2;
use proc_macro::TokenStream;
use quote::ToTokens;
use quote::{format_ident, quote};
use syn::{parse_macro_input, parse_quote, Data, DeriveInput, Fields, PathArguments, GenericArgument, Lit, Expr, Type, Token, TypePtr};
use syn::punctuated::Punctuated;
use proc_lock::{lock, LockPath};
use std::fs::OpenOptions;
use std::io::Write;
use std::path::PathBuf;

enum PrimType {
    Signed(usize),
    Unsigned(usize),
}

fn append_to_c_header(txt: &String, filename: &str) {
    let base_dir = std::env::var("C_HEADER_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|_| std::env::current_dir().unwrap());
    
    let target_header = base_dir.join(filename);
    
    let lock_path = LockPath::Tmp("c_header_generation.lock");
    let _guard = lock(&lock_path).expect("Failed to secure file system lock");

    let mut file = OpenOptions::new()
        .create(true)
        .append(true)
        .open(&target_header)
        .expect("Failed to open or create C header file");

    writeln!(file,"{}",txt).expect("Failed to write function declaration");
}

fn is_c_prim(txt: &String) -> bool {
    match txt.as_str() {
	"u8" => true,
	"u16" => true,
	"u32" => true,
	"u64" => true,
	"i8" => true,
	"i16" => true,
	"i32" => true,
	"i64" => true,
	_ => false,
    }
}

fn to_c_prim(txt: &String) -> String {
    match txt.as_str() {
	"u8" => "unsigned char".to_owned(),
	"u16" => "unsigned short".to_owned(),
	"u32" => "unsigned int".to_owned(),
	"u64" => "unsigned long".to_owned(),
	"i8" => "char".to_owned(),
	"i16" => "short".to_owned(),
	"i32" => "int".to_owned(),
	"i64" => "long".to_owned(),
	_ => format!("{}*",txt).to_owned(),
    }
}

fn as_type_string(ty : &Type) -> Option<String> {
    match ty {
	Type::Path(path) => {
	    if let Some(final_seg) = path.path.segments.last() {
		let ident_str = final_seg.ident.to_string();
		if ident_str == "Array" {
		    if let PathArguments::AngleBracketed(args) = &final_seg.arguments {
			if let Some(GenericArgument::Type(subtype)) = args.args.first() {
			    if let Some(subtype_str) = as_type_string(&subtype) {
				return Some(format!("Arr_{}",subtype_str));
			    } else {
				return None;//Some(format!("{}",ty.to_token_stream()));
			    }
			}
	            }
		} else if ident_str == "U" {
		    if let PathArguments::AngleBracketed(args) = &final_seg.arguments {
			if let Some(GenericArgument::Const(Expr::Lit(lit_expr))) = args.args.first() {
			    if let Lit::Int(int_lit) = &lit_expr.lit {
				let value : usize = int_lit.base10_parse().unwrap();
				if value <= 8 {       return Some( "u8".to_owned()); }
				else if value <= 16 { return Some("u16".to_owned()); }
				else if value <= 32 { return Some("u32".to_owned()); }
				else if value <= 64 { return Some("u64".to_owned()); }
				else { panic!("only support uint width up to 64 bits"); }
			    }
			}
		    }
		} else if ident_str == "I" {
		    if let PathArguments::AngleBracketed(args) = &final_seg.arguments {
			if let Some(GenericArgument::Const(Expr::Lit(lit_expr))) = args.args.first() {
			    if let Lit::Int(int_lit) = &lit_expr.lit {
				let value : usize = int_lit.base10_parse().unwrap();
				if value <= 8 {       return Some("i8".to_owned()); }
				else if value <= 16 { return Some("i16".to_owned()); }
				else if value <= 32 { return Some("i32".to_owned()); }
				else if value <= 64 { return Some("i64".to_owned()); }
				else { panic!("only support int width up to 64 bits"); }
			    }
			}
		    }	    
		} else {
		    return Some(ident_str);
		}
	    }
	    return None;
	},
	Type::Ptr(TypePtr {const_token: _, star_token: _, mutability: _, elem: subtype}) => {
	    return as_type_string(subtype);
	},
	_ => { return None }
    };
    return None
}

fn as_primitive_type(ty : &Type) -> Option<PrimType> {
    if let Type::Path(path) = ty {
	if let Some(final_seg) = path.path.segments.last() {
	    let ident_str = final_seg.ident.to_string();
	    if ident_str == "U" {
		if let PathArguments::AngleBracketed(args) = &final_seg.arguments {
		    if let Some(GenericArgument::Const(Expr::Lit(lit_expr))) = args.args.first() {
			if let Lit::Int(int_lit) = &lit_expr.lit {
			    let value : usize = int_lit.base10_parse().unwrap();
			    if value <= 8 {       return Some(PrimType::Unsigned(8)); }
			    else if value <= 16 { return Some(PrimType::Unsigned(16)); }
			    else if value <= 32 { return Some(PrimType::Unsigned(32)); }
			    else if value <= 64 { return Some(PrimType::Unsigned(64)); }
			    else { panic!("only support uint width up to 64 bits"); }
			}
		    }
		}
	    } else if ident_str == "I" {
		if let PathArguments::AngleBracketed(args) = &final_seg.arguments {
		    if let Some(GenericArgument::Const(Expr::Lit(lit_expr))) = args.args.first() {
			if let Lit::Int(int_lit) = &lit_expr.lit {
			    let value : usize = int_lit.base10_parse().unwrap();
			    if value <= 8 {       return Some(PrimType::Signed(8)); }
			    else if value <= 16 { return Some(PrimType::Signed(16)); }
			    else if value <= 32 { return Some(PrimType::Signed(32)); }
			    else if value <= 64 { return Some(PrimType::Signed(64)); }
			    else { panic!("only support int width up to 64 bits"); }
			}
		    }
		}
		
	    }/* else if ident_str == "Array" {
		    if let PathArguments::AngleBracketed(args) = &final_seg.arguments {
		      if let Some(GenericArgument::Type(subtype)) = args.args.first() {
		        return_type = subtype.clone();
		      }
	            }
	        }*/
	}
    }
    None
}

#[proc_macro_derive(Accessors)]
pub fn derive_accessors(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let struct_name = input.ident;
    let fields = match input.data {
	Data::Struct(data) => match data.fields {
	    Fields::Named(fields) => fields.named,
	    _ => panic!("expected named fields only"),
	}
	_ => panic!("can only make accessors for struct types"),
    };

    // let accessors = fields.iter().map(|f| {
    // 	let name = f.ident.as_ref().unwrap();
    // 	let ty = &f.ty;
    // 	let getter_name = format_ident!("safe_get_{}", name);
    // 	quote! {
    // 	    pub fn #getter_name(&self) -> &#ty {
    //           &self.#name
    //         }
    // 	}
    // });
    append_to_c_header(&format!("typedef struct {} {};", struct_name, struct_name),"parser_types.h");
    let c_accessors = fields.iter().map(|f| {
	let name = f.ident.as_ref().unwrap();
	let getter_name = format_ident!("get_{}_{}", struct_name, name);
	let ty = &f.ty;
	let mut return_type: Type = parse_quote! { *const #ty };
	let mut null_val = quote! { std::ptr::null() };
	let mut is_prim = false;
	match as_primitive_type(ty) {
	    Some(PrimType::Unsigned(sz)) => {
		let type_name = format_ident!("u{}",sz);
		return_type = parse_quote! { #type_name };
		is_prim = true;
	    },
	    Some(PrimType::Signed(sz)) => {
		let type_name = format_ident!("i{}",sz);
		return_type = parse_quote! { #type_name };
		is_prim = true;
	    },
	    None => {}
	}
	let epilogue = if is_prim {
	    null_val = quote! { 0 };
	    quote! {
		#return_type::from(o.#name)
	    }
	} else {
	    quote! {
		&o.#name as #return_type
	    }
	};
	let ret_type = as_type_string(&return_type).unwrap();
	if ! is_c_prim(&ret_type) {
	    append_to_c_header(&format!("typedef struct {} {};", ret_type, ret_type),"parser_types.h"); 
	}
	append_to_c_header(&format!("extern {} {}({}* obj, int* error);", to_c_prim(&ret_type), getter_name, struct_name.to_string()),"parser_bindings.h");
	quote! {
	    #[unsafe(no_mangle)]
	    pub fn #getter_name(obj: *const #struct_name, err: *mut c_int) -> #return_type {
		if obj.is_null() {
		    unsafe {
			set_error(err, FfiError::InvalidPtr);
		    }
		    return #null_val;
		}
		unsafe {
		    let o = &*obj;
		    set_error(err, FfiError::Ok);
		    #epilogue//&o.#name// as #return_type
		}
            }
	}
    });

    
    let code = quote! {
	//impl #struct_name {
	//    #(#accessors)*
	//}
	#(#c_accessors)*
    };

    TokenStream::from(code)
}

#[proc_macro_derive(Variants)]
pub fn derive_variants(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let enum_name = input.ident;
    let variants = match input.data {
	Data::Enum(data) => data.variants,
	_ => panic!("can only make variant checks for enum types"),
    };

    append_to_c_header(&format!("typedef struct {} {};", enum_name, enum_name),"parser_types.h");
    let c_methods = variants.iter().map(|v| {
	let name = &v.ident;
	let checker_name = format_ident!("{}_is_{}", enum_name, name);
	let getter_name = format_ident!("{}_as_{}", enum_name, name);
	let doc_string = format!("extern int {}({}* obj, int* error);", checker_name, enum_name.to_string());
	append_to_c_header(&doc_string,"parser_bindings.h");
	let is_method = quote! {
	    #[unsafe(no_mangle)]
	    pub unsafe extern "C" fn #checker_name(obj: *const #enum_name, err: *mut c_int) -> c_int {
		if obj.is_null() {
		    unsafe {
			set_error(err, FfiError::InvalidPtr);
		    }
		    return 0;
		}
		unsafe {
		    let o = &*obj;
		    if matches!(o, #enum_name::#name { .. }) {
			return 1;
		    }
		    return 0;
		}
            }
	};
	let as_method = match(v.fields) {
	    Fields::Unnamed(ref fields) => {
		let bindings: Vec<_> = (0..fields.unnamed.len()).map(|idx| quote::format_ident!("field_{}", idx)).collect();
		let getters = fields.unnamed.iter().enumerate().map(|(idx,f)| {
		    
		    let target_field = &bindings[idx];
		    let getter_name = if bindings.len() == 0 {
			format_ident!("{}_as_{}", enum_name, name)
		    } else {
			format_ident!("{}_as_{}_get_{}", enum_name, name, idx)
		    };
		    let ty = &f.ty;
		    let mut return_type: Type = parse_quote! { *const #ty };
		    let mut null_val = quote! { std::ptr::null() };
		    let mut is_prim = false;
		    match as_primitive_type(ty) {
			Some(PrimType::Unsigned(sz)) => {
			    let type_name = format_ident!("u{}",sz);
			    return_type = parse_quote! { #type_name };
			    is_prim = true;
			},
			Some(PrimType::Signed(sz)) => {
			    let type_name = format_ident!("i{}",sz);
			    return_type = parse_quote! { #type_name };
			    is_prim = true;
			},
			None => {}
		    }
		    let epilogue = if is_prim {
			null_val = quote! { 0 };
			quote! {
			    return #return_type::from(*#target_field);
			}
		    } else {
			quote! {
			    return #target_field as #return_type;
			}
		    };

		    let c_ret_type = as_type_string(&return_type).unwrap();
		    if ! is_c_prim(&c_ret_type) {
			append_to_c_header(&format!("typedef struct {} {};",c_ret_type,c_ret_type),"parser_types.h");
		    }
		    append_to_c_header(&format!("extern {} {}({}* obj, int* error);", to_c_prim(&c_ret_type), getter_name, enum_name.to_string()),"parser_bindings.h");
		    quote! {
			#[unsafe(no_mangle)]
			pub unsafe extern "C" fn #getter_name(obj: *const #enum_name, err: *mut c_int) -> #return_type {
			    if obj.is_null() {
				unsafe {
				    set_error(err, FfiError::InvalidPtr);
				}
				return #null_val;
			    }
			    unsafe {
				let o = &*obj;
				if let #enum_name::#name( #( #bindings ),* ) = o {
				    set_error(err, FfiError::Ok);
				    #epilogue
				}
				set_error(err, FfiError::InvalidIndex);
				return #null_val;
			    }
			}
		    }
		});
		quote!{
		    #(#getters)*
		}
	    },
	    Fields::Named(ref fields) => {
		quote!{}
	    },
	    _ => quote!{},
	};
	
	quote! {
	    #is_method
	    #as_method
	}
    });
    
    let code = quote! {
	#(#c_methods)*
    };

    TokenStream::from(code)
}

#[proc_macro]
pub fn make_parser_main(input: TokenStream) -> TokenStream {
    let mut idents = parse_macro_input!(input with Punctuated::<Ident, Token![,]>::parse_terminated);
    if idents.len() != 2 {
	panic!("make_parser_main requires two arguments: struct_name, and parser_main_function");
    }
    let parser_fn = idents.pop().unwrap().into_value();
    let struct_name = idents.pop().unwrap().into_value();
    let parser_name = format_ident!("parse_{}", struct_name);
    let main_getter_name = format_ident!("get_{}", struct_name);
    let parsed_struct_name = format_ident!("Parsed{}", struct_name);
    append_to_c_header(&format!("typedef struct {} {};", parsed_struct_name, parsed_struct_name),"parser_types.h");
    append_to_c_header(&format!("typedef struct {} {};", struct_name, struct_name),"parser_types.h");
    append_to_c_header(&format!("extern {}* {}({}* parsed);", struct_name, main_getter_name, parsed_struct_name),"parser_bindings.h");
    append_to_c_header(&format!("extern {}* {}(char* buf, int len, int* error);", parsed_struct_name, parser_name),"parser_bindings.h");
    quote! {
	pub struct #parsed_struct_name {
	    buffer: Vec<u8>,
	    main: #struct_name,
	}

	#[unsafe(no_mangle)]
	pub unsafe extern "C" fn #main_getter_name(p: *const #parsed_struct_name) -> *const #struct_name {
	    let ps = &*p;
	    return &ps.main;
	}
	
	#[unsafe(no_mangle)]
	pub unsafe extern "C" fn #parser_name(buf: *const c_char, len: c_int, err: *mut c_int) -> *mut #parsed_struct_name {
	    if buf.is_null() {
		set_error(err, FfiError::InvalidPtr);
		return std::ptr::null_mut();
	    }
	    let byte_slice = slice::from_raw_parts(buf as *const u8, len as usize);
	    let byte_arr = ddl::new_byte_array(&byte_slice);
	    let name = "input";
	    let name_arr = ddl::new_byte_array(name.as_bytes());

	    let mut pstate = ddl::new_parser_state();

	    match #parser_fn(&mut pstate, ddl::new_input(name_arr, byte_arr)) {
		ddl::ParserResult::Failure => {
		    //ddl::print_json(&pstate.error, false);
		    set_error(err, FfiError::ParseError);
		    return std::ptr::null_mut();
		},
		ddl::ParserResult::Exception => {
		    //ddl::print_json(&pstate.error, false);
		    set_error(err, FfiError::ParseError);
		    return std::ptr::null_mut();
		},
		ddl::ParserResult::Ok(a, _) => {
		    // print_json(&a, true);
		    
		    let res = #parsed_struct_name {
			buffer: byte_slice.to_vec(),
			main: a, 
		    };
		    set_error(err, FfiError::Ok);
		    return Box::into_raw(Box::new(res));
		},
	    }

	}
    }.into()
}

#[proc_macro]
pub fn array_accessors(input: TokenStream) -> TokenStream {
    let ty = parse_macro_input!(input as Type);
    let mut return_type: Type = parse_quote! { *const #ty };
    let mut null_val = quote! { std::ptr::null() };
    let mut is_prim = false;
    match as_primitive_type(&ty) {
	Some(PrimType::Unsigned(sz)) => {
	    let type_name = format_ident!("u{}",sz);
	    return_type = parse_quote! { #type_name };
	    is_prim = true;
	},
	Some(PrimType::Signed(sz)) => {
	    let type_name = format_ident!("i{}",sz);
	    return_type = parse_quote! { #type_name };
	    is_prim = true;
	},
	None => {}
    }
    let arr_type_ident = match is_prim {
	true => {
	    if let Type::Path(ref p) = return_type {
		&p.path.segments.last().unwrap().ident
	    } else {
		panic!("unsupported sort of type");
	    }
	},
	false => {
	    if let Type::Path(ref p) = ty {
		&p.path.segments.last().unwrap().ident
	    } else {
		panic!("unsupported sort of type");
	    }
	},
    };
    let get_item_name = format_ident!("get_arr_{}_item",arr_type_ident);
    let get_len_name = format_ident!("len_arr_{}",arr_type_ident);
    let epilogue = match is_prim {
	true => {
	    null_val = quote! { 0 };
	    quote! { return #return_type::from(a[i as usize]); }
	},
	false => {
	    quote! { return &a[i as usize] as #return_type; }
	},
    };
    let ret_type = as_type_string(&return_type).unwrap();
    let elem_type = as_type_string(&ty).unwrap();
    let get_doc_string = format!("extern {} {}(Arr_{}* arr, int idx, int* error);", to_c_prim(&ret_type), get_item_name, elem_type);
    let len_doc_string = format!("extern int {}(Arr_{}* arr, int* error);", get_len_name, elem_type);
    append_to_c_header(&format!("typedef struct Arr_{} Arr_{};", elem_type, elem_type),"parser_types.h");
    append_to_c_header(&get_doc_string,"parser_bindings.h");
    append_to_c_header(&len_doc_string,"parser_bindings.h");
    quote! {
	//type #arr_type_name = ddl::Array<#struct_name>;
	#[unsafe(no_mangle)]
	pub unsafe extern "C" fn #get_item_name(arr: *const ddl::Array<#ty>, i: c_int, err: *mut c_int) -> #return_type {
	    if arr.is_null() {
		set_error(err, FfiError::InvalidPtr);
		return #null_val;
	    }
	    unsafe {
		let a = &*arr;
		if i as usize >= a.len() {
		    set_error(err, FfiError::InvalidIndex);
		    return #null_val;
		}
		set_error(err, FfiError::Ok);
		#epilogue
	    }
	}

	#[unsafe(no_mangle)]
	pub unsafe extern "C" fn #get_len_name(arr: *const ddl::Array<#ty>, err: *mut c_int) -> c_int {
	    if arr.is_null() {
		set_error(err, FfiError::InvalidPtr);
		return -1;
	    }
	    unsafe {
		let a = &*arr;
		set_error(err, FfiError::Ok);
		return a.len() as i32;
	    }
	}
    }.into()
    
}
