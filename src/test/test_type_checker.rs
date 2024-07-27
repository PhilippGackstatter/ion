#[cfg(test)]
mod tests {
    use crate::{
        lexer::Lexer,
        parser::Parser,
        types::{
            CompilationErrorKind, CompileError, Moved, TraitMethodImplIncorrect,
            TraitMethodImplMissing, TypeAlreadyExists,
        },
        util,
    };
    use std::path::Path;

    fn lex_parse_check(path: &str) -> Result<(), CompileError> {
        let test_path: &Path = "src/test_input/type_checker/".as_ref();
        let input = util::file_to_string(&test_path.join(path)).unwrap();
        let mut lexer = Lexer::new();
        lexer.lex(&input);
        let mut parser = Parser::new(&lexer);
        let tree = parser.parse().unwrap();
        let mut checker = crate::type_checker::TypeChecker::new();
        let check_result = checker.check(tree, false);

        if let Err(err) = &check_result {
            util::print_error(&input, err.clone());
        }

        check_result
    }

    #[test]
    fn test_no_return() {
        let res = lex_parse_check("no_return.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .unwrap_migration()
            .message
            .contains("does not match declared return type"));
    }

    #[test]
    fn test_multiple_returns() {
        let res = lex_parse_check("multiple_returns.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .unwrap_migration()
            .message
            .contains("does not match declared return type"));
    }

    #[test]
    fn test_return_type_void() {
        let res = lex_parse_check("return_type_void.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .unwrap_migration()
            .message
            .contains("does not match declared return type"));
    }

    #[test]
    fn test_empty_return() {
        let res = lex_parse_check("empty_return.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .unwrap_migration()
            .message
            .contains("does not match declared return type"));
    }

    #[test]
    fn test_fn_arg() {
        let res = lex_parse_check("fn_arg.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .unwrap_migration()
            .message
            .contains("Parameter has incompatible type"));
    }

    #[test]
    fn test_if_condition() {
        let res = lex_parse_check("if_condition.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .unwrap_migration()
            .message
            .contains("Condition must be of type bool"));
    }

    #[test]
    fn test_local_binary_plus() {
        let res = lex_parse_check("local_binary_plus.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .unwrap_migration()
            .message
            .contains("Condition must be of type bool"));
    }

    #[test]
    fn test_local_binary_greater() {
        let res = lex_parse_check("local_binary_greater.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .unwrap_migration()
            .message
            .contains("not compatible in binary operation"));
    }

    #[test]
    fn test_local_unary_bang() {
        let res = lex_parse_check("local_unary_bang.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .unwrap_migration()
            .message
            .contains("can not be used with a ! operator"));
    }

    #[test]
    fn test_redeclare_struct() {
        let res = lex_parse_check("redeclare_struct.io");
        let err = res.unwrap_err();
        assert!(matches!(
            err.kind,
            CompilationErrorKind::TypeAlreadyExists(TypeAlreadyExists {
                duplicate_name,
                ..
            }) if duplicate_name.as_str() == "Flag"
        ));
    }

    #[test]
    fn test_struct_init_too_many_fields() {
        let res = lex_parse_check("struct_init_too_many.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .unwrap_migration()
            .message
            .contains("Expected 2 fields, but 3 were given"));
    }

    #[test]
    fn test_struct_init_wrong_field_name() {
        let res = lex_parse_check("struct_init_wrong_field_name.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .unwrap_migration()
            .message
            .contains("has no field with name"));
    }

    #[test]
    fn test_struct_init_wrong_type() {
        let res = lex_parse_check("struct_init_wrong_type.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .unwrap_migration()
            .message
            .contains("but type i32 was found"));
    }

    #[test]
    fn test_struct_access_use() {
        let res = lex_parse_check("struct_access_use.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .unwrap_migration()
            .message
            .contains("Type i32 can not be used with a ! operator"));
    }

    #[test]
    fn test_struct_access_assign() {
        let res = lex_parse_check("struct_access_assign.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .unwrap_migration()
            .message
            .contains("Expression of type str can not be assigned to variable of type i32"));
    }

    #[test]
    fn test_struct_access_impl() {
        let res = lex_parse_check("struct_access_impl.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .unwrap_migration()
            .message
            .contains("Type NumWrap not declared in this scope."));
    }

    #[test]
    fn test_trait_impl_method_missing() {
        let res = lex_parse_check("trait_impl_method_missing.io");
        let err = res.unwrap_err();
        assert!(matches!(
            err.kind,
            CompilationErrorKind::TraitMethodImplMissing(TraitMethodImplMissing {
                missing_methods,
                ..
            }) if missing_methods.as_slice() == &["fmt"]
        ));
    }

    #[test]
    fn test_trait_impl_type_mismatch() {
        let res = lex_parse_check("trait_impl_type_mismatch.io");
        let err = res.unwrap_err();
        assert!(matches!(
            err.kind,
            CompilationErrorKind::TraitMethodImplIncorrect(TraitMethodImplIncorrect {
                incorrect_method,
                ..
            }) if incorrect_method.as_str() == "fmt"
        ));
    }

    #[test]
    fn trait_implementor_struct_passed_as_trait() {
        let res = lex_parse_check("trait_implementor_struct_passed_as_trait.io");
        assert!(res.is_ok());
    }

    #[test]
    fn non_trait_implementor_struct_passed_as_trait() {
        let res = lex_parse_check("non_trait_implementor_struct_passed_as_trait.io");
        assert!(res.is_err());
        assert!(res
            .unwrap_err()
            .unwrap_migration()
            .message
            .contains("does not implement trait"));
    }

    #[test]
    fn move_with_variable_declaration() {
        let res = lex_parse_check("move_with_variable_declaration.io");
        assert!(matches!(
            res.unwrap_err().kind,
            CompilationErrorKind::Moved(Moved {
                moved_identifier,
                moved_into,
                ..
            }) if moved_identifier == "x" && moved_into == "y"
        ));
    }

    #[test]
    fn move_with_assignment() {
        let res = lex_parse_check("move_with_assignment.io");
        assert!(matches!(
            res.unwrap_err().kind,
            CompilationErrorKind::Moved(Moved {
                moved_identifier,
                moved_into,
                ..
            }) if moved_identifier == "x" && moved_into == "y"
        ));
    }

    #[test]
    fn move_with_struct_init() {
        let res = lex_parse_check("move_with_struct_init.io");
        assert!(matches!(
            res.unwrap_err().kind,
            CompilationErrorKind::Moved(Moved {
                moved_identifier,
                moved_into,
                ..
            }) if moved_identifier == "x" && moved_into == "num"
        ));
    }

    #[test]
    fn move_with_function_call() {
        let res = lex_parse_check("move_with_function_call.io");
        assert!(matches!(
            res.unwrap_err().kind,
            CompilationErrorKind::Moved(Moved {
                moved_identifier,
                moved_into,
                ..
            }) if moved_identifier == "x" && moved_into == "addOne"
        ));
    }

    #[test]
    fn move_struct_partially() {
        let res = lex_parse_check("move_struct_partially.io");
        assert!(matches!(
            res.unwrap_err().kind,
            CompilationErrorKind::Moved(Moved {
                moved_identifier,
                moved_into,
                ..
            }) if moved_identifier == "book" && moved_into == "y"
        ));
    }
}
