use egui_action_macro::impl_action;

enum ActionArgs<'a> {
    None,
    Usize(usize),
    String(&'a str),
}

impl<'a> ActionArgs<'a> {
    fn as_usize(&self) -> Option<usize> {
        match self {
            ActionArgs::Usize(u) => Some(*u),
            _ => Option::None,
        }
    }

    fn as_string(&self) -> Option<&str> {
        match self {
            ActionArgs::String(s) => Some(s),
            _ => Option::None,
        }
    }

    #[allow(dead_code)]
    fn idx(&self, _index: usize) -> Option<&ActionArgs<'a>> { Option::None }
}

#[test]
fn test_wrapper_generation() {
    struct TestApp {
        value: usize,
    }

    #[impl_action]
    impl TestApp {
        pub fn set_value(&mut self, val: usize) { self.value = val; }
    }

    let mut app = TestApp { value: 0 };
    let args = ActionArgs::Usize(42);
    app.__action_set_value__(&args);
    assert_eq!(app.value, 42);
}

#[test]
fn test_no_params_wrapper() {
    struct TestApp {
        called: bool,
    }

    #[impl_action]
    impl TestApp {
        pub fn trigger(&mut self) { self.called = true; }
    }

    let mut app = TestApp { called: false };
    let args = ActionArgs::None;
    app.__action_trigger__(&args);
    assert!(app.called);
}

#[test]
fn test_string_param_wrapper() {
    struct TestApp {
        name: String,
    }

    #[impl_action]
    impl TestApp {
        pub fn set_name(&mut self, name: &str) { self.name = name.to_string(); }
    }

    let mut app = TestApp {
        name: String::new(),
    };
    let args = ActionArgs::String("test");
    app.__action_set_name__(&args);
    assert_eq!(app.name, "test");
}

#[test]
fn test_standalone_function_wrapper() {
    struct TestApp {
        counter: usize,
    }

    #[impl_action]
    pub fn increment_counter(app: &mut TestApp) { app.counter += 1; }

    let mut app = TestApp { counter: 0 };
    let args = ActionArgs::None;
    __action_increment_counter__(&mut app, &args);
    assert_eq!(app.counter, 1);
}

#[test]
fn test_private_methods_not_wrapped() {
    struct TestApp;

    #[impl_action]
    impl TestApp {
        pub fn public(&mut self) {}

        fn _private(&mut self) {}
    }

    let mut app = TestApp;
    let args = ActionArgs::None;
    app.__action_public__(&args);
}

#[test]
fn test_return_value_preserved() {
    struct TestApp;

    #[impl_action]
    impl TestApp {
        pub fn get_number(&self) -> usize { 42 }
    }

    let mut app = TestApp;
    let args = ActionArgs::None;
    assert_eq!(app.__action_get_number__(&args), 42);
}
