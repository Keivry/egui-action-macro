use egui_action_macro::impl_action;

// Mock types for testing
#[derive(Debug, PartialEq)]
enum ActionArgs<'a> {
    None,
    Usize(usize),
    String(&'a str),
    Multi(Vec<ActionArgs<'a>>),
}

impl<'a> ActionArgs<'a> {
    fn as_usize(&self) -> Option<usize> {
        match self {
            ActionArgs::Usize(u) => Some(*u),
            _ => None,
        }
    }

    fn as_string(&self) -> Option<&str> {
        match self {
            ActionArgs::String(s) => Some(s),
            _ => None,
        }
    }

    #[allow(dead_code)]
    fn idx(&self, index: usize) -> Option<&ActionArgs<'a>> {
        match self {
            ActionArgs::Multi(v) => v.get(index),
            _ => None,
        }
    }
}

impl<'a> From<usize> for ActionArgs<'a> {
    fn from(u: usize) -> Self { ActionArgs::Usize(u) }
}

impl<'a> From<&'a str> for ActionArgs<'a> {
    fn from(s: &'a str) -> Self { ActionArgs::String(s) }
}

// Test struct for impl block tests
struct TestApp {
    volume: usize,
    name: String,
}

#[impl_action]
impl TestApp {
    pub fn set_volume(&mut self, volume: usize) { self.volume = volume; }

    pub fn set_name(&mut self, name: &str) { self.name = name.to_string(); }

    pub fn set_both(&mut self, volume: usize, name: &str) {
        self.volume = volume;
        self.name = name.to_string();
    }

    pub fn get_volume(&mut self) -> usize { self.volume }

    // Private method should not get wrapper
    #[allow(dead_code)]
    fn internal_method(&mut self, _x: usize) {}
}

// Standalone function tests
#[allow(private_interfaces)]
#[impl_action]
pub fn process_volume(app: &mut TestApp, volume: usize) { app.volume = volume; }

#[allow(private_interfaces)]
#[impl_action]
pub fn process_name(app: &mut TestApp, name: &str) { app.name = name.to_string(); }

#[allow(private_interfaces)]
#[impl_action]
pub fn process_both(app: &mut TestApp, volume: usize, name: &str) {
    app.volume = volume;
    app.name = name.to_string();
}

#[allow(private_interfaces)]
#[impl_action]
pub fn get_app_volume(app: &TestApp) -> usize { app.volume }

// ========== Tests for impl block wrappers ==========

#[test]
fn test_single_param_wrapper() {
    let mut app = TestApp {
        volume: 0,
        name: String::new(),
    };

    let args = ActionArgs::Usize(75);
    app.__action_set_volume__(&args);
    assert_eq!(app.volume, 75);
}

#[test]
fn test_string_param_wrapper() {
    let mut app = TestApp {
        volume: 0,
        name: String::new(),
    };

    let args = ActionArgs::String("test_name");
    app.__action_set_name__(&args);
    assert_eq!(app.name, "test_name");
}

#[test]
fn test_multi_param_wrapper() {
    let mut app = TestApp {
        volume: 0,
        name: String::new(),
    };

    let args = ActionArgs::Multi(vec![
        ActionArgs::Usize(100),
        ActionArgs::String("multi_test"),
    ]);

    app.__action_set_both__(&args);
    assert_eq!(app.volume, 100);
    assert_eq!(app.name, "multi_test");
}

#[test]
fn test_no_param_wrapper() {
    let mut app = TestApp {
        volume: 42,
        name: String::new(),
    };

    let args = ActionArgs::None;
    let result = app.__action_get_volume__(&args);
    assert_eq!(result, 42);
}

#[test]
fn test_original_methods_unchanged() {
    let mut app = TestApp {
        volume: 0,
        name: String::new(),
    };

    app.set_volume(50);
    assert_eq!(app.volume, 50);

    app.set_name("original");
    assert_eq!(app.name, "original");

    app.set_both(99, "both");
    assert_eq!(app.volume, 99);
    assert_eq!(app.name, "both");
}

// ========== Tests for standalone function wrappers ==========

#[test]
fn test_fn_single_param_wrapper() {
    let mut app = TestApp {
        volume: 0,
        name: String::new(),
    };

    // Original function still works
    process_volume(&mut app, 50);
    assert_eq!(app.volume, 50);

    // Wrapper function works
    let args = ActionArgs::Usize(75);
    __action_process_volume__(&mut app, &args);
    assert_eq!(app.volume, 75);
}

#[test]
fn test_fn_string_param_wrapper() {
    let mut app = TestApp {
        volume: 0,
        name: String::new(),
    };

    process_name(&mut app, "original");
    assert_eq!(app.name, "original");

    let args = ActionArgs::String("wrapper_name");
    __action_process_name__(&mut app, &args);
    assert_eq!(app.name, "wrapper_name");
}

#[test]
fn test_fn_multi_param_wrapper() {
    let mut app = TestApp {
        volume: 0,
        name: String::new(),
    };

    process_both(&mut app, 50, "original");
    assert_eq!(app.volume, 50);
    assert_eq!(app.name, "original");

    let args = ActionArgs::Multi(vec![
        ActionArgs::Usize(100),
        ActionArgs::String("wrapper_both"),
    ]);
    __action_process_both__(&mut app, &args);
    assert_eq!(app.volume, 100);
    assert_eq!(app.name, "wrapper_both");
}

#[test]
fn test_fn_no_extra_param_wrapper() {
    let app = TestApp {
        volume: 42,
        name: String::new(),
    };

    let result = get_app_volume(&app);
    assert_eq!(result, 42);

    let args = ActionArgs::None;
    let result = __action_get_app_volume__(&app, &args);
    assert_eq!(result, 42);
}
