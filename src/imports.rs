use wasmtime;

pub fn make_imports<T>(
    mut store: impl wasmtime::AsContextMut<Data = T>,
) -> Vec<wasmtime::Extern> {
    let print = wasmtime::Func::wrap(&mut store, |x: i64| println!("{}", x));
    vec![wasmtime::Extern::Func(print)]
}
