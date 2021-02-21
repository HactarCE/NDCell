fn main() {
    #[cfg(all(windows, not(debug_assertions)))]
    {
        let mut res = winres::WindowsResource::new();
        res.set_icon("resources/icon/ndcell.ico");
        res.compile().unwrap();
    }
}
