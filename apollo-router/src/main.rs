//! Main entry point for CLI command to start server.

fn main() {
    #[cfg(target_os = "linux")]
    unsafe {
        let info = libc::mallinfo();
        tracing::info!("keepcost: {}", info.keepcost);
        libc::malloc_trim(0);
    }

    match apollo_router::main() {
        Ok(_) => {}
        Err(e) => {
            eprintln!("{e}");
            std::process::exit(1)
        }
    }
}
