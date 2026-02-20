/// JavaScript runtime bridge that provides WASM imports.
pub const RUNTIME_JS: &str = r#"// Weir WASM Runtime Bridge
// Provides the "env" imports expected by compiled Weir WASM modules.

export function createRuntime() {
    let memory = null;

    function readString(ptr) {
        const mem = new Uint8Array(memory.buffer);
        let end = ptr;
        while (mem[end] !== 0) end++;
        return new TextDecoder().decode(mem.subarray(ptr, end));
    }

    const env = {
        // Printing
        weir_print_i64(n) {
            const lo = Number(n & BigInt(0xFFFFFFFF));
            const hi = Number((n >> BigInt(32)) & BigInt(0xFFFFFFFF));
            const val = hi !== 0 ? Number(n) : lo;
            if (typeof process !== 'undefined' && process.stdout) {
                process.stdout.write(String(val));
            } else {
                const el = document.getElementById('output');
                if (el) el.insertAdjacentText('beforeend', String(val));
            }
        },
        weir_print_f64(n) {
            const s = String(n);
            if (typeof process !== 'undefined' && process.stdout) {
                process.stdout.write(s);
            } else {
                const el = document.getElementById('output');
                if (el) el.insertAdjacentText('beforeend', s);
            }
        },
        weir_print_str(ptr) {
            const s = readString(ptr);
            if (typeof process !== 'undefined' && process.stdout) {
                process.stdout.write(s);
            } else {
                const el = document.getElementById('output');
                if (el) el.insertAdjacentText('beforeend', s);
            }
        },
        weir_print_bool(b) {
            const s = b ? 'true' : 'false';
            if (typeof process !== 'undefined' && process.stdout) {
                process.stdout.write(s);
            } else {
                const el = document.getElementById('output');
                if (el) el.insertAdjacentText('beforeend', s);
            }
        },
        weir_print_unit() {
            const s = '()';
            if (typeof process !== 'undefined' && process.stdout) {
                process.stdout.write(s);
            } else {
                const el = document.getElementById('output');
                if (el) el.insertAdjacentText('beforeend', s);
            }
        },
        weir_print_newline() {
            if (typeof process !== 'undefined' && process.stdout) {
                process.stdout.write('\n');
            } else {
                const el = document.getElementById('output');
                if (el) el.insertAdjacentText('beforeend', '\n');
            }
        },
        // Time
        weir_time_ms() {
            return BigInt(Math.floor(performance.now()));
        },
        // Sleep (no-op in browser)
        weir_sleep_ms(_ms) {},
        // File I/O (stubs)
        weir_read_file(_ptr) { return 0; },
        weir_write_file(_ptr, _data) {},
        // Math
        sin: Math.sin,
        cos: Math.cos,
        tan: Math.tan,
        asin: Math.asin,
        acos: Math.acos,
        atan: Math.atan,
        atan2: Math.atan2,
        exp: Math.exp,
        log: Math.log,
        pow: Math.pow,
        round: Math.round,
        sqrt: Math.sqrt,
    };

    return {
        env,
        setMemory(mem) { memory = mem; },
    };
}
"#;

/// Generate loader.js content.
pub fn generate_loader(has_gl_bridge: bool) -> String {
    let gl_import = if has_gl_bridge {
        "import { createGLBridge } from './gl_bridge.js';\n"
    } else {
        ""
    };

    let gl_setup = if has_gl_bridge {
        r#"
const canvas = document.getElementById('canvas');
const gl = createGLBridge(canvas);
Object.assign(importObj.env, gl.env);
"#
    } else {
        ""
    };

    let gl_memory = if has_gl_bridge {
        "gl.setMemory(instance.exports.memory);\n"
    } else {
        ""
    };

    format!(
        r#"// Weir WASM Loader
import {{ createRuntime }} from './runtime.js';
{gl_import}
const runtime = createRuntime();
const importObj = {{ env: {{ ...runtime.env }} }};
{gl_setup}
const {{ instance }} = await WebAssembly.instantiateStreaming(
    fetch('app.wasm'), importObj
);

runtime.setMemory(instance.exports.memory);
{gl_memory}
// Run main
instance.exports.weir_main();

// If weir_frame is exported, drive it with requestAnimationFrame
if (instance.exports.weir_frame) {{
    function frame() {{
        instance.exports.weir_frame();
        requestAnimationFrame(frame);
    }}
    requestAnimationFrame(frame);
}}
"#
    )
}

/// Generate index.html content.
pub fn generate_html(title: &str) -> String {
    format!(
        r#"<!DOCTYPE html>
<html>
<head>
  <meta charset="utf-8">
  <title>{title}</title>
  <style>
    body {{ margin: 0; background: #1a1a2e; display: flex; justify-content: center; align-items: center; min-height: 100vh; }}
    canvas {{ display: block; }}
    #output {{ color: #e0e0e0; font-family: monospace; white-space: pre; padding: 16px; }}
  </style>
</head>
<body>
  <canvas id="canvas" width="920" height="1280"></canvas>
  <pre id="output"></pre>
  <script type="module" src="loader.js"></script>
</body>
</html>
"#
    )
}
