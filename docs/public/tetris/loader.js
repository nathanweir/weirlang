// Weir WASM Loader
import { createRuntime } from './runtime.js';
import { createGLBridge } from './gl_bridge.js';

const runtime = createRuntime();
const importObj = { env: { ...runtime.env } };

const canvas = document.getElementById('canvas');
const gl = createGLBridge(canvas);
Object.assign(importObj.env, gl.env);

const { instance } = await WebAssembly.instantiateStreaming(
    fetch('app.wasm'), importObj
);

runtime.setMemory(instance.exports.memory);
gl.setMemory(instance.exports.memory);

// Run main
instance.exports.weir_main();

// If weir_frame is exported, drive it with requestAnimationFrame
if (instance.exports.weir_frame) {
    function frame() {
        instance.exports.weir_frame();
        requestAnimationFrame(frame);
    }
    requestAnimationFrame(frame);
}
