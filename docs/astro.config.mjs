// @ts-check
import { defineConfig } from "astro/config";
import starlight from "@astrojs/starlight";
import starlightClientMermaid from "@pasqal-io/starlight-client-mermaid";

// https://astro.build/config
export default defineConfig({
  integrations: [
    starlight({
      title: "Weir Language",
      plugins: [starlightClientMermaid()],
      social: [
        {
          icon: "github",
          label: "GitHub",
          href: "https://github.com/nathanweir/weirlang",
        },
      ],
      sidebar: [
        {
          label: "Getting Started",
          items: [
            { label: "Introduction", slug: "getting-started/introduction" },
            { label: "Installation", slug: "getting-started/installation" },
            { label: "Hello World", slug: "getting-started/hello-world" },
            { label: "CLI Reference", slug: "getting-started/cli-reference" },
          ],
        },
        {
          label: "Language Guide",
          items: [
            { label: "Syntax", slug: "guide/syntax" },
            { label: "Data Types", slug: "guide/data-types" },
            { label: "Functions", slug: "guide/functions" },
            { label: "Control Flow", slug: "guide/control-flow" },
            { label: "Pattern Matching", slug: "guide/pattern-matching" },
            { label: "Structs & Types", slug: "guide/structs-types" },
            { label: "Typeclasses", slug: "guide/typeclasses" },
            { label: "Macros", slug: "guide/macros" },
            { label: "Error Handling", slug: "guide/error-handling" },
          ],
        },
        {
          label: "Standard Library",
          items: [
            { label: "Built-in Functions", slug: "stdlib/builtins" },
            { label: "String Operations", slug: "stdlib/strings" },
            { label: "I/O", slug: "stdlib/io" },
            { label: "Collections", slug: "stdlib/collections" },
          ],
        },
        {
          label: "Concepts",
          items: [
            {
              label: "Compilation Pipeline",
              slug: "concepts/compilation-pipeline",
            },
            { label: "Live Reloading", slug: "concepts/live-reloading" },
            { label: "Memory Model", slug: "concepts/memory-model" },
            { label: "Package System", slug: "concepts/package-system" },
            { label: "Type Inference", slug: "concepts/type-inference" },
          ],
        },
        {
          label: "Interop",
          items: [
            { label: "C FFI", slug: "interop/c-ffi" },
            { label: "Rust Integration", slug: "interop/rust" },
            { label: "Native Libraries", slug: "interop/native-libraries" },
          ],
        },
        {
          label: "Reference",
          items: [
            { label: "Grammar", slug: "reference/grammar" },
            { label: "Operator Precedence", slug: "reference/operators" },
            { label: "Reserved Words", slug: "reference/reserved-words" },
          ],
        },
      ],
    }),
  ],
});
