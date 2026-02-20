use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use smol_str::SmolStr;

use crate::error::PkgError;
use crate::manifest::{self, PackageManifest};

/// A resolved module (source file) within a project.
#[derive(Debug, Clone)]
pub struct ResolvedModule {
    pub module_name: SmolStr,
    pub source_path: PathBuf,
    pub package_name: String,
}

/// A fully resolved project: all modules in topological order, ready for compilation.
#[derive(Debug, Clone)]
pub struct ResolvedProject {
    pub modules: Vec<ResolvedModule>,
    pub entry_module: Option<SmolStr>,
    pub entry_source: Option<PathBuf>,
    pub native_sources: Vec<PathBuf>,
    pub link_libs: Vec<String>,
    /// JS bridge files for WASM builds, collected from all packages.
    pub wasm_bridge_files: Vec<PathBuf>,
    pub package_root: PathBuf,
}

struct CollectedPackage {
    manifest: PackageManifest,
    root: PathBuf,
    dep_names: Vec<String>,
}

/// Resolve a project starting from a `weir.pkg` manifest.
pub fn resolve_project(manifest_path: &Path) -> Result<ResolvedProject, PkgError> {
    let manifest_path = std::fs::canonicalize(manifest_path).map_err(|e| PkgError::Io {
        path: manifest_path.to_path_buf(),
        source: e,
    })?;
    let package_root = manifest_path
        .parent()
        .unwrap_or_else(|| Path::new("."))
        .to_path_buf();

    // 1. Recursively collect all packages
    let packages = collect_packages(&manifest_path)?;

    // 2. Topological sort with cycle detection
    let sorted = topological_sort(&packages)?;

    // Read the root manifest to identify the entry point
    let root_source = std::fs::read_to_string(&manifest_path).map_err(|e| PkgError::Io {
        path: manifest_path.clone(),
        source: e,
    })?;
    let root_manifest = manifest::parse_manifest(&root_source, &manifest_path)?;

    // 3. Build resolved modules in topo order
    let mut modules = Vec::new();
    let mut native_sources = Vec::new();
    let mut link_libs = Vec::new();
    let mut wasm_bridge_files = Vec::new();

    for pkg_name in &sorted {
        let pkg = &packages[pkg_name];

        // Collect native sources and link libs
        for ns in &pkg.manifest.native_sources {
            if !native_sources.contains(ns) {
                native_sources.push(ns.clone());
            }
        }
        for lib in &pkg.manifest.link_libs {
            if !link_libs.contains(lib) {
                link_libs.push(lib.clone());
            }
        }
        for wf in &pkg.manifest.wasm_bridge_files {
            if !wasm_bridge_files.contains(wf) {
                wasm_bridge_files.push(wf.clone());
            }
        }

        // Map source files to modules
        for source_path in &pkg.manifest.sources {
            let module_name = compute_module_name(&pkg.manifest.name, source_path);
            let abs_path = if source_path.is_absolute() {
                source_path.clone()
            } else {
                pkg.root.join(source_path)
            };
            modules.push(ResolvedModule {
                module_name,
                source_path: abs_path,
                package_name: pkg.manifest.name.clone(),
            });
        }
    }

    // 4. Determine entry module
    let entry_source = root_manifest.main.map(|m| {
        if m.is_absolute() {
            m
        } else {
            package_root.join(m)
        }
    });
    let entry_module = entry_source
        .as_ref()
        .map(|p| compute_module_name(&root_manifest.name, p));

    // If main is specified but not in sources, add it as a module
    if let Some(ref entry_path) = entry_source {
        let already_listed = modules.iter().any(|m| m.source_path == *entry_path);
        if !already_listed {
            let mod_name = compute_module_name(&root_manifest.name, entry_path);
            modules.push(ResolvedModule {
                module_name: mod_name,
                source_path: entry_path.clone(),
                package_name: root_manifest.name.clone(),
            });
        }
    }

    Ok(ResolvedProject {
        modules,
        entry_module,
        entry_source,
        native_sources,
        link_libs,
        wasm_bridge_files,
        package_root,
    })
}

/// Compute the module name from a package name and source file path.
///
/// `lib.weir` → bare package name (e.g. `"weir-gl"`)
/// `other.weir` → `"package.other"` (e.g. `"weir-gl.utils"`)
pub fn compute_module_name(package_name: &str, source_path: &Path) -> SmolStr {
    let stem = source_path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("unknown");

    if stem == "lib" {
        SmolStr::new(package_name)
    } else {
        SmolStr::new(format!("{}.{}", package_name, stem))
    }
}

fn collect_packages(
    manifest_path: &Path,
) -> Result<HashMap<String, CollectedPackage>, PkgError> {
    let mut packages = HashMap::new();
    let mut queue = vec![manifest_path.to_path_buf()];
    let mut visited = HashSet::new();

    while let Some(path) = queue.pop() {
        let canonical = std::fs::canonicalize(&path).map_err(|e| PkgError::Io {
            path: path.clone(),
            source: e,
        })?;

        if visited.contains(&canonical) {
            continue;
        }
        visited.insert(canonical.clone());

        let pkg_dir = canonical
            .parent()
            .unwrap_or_else(|| Path::new("."))
            .to_path_buf();

        let source = std::fs::read_to_string(&canonical).map_err(|e| PkgError::Io {
            path: canonical.clone(),
            source: e,
        })?;
        let manifest = manifest::parse_manifest(&source, &canonical)?;

        let dep_names: Vec<String> = manifest.deps.iter().map(|d| d.name.clone()).collect();

        // Queue dependency manifests
        for dep in &manifest.deps {
            let dep_manifest = dep.path.join("weir.pkg");
            if !dep_manifest.exists() {
                return Err(PkgError::DepNotFound {
                    name: dep.name.clone(),
                    path: dep.path.clone(),
                });
            }
            queue.push(dep_manifest);
        }

        packages.insert(
            manifest.name.clone(),
            CollectedPackage {
                manifest,
                root: pkg_dir,
                dep_names,
            },
        );
    }

    Ok(packages)
}

fn topological_sort(
    packages: &HashMap<String, CollectedPackage>,
) -> Result<Vec<String>, PkgError> {
    let mut result = Vec::new();
    let mut visited = HashSet::new();
    let mut in_stack = HashSet::new();

    for name in packages.keys() {
        if !visited.contains(name) {
            topo_dfs(name, packages, &mut visited, &mut in_stack, &mut result)?;
        }
    }

    Ok(result)
}

fn topo_dfs(
    name: &str,
    packages: &HashMap<String, CollectedPackage>,
    visited: &mut HashSet<String>,
    in_stack: &mut HashSet<String>,
    result: &mut Vec<String>,
) -> Result<(), PkgError> {
    if in_stack.contains(name) {
        return Err(PkgError::DependencyCycle(name.to_string()));
    }
    if visited.contains(name) {
        return Ok(());
    }

    in_stack.insert(name.to_string());

    if let Some(pkg) = packages.get(name) {
        for dep_name in &pkg.dep_names {
            topo_dfs(dep_name, packages, visited, in_stack, result)?;
        }
    }

    in_stack.remove(name);
    visited.insert(name.to_string());
    result.push(name.to_string());

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn module_name_lib_is_bare_package() {
        assert_eq!(
            compute_module_name("weir-gl", Path::new("lib.weir")),
            SmolStr::new("weir-gl")
        );
    }

    #[test]
    fn module_name_other_is_dotted() {
        assert_eq!(
            compute_module_name("weir-gl", Path::new("utils.weir")),
            SmolStr::new("weir-gl.utils")
        );
    }

    #[test]
    fn module_name_nested_path() {
        assert_eq!(
            compute_module_name("foo", Path::new("src/bar.weir")),
            SmolStr::new("foo.bar")
        );
    }

    #[test]
    fn module_name_lib_nested() {
        assert_eq!(
            compute_module_name("foo", Path::new("src/lib.weir")),
            SmolStr::new("foo")
        );
    }
}
