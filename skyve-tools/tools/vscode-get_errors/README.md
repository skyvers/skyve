# VS Code get_errors

This bridge exposes VS Code Problems diagnostics to Codex through a local MCP tool named `get_errors`.

The bridge has two parts:

- `extension/`: a VS Code extension that runs inside VS Code and reads `vscode.languages.getDiagnostics()`.
- `mcp-server/`: a stdio MCP server launched by Codex. It calls the local VS Code extension endpoint.

## Install

From this directory:

```bash
cd extension
npm install
npm run package
code --install-extension vscode-get-errors-0.1.0.vsix
```

If the `code` shell command is not on `PATH` on macOS, use the VS Code app bundle CLI directly:

```bash
/Applications/Visual\ Studio\ Code.app/Contents/Resources/app/bin/code --install-extension vscode-get-errors-0.1.0.vsix --force
```

Reload VS Code and open the Skyve workspace. The extension starts a localhost diagnostics endpoint on port `37645` by default.

Then build the MCP server:

```bash
cd ../mcp-server
npm install
npm run build
```

Add this to `~/.codex/config.toml`:

```toml
[mcp_servers.get_errors]
command = "node"
args = ["/Users/mike/_/skyve/skyve-tools/tools/vscode-get_errors/mcp-server/dist/server.js"]
cwd = "/Users/mike/_/skyve"
startup_timeout_sec = 10
tool_timeout_sec = 30
```

Restart Codex after changing the config.

## Verify

With VS Code open on the Skyve workspace, check the extension endpoint:

```bash
curl http://127.0.0.1:37645/health
```

After Codex restarts, the MCP tool `get_errors` should be available. It accepts optional absolute file paths:

```json
{
  "filePaths": ["/Users/mike/_/skyve/skyve-core/src/main/java/example/File.java"]
}
```

If `filePaths` is omitted, the tool returns all active VS Code diagnostics in the open workspace. By default it includes both errors and warnings, because Skyve treats JDT warnings as actionable.

When several VS Code windows are open, pass `workspacePath` to select the window explicitly:

```json
{
  "workspacePath": "/Users/mike/_/skyve"
}
```

Without `filePaths` or `workspacePath`, the MCP server uses the most recently focused or active registered VS Code window.

For a Codex profile that should always target one workspace, set `VSCODE_DIAGNOSTICS_WORKSPACE` on the MCP server:

```toml
[mcp_servers.get_errors.env]
VSCODE_DIAGNOSTICS_WORKSPACE = "/Users/mike/_/skyve"
```

After reinstalling the VS Code extension, reload every open VS Code window that should serve diagnostics. The extension registry is per running window, so stale extension hosts can keep serving the old routing behaviour until they are reloaded.

To return diagnostics for the current VS Code editor tab only:

```json
{
  "activeFileOnly": true
}
```

The response also includes `activeFile` when VS Code has a file-backed active editor.

## Configuration

VS Code settings:

- `vscodeDiagnosticsBridge.port`: localhost port for the extension endpoint. Default: `37645`.
- `vscodeDiagnosticsBridge.token`: optional bearer token. If set, the MCP server must receive the same value in `VSCODE_DIAGNOSTICS_BRIDGE_TOKEN`.

MCP server environment variables:

- `VSCODE_DIAGNOSTICS_BRIDGE_URL`: override extension endpoint URL. Default: `http://127.0.0.1:37645`.
- `VSCODE_DIAGNOSTICS_BRIDGE_TOKEN`: optional bearer token matching the VS Code setting.
- `VSCODE_DIAGNOSTICS_WORKSPACE`: optional workspace path used to select the correct VS Code window when several windows are registered.
