import * as http from 'node:http';
import * as fs from 'node:fs/promises';
import * as os from 'node:os';
import * as path from 'node:path';
import * as vscode from 'vscode';

interface DiagnosticRequest {
	filePaths?: string[];
	includeWarnings?: boolean;
	activeFileOnly?: boolean;
}

interface DiagnosticResponse {
	diagnostics: DiagnosticError[];
	workspaceFolders: string[];
	activeFile?: string;
}

interface DiagnosticError {
	file: string;
	line: number;
	character: number;
	severity: string;
	source: string;
	message: string;
	code?: string;
}

let server: http.Server | undefined;
let activePort: number | undefined;

export function activate(context: vscode.ExtensionContext): void {
	const output = vscode.window.createOutputChannel('Diagnostics Bridge');
	context.subscriptions.push(output);

	const restart = async () => {
		await stopServer();
		startServer(output);
	};

	context.subscriptions.push(vscode.commands.registerCommand('vscode-get-errors.restart', restart));
	context.subscriptions.push(vscode.workspace.onDidChangeConfiguration((event) => {
		if (event.affectsConfiguration('vscodeDiagnosticsBridge.port') ||
				event.affectsConfiguration('vscodeDiagnosticsBridge.token')) {
			void restart();
		}
	}));
	context.subscriptions.push(vscode.window.onDidChangeWindowState((state) => {
		if (state.focused) {
			void registerInstance(output);
		}
	}));
	context.subscriptions.push(vscode.window.onDidChangeActiveTextEditor(() => {
		void registerInstance(output);
	}));
	context.subscriptions.push(vscode.workspace.onDidChangeWorkspaceFolders(() => {
		void registerInstance(output);
	}));

	startServer(output);
}

export async function deactivate(): Promise<void> {
	await stopServer();
}

function startServer(output: vscode.OutputChannel): void {
	const configuration = vscode.workspace.getConfiguration('vscodeDiagnosticsBridge');
	const port = configuration.get<number>('port', 37645);

	server = http.createServer(async (request, response) => {
		try {
			if (!isAuthorized(request)) {
				sendJson(response, 401, { error: 'Unauthorized' });
				return;
			}

			if (request.method === 'GET' && request.url === '/health') {
				sendJson(response, 200, {
					ok: true,
					workspaceFolders: getWorkspaceFolders(),
					activeFile: getActiveFile()
				});
				return;
			}

			if (request.method === 'POST' && request.url === '/diagnostics') {
				const body = await readJsonBody<DiagnosticRequest>(request);
				sendJson(response, 200, collectDiagnostics(body));
				return;
			}

			sendJson(response, 404, { error: 'Not found' });
		}
		catch (error) {
			const message = error instanceof Error ? error.message : String(error);
			output.appendLine(`Request failed: ${message}`);
			sendJson(response, 500, { error: message });
		}
	});

	listenOnAvailablePort(server, port, output);
}

function listenOnAvailablePort(httpServer: http.Server, preferredPort: number, output: vscode.OutputChannel): void {
	let candidatePort = preferredPort;
	const maxPort = Math.min(preferredPort + 20, 65535);

	const tryListen = () => {
		httpServer.listen(candidatePort, '127.0.0.1', () => {
			activePort = candidatePort;
			output.appendLine(`Diagnostics bridge listening on http://127.0.0.1:${candidatePort}`);
			void registerInstance(output);
		});
	};

	httpServer.on('error', (error: NodeJS.ErrnoException) => {
		if (error.code === 'EADDRINUSE' && candidatePort < maxPort) {
			candidatePort++;
			tryListen();
			return;
		}

		output.appendLine(`Diagnostics bridge failed: ${error.message}`);
		void vscode.window.showWarningMessage(`Diagnostics Bridge failed to start: ${error.message}`);
	});

	tryListen();
}

function stopServer(): Promise<void> {
	return new Promise((resolve) => {
		if (!server) {
			resolve();
			return;
		}

		server.close(() => {
			server = undefined;
			activePort = undefined;
			resolve();
		});
	});
}

async function registerInstance(output: vscode.OutputChannel): Promise<void> {
	if (activePort === undefined) {
		return;
	}

	const registryPath = getRegistryPath();
	const instance = {
		pid: process.pid,
		port: activePort,
		url: `http://127.0.0.1:${activePort}`,
		workspaceFolders: getWorkspaceFolders(),
		updatedAt: new Date().toISOString()
	};

	try {
		await fs.mkdir(path.dirname(registryPath), { recursive: true });
		const instances = await readRegistry(registryPath);
		const remaining = instances.filter((entry) =>
			entry.pid !== process.pid &&
			JSON.stringify(entry.workspaceFolders) !== JSON.stringify(instance.workspaceFolders));
		remaining.push(instance);
		await fs.writeFile(registryPath, JSON.stringify(remaining, null, 2));
	}
	catch (error) {
		const message = error instanceof Error ? error.message : String(error);
		output.appendLine(`Could not update diagnostics bridge registry: ${message}`);
	}
}

async function readRegistry(registryPath: string): Promise<Array<{ pid: number; workspaceFolders: string[] }>> {
	try {
		const content = await fs.readFile(registryPath, 'utf8');
		return JSON.parse(content) as Array<{ pid: number; workspaceFolders: string[] }>;
	}
	catch (error) {
		if ((error as NodeJS.ErrnoException).code === 'ENOENT') {
			return [];
		}
		throw error;
	}
}

function getRegistryPath(): string {
	return path.join(os.homedir(), '.codex', 'vscode-get_errors', 'instances.json');
}

function collectDiagnostics(request: DiagnosticRequest): DiagnosticResponse {
	const diagnostics: DiagnosticError[] = [];
	const includeWarnings = request.includeWarnings !== false;
	const activeFileUri = getActiveFileUri();
	const activeFile = activeFileUri?.fsPath;
	const requestedFilePaths = request.filePaths?.filter((filePath) => filePath.trim().length > 0);

	if (request.activeFileOnly && activeFileUri) {
		processDiagnostics(activeFileUri, vscode.languages.getDiagnostics(activeFileUri), includeWarnings, diagnostics);
	}
	else if (requestedFilePaths && requestedFilePaths.length > 0) {
		const diagnosticsByPath = diagnosticsSnapshotByPath();
		for (const filePath of requestedFilePaths) {
			const diagnosticEntry = diagnosticEntryForPath(filePath, diagnosticsByPath, activeFileUri);
			processDiagnostics(diagnosticEntry.uri, diagnosticEntry.diagnostics, includeWarnings, diagnostics);
		}
	}
	else {
		for (const [uri, uriDiagnostics] of vscode.languages.getDiagnostics()) {
			if (!isInWorkspace(uri.fsPath)) {
				continue;
			}
			processDiagnostics(uri, uriDiagnostics, includeWarnings, diagnostics);
		}
	}

	diagnostics.sort((left, right) =>
		left.file.localeCompare(right.file) ||
		left.line - right.line ||
		left.character - right.character ||
		left.message.localeCompare(right.message));

	return {
		diagnostics,
		workspaceFolders: getWorkspaceFolders(),
		activeFile
	};
}

function diagnosticsSnapshotByPath(): Map<string, { uri: vscode.Uri; diagnostics: readonly vscode.Diagnostic[] }> {
	const result = new Map<string, { uri: vscode.Uri; diagnostics: readonly vscode.Diagnostic[] }>();
	for (const [uri, diagnostics] of vscode.languages.getDiagnostics()) {
		if (uri.scheme === 'file') {
			result.set(normalisePath(uri.fsPath), { uri, diagnostics });
		}
	}
	return result;
}

function diagnosticEntryForPath(
	filePath: string,
	diagnosticsByPath: Map<string, { uri: vscode.Uri; diagnostics: readonly vscode.Diagnostic[] }>,
	activeFileUri: vscode.Uri | undefined): { uri: vscode.Uri; diagnostics: readonly vscode.Diagnostic[] } {
	if (activeFileUri && samePath(filePath, activeFileUri.fsPath)) {
		return {
			uri: activeFileUri,
			diagnostics: vscode.languages.getDiagnostics(activeFileUri)
		};
	}

	const snapshotEntry = diagnosticsByPath.get(normalisePath(filePath));
	if (snapshotEntry) {
		return snapshotEntry;
	}

	const uri = vscode.Uri.file(filePath);
	return {
		uri,
		diagnostics: vscode.languages.getDiagnostics(uri)
	};
}

function samePath(left: string, right: string): boolean {
	return normalisePath(left) === normalisePath(right);
}

function normalisePath(filePath: string): string {
	const resolved = path.resolve(filePath);
	return process.platform === 'win32' || process.platform === 'darwin' ? resolved.toLowerCase() : resolved;
}

function processDiagnostics(
	uri: vscode.Uri,
	diagnostics: readonly vscode.Diagnostic[],
	includeWarnings: boolean,
	list: DiagnosticError[]): void {
	for (const diagnostic of diagnostics) {
		if (diagnostic.severity !== vscode.DiagnosticSeverity.Error &&
				(!includeWarnings || diagnostic.severity !== vscode.DiagnosticSeverity.Warning)) {
			continue;
		}

		list.push({
			file: uri.fsPath,
			line: diagnostic.range.start.line + 1,
			character: diagnostic.range.start.character + 1,
			severity: severityName(diagnostic.severity),
			source: diagnostic.source || 'VS Code',
			message: diagnostic.message,
			code: diagnostic.code === undefined ? undefined : String(diagnostic.code)
		});
	}
}

function severityName(severity: vscode.DiagnosticSeverity): string {
	switch (severity) {
		case vscode.DiagnosticSeverity.Error:
			return 'Error';
		case vscode.DiagnosticSeverity.Warning:
			return 'Warning';
		case vscode.DiagnosticSeverity.Information:
			return 'Information';
		case vscode.DiagnosticSeverity.Hint:
			return 'Hint';
		default:
			return 'Unknown';
	}
}

function getWorkspaceFolders(): string[] {
	return vscode.workspace.workspaceFolders?.map((folder) => folder.uri.fsPath) ?? [];
}

function isInWorkspace(filePath: string): boolean {
	return getWorkspaceFolders().some((workspaceFolder) => {
		const relative = path.relative(workspaceFolder, filePath);
		return relative === '' || (!relative.startsWith('..') && !path.isAbsolute(relative));
	});
}

function getActiveFile(): string | undefined {
	return getActiveFileUri()?.fsPath;
}

function getActiveFileUri(): vscode.Uri | undefined {
	const activeEditor = vscode.window.activeTextEditor;
	if (!activeEditor || activeEditor.document.uri.scheme !== 'file') {
		return undefined;
	}

	return activeEditor.document.uri;
}

function isAuthorized(request: http.IncomingMessage): boolean {
	const token = vscode.workspace.getConfiguration('vscodeDiagnosticsBridge').get<string>('token', '');
	if (!token) {
		return true;
	}

	return request.headers.authorization === `Bearer ${token}`;
}

function readJsonBody<T>(request: http.IncomingMessage): Promise<T> {
	return new Promise((resolve, reject) => {
		let body = '';
		request.setEncoding('utf8');
		request.on('data', (chunk: string) => {
			body += chunk;
			if (body.length > 1024 * 1024) {
				request.destroy(new Error('Request body is too large'));
			}
		});
		request.on('end', () => {
			if (!body) {
				resolve({} as T);
				return;
			}

			try {
				resolve(JSON.parse(body) as T);
			}
			catch (error) {
				reject(error);
			}
		});
		request.on('error', reject);
	});
}

function sendJson(response: http.ServerResponse, statusCode: number, body: unknown): void {
	response.writeHead(statusCode, {
		'Content-Type': 'application/json; charset=utf-8'
	});
	response.end(JSON.stringify(body));
}
