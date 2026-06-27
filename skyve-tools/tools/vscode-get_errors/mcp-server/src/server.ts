#!/usr/bin/env node
import * as fs from 'node:fs/promises';
import * as os from 'node:os';
import * as path from 'node:path';
import { McpServer } from '@modelcontextprotocol/sdk/server/mcp.js';
import { StdioServerTransport } from '@modelcontextprotocol/sdk/server/stdio.js';
import { z } from 'zod';

interface DiagnosticResponse {
	[key: string]: unknown;
	diagnostics: DiagnosticError[];
	workspaceFolders: string[];
	activeFile?: string;
}

interface DiagnosticError {
	[key: string]: unknown;
	file: string;
	line: number;
	character: number;
	severity: string;
	source: string;
	message: string;
	code?: string;
}

interface BridgeInstance {
	pid: number;
	port: number;
	url: string;
	workspaceFolders: string[];
	updatedAt: string;
}

const fallbackBridgeUrl = process.env.VSCODE_DIAGNOSTICS_BRIDGE_URL ?? 'http://127.0.0.1:37645';
const bridgeToken = process.env.VSCODE_DIAGNOSTICS_BRIDGE_TOKEN;

const server = new McpServer({
	name: 'vscode-get_errors',
	version: '0.1.0'
}, {
	instructions: 'Use get_errors to inspect VS Code Problems diagnostics for Java/JDT, SonarLint, and other language servers. For Skyve Java edits, include warnings and treat them as actionable unless the repository guidance says otherwise.'
});

server.tool(
	'get_errors',
	'Retrieves VS Code Problems diagnostics from the open workspace or specific absolute file paths. Returns errors and warnings by default.',
	{
		filePaths: z.array(z.string()).optional().describe('Optional absolute file paths to filter diagnostics for. If omitted, retrieves all active diagnostics in the open VS Code workspace.'),
		workspacePath: z.string().optional().describe('Optional workspace path used to select the VS Code window when multiple windows are open.'),
		includeWarnings: z.boolean().optional().default(true).describe('Include warnings as well as errors. Defaults to true.'),
		activeFileOnly: z.boolean().optional().default(false).describe('When true, ignore filePaths and return diagnostics for the active VS Code editor file only.')
	},
	async ({ filePaths, workspacePath, includeWarnings, activeFileOnly }) => {
		const response = await fetchDiagnostics(filePaths, workspacePath, includeWarnings, activeFileOnly);
		return {
			content: [
				{
					type: 'text',
					text: JSON.stringify(response.diagnostics, null, 2)
				}
			],
			structuredContent: response
		};
	}
);

async function fetchDiagnostics(
	filePaths: string[] | undefined,
	workspacePath: string | undefined,
	includeWarnings: boolean,
	activeFileOnly: boolean): Promise<DiagnosticResponse> {
	const bridgeUrl = await resolveBridgeUrl(filePaths, workspacePath);
	const response = await fetch(`${bridgeUrl}/diagnostics`, {
		method: 'POST',
		headers: buildHeaders(),
		body: JSON.stringify({
			filePaths,
			includeWarnings,
			activeFileOnly
		})
	});

	if (!response.ok) {
		throw new Error(`VS Code diagnostics bridge returned ${response.status}: ${await response.text()}`);
	}

	return await response.json() as DiagnosticResponse;
}

async function resolveBridgeUrl(filePaths: string[] | undefined, workspacePath: string | undefined): Promise<string> {
	if (process.env.VSCODE_DIAGNOSTICS_BRIDGE_URL) {
		return fallbackBridgeUrl;
	}

	const instances = await readInstances();
	if (instances.length === 0) {
		return fallbackBridgeUrl;
	}

	const explicitTargetPaths = [
		...(filePaths && filePaths.length > 0 ? filePaths : []),
		...(workspacePath ? [workspacePath] : []),
		...(process.env.VSCODE_DIAGNOSTICS_WORKSPACE ? [process.env.VSCODE_DIAGNOSTICS_WORKSPACE] : [])
	];
	if (explicitTargetPaths.length === 0) {
		return mostRecentlyUpdated(instances).url;
	}

	const targetPaths = explicitTargetPaths.map((targetPath) => path.resolve(targetPath));
	let best: { instance: BridgeInstance; prefixLength: number } | undefined;

	for (const instance of instances) {
		for (const workspaceFolder of instance.workspaceFolders) {
			for (const targetPath of targetPaths) {
				if (!isPathInside(targetPath, workspaceFolder)) {
					continue;
				}

				if (!best || workspaceFolder.length > best.prefixLength) {
					best = {
						instance,
						prefixLength: workspaceFolder.length
					};
				}
			}
		}
	}

	if (!best) {
		throw new Error(`No registered VS Code diagnostics bridge matches ${targetPaths.join(', ')}`);
	}

	return best.instance.url;
}

async function readInstances(): Promise<BridgeInstance[]> {
	const registryPath = path.join(os.homedir(), '.codex', 'vscode-get_errors', 'instances.json');
	try {
		const content = await fs.readFile(registryPath, 'utf8');
		return JSON.parse(content) as BridgeInstance[];
	}
	catch (error) {
		if ((error as NodeJS.ErrnoException).code === 'ENOENT') {
			return [];
		}
		throw error;
	}
}

function mostRecentlyUpdated(instances: BridgeInstance[]): BridgeInstance {
	return [...instances].sort((left, right) =>
		Date.parse(right.updatedAt) - Date.parse(left.updatedAt))[0];
}

function isPathInside(targetPath: string, workspaceFolder: string): boolean {
	const relative = path.relative(workspaceFolder, targetPath);
	return relative === '' || (!relative.startsWith('..') && !path.isAbsolute(relative));
}

function buildHeaders(): Record<string, string> {
	const headers: Record<string, string> = {
		'Content-Type': 'application/json'
	};

	if (bridgeToken) {
		headers.Authorization = `Bearer ${bridgeToken}`;
	}

	return headers;
}

const transport = new StdioServerTransport();
await server.connect(transport);
