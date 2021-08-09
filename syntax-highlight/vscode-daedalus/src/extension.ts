/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import * as vscode from 'vscode'

import {
	ExecuteCommandParams,
	ExecuteCommandRequest,
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind
} from 'vscode-languageclient/node'

import {
	configLanguageServerPath,
	getConfiguration,
} from './configuration'


let client: LanguageClient


export function activate(context: vscode.ExtensionContext) {

	const configuration = getConfiguration()

	let serverExecutable = configuration[configLanguageServerPath]

	// If the extension is launched in debug mode then the debug server options
	// are used.  Otherwise the run options are used.

	let serverOptions: ServerOptions = {
		run: { command: serverExecutable, transport: TransportKind.stdio },
		debug: { command: serverExecutable, transport: TransportKind.stdio }
	}

	let clientOptions: LanguageClientOptions = {
		documentSelector: [{ scheme: 'file', language: 'daedalus' }]
	}

	client = new LanguageClient(
		'DaedalusLanguageServer',
		'DDL Language Server',
		serverOptions,
		clientOptions
	)

	// This will also launch the server.
	client.start()

	const runWatchProvider = new DaedalusWatchContentProvider()

	context.subscriptions.push(

		runWatchProvider,

		vscode.commands.registerTextEditorCommand(
			'daedalus.watch',
			e => daedalusWatch(e, runWatchProvider),
		),

		vscode.workspace.registerTextDocumentContentProvider(
			DaedalusWatchContentProvider.scheme,
			runWatchProvider,
		)

	)

}


export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined
	}
	return client.stop()
}


/**
 * Sends the 'workspace/executeCommand' request 'run/watch' to the language
 * server.
 * @param editor - Editor to watch.
 * @returns The LSP cancellation token for this watch task.
 */
async function daedalusRunWatch(
	editor: vscode.TextEditor,
): Promise<number> {

	const c2p = client.code2ProtocolConverter

	const params: ExecuteCommandParams = {
		command: 'run/watch',
		arguments: [
			c2p.asTextDocumentIdentifier(editor.document),
			c2p.asPosition(editor.selection.active),
			c2p.asUri(editor.document.uri),
		],
	}

	return await client.sendRequest(
		ExecuteCommandRequest.type,
		params,
	)

}


/**
 * Sends the 'workspace/executeCommand' request 'run/cancel' to the language
 * server.
 * @param lspCancellationToken - Token received from an acknowledged 'run/watch'
 * command.
 */
function daedalusRunCancel(
	lspCancellationToken: number,
): void {
	const cancelParams: ExecuteCommandParams = {
		command: 'run/cancel',
		arguments: [
			lspCancellationToken,
		],
	}

	client.sendRequest(
		ExecuteCommandRequest.type,
		cancelParams,
	)
}


/**
 * Sends a watch request to the language server, and awaits either the
 * 'daedalus/run/watchResult' notification, or an intent from the user to cancel
 * the request.
 * @param editor - Editor to watch.
 * @param provider - The text content provider in charge of displaying results
 * from the watch task.
 * @returns - Promise resolved upon completion or cancellation.
 */
function daedalusWatch(
	editor: vscode.TextEditor,
	provider: DaedalusWatchContentProvider,
): Promise<void> {

	return new Promise(async resolve => {

		const cleanup = () => {
			disposable.dispose()
			editorClosedDispoable.dispose()
			resolve()
		}

		const lspCancellationToken = await daedalusRunWatch(editor)

		// It is pleasant to have the file name in the tab label.
		const editorName = editor.document.fileName
		const uri = vscode.Uri.parse(
			`${DaedalusWatchContentProvider.scheme}:${editorName}-${lspCancellationToken}`,
			true,
		)

		const disposable = client.onNotification(
			'daedalus/run/watchResult',
			({ _clientHandle, result }) => {
				provider.update(uri, result)
			}
		)

		const doc = await vscode.workspace.openTextDocument(uri)

		const lspEditor = await vscode.window.showTextDocument(doc, {
			preserveFocus: true,
			preview: false,
			viewColumn: vscode.ViewColumn.Beside,
		})

		// When the user closes the virtual editor, we send the cancel message
		// to the server.
		const editorClosedDispoable =
			vscode.window.onDidChangeVisibleTextEditors(e => {
				if (!e.includes(lspEditor)) {
					daedalusRunCancel(lspCancellationToken)
					cleanup()
				}
			})

	})

}


class DaedalusWatchContentProvider implements vscode.TextDocumentContentProvider {

	static scheme = 'daedalus-watch'

	private _lastResult = ''
	private _onDidChangeEmitter = new vscode.EventEmitter<vscode.Uri>()
	onDidChange = this._onDidChangeEmitter.event

	dispose() {
		this._onDidChangeEmitter.dispose()
	}

	provideTextDocumentContent(_uri: vscode.Uri): string {
		return this._lastResult
	}

	update(uri: vscode.Uri, s: string): void {
		this._lastResult = s
		this._onDidChangeEmitter.fire(uri)
	}

}
