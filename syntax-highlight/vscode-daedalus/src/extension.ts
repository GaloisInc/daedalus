/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import * as vscode from 'vscode'

import {
	ClientCapabilities,
	ExecuteCommandParams,
	ExecuteCommandRequest,
	LanguageClient,
	LanguageClientOptions,
	SemanticTokenModifiers,
	SemanticTokenTypes,
	ServerOptions,
	StaticFeature,
	TokenFormat,
	TransportKind,
} from 'vscode-languageclient/node'

import {
	configLanguageServerPath,
	getConfiguration,
} from './configuration'


let client: LanguageClient

class SemanticTokensFeature implements StaticFeature {
	dispose() {
		return
	}
	fillClientCapabilities(capabilities: ClientCapabilities) {
		capabilities.textDocument = capabilities.textDocument || { moniker: {} }
		capabilities.textDocument.semanticTokens = {
			requests: {
				full: true,
			},
			tokenModifiers: [
				SemanticTokenModifiers.abstract,
				SemanticTokenModifiers.async,
				SemanticTokenModifiers.declaration,
				SemanticTokenModifiers.defaultLibrary,
				SemanticTokenModifiers.definition,
				SemanticTokenModifiers.deprecated,
				SemanticTokenModifiers.documentation,
				SemanticTokenModifiers.modification,
				SemanticTokenModifiers.readonly,
				SemanticTokenModifiers.static,
			],
			tokenTypes: [
				SemanticTokenTypes.class,
				SemanticTokenTypes.comment,
				SemanticTokenTypes.enum,
				SemanticTokenTypes.enumMember,
				SemanticTokenTypes.event,
				SemanticTokenTypes.function,
				SemanticTokenTypes.interface,
				SemanticTokenTypes.keyword,
				SemanticTokenTypes.macro,
				SemanticTokenTypes.method,
				SemanticTokenTypes.modifier,
				SemanticTokenTypes.namespace,
				SemanticTokenTypes.number,
				SemanticTokenTypes.operator,
				SemanticTokenTypes.parameter,
				SemanticTokenTypes.property,
				SemanticTokenTypes.regexp,
				SemanticTokenTypes.string,
				SemanticTokenTypes.struct,
				SemanticTokenTypes.type,
				SemanticTokenTypes.typeParameter,
				SemanticTokenTypes.variable,
			],
			formats: [TokenFormat.Relative],
		}
	}
	initialize(): void {
	}
}

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

	// client.registerFeature(new SemanticTokensFeature())

	const runWatchProvider = new DaedalusWatchContentProvider()

	const watchers: Watchers = {}

	context.subscriptions.push(

		runWatchProvider,

		vscode.commands.registerTextEditorCommand(
			'daedalus.watch',
			e => daedalusWatch(e, runWatchProvider, watchers),
		),

		vscode.workspace.registerTextDocumentContentProvider(
			DaedalusWatchContentProvider.scheme,
			runWatchProvider,
		),

	)

	// This will also launch the server.
	client.start()

	/**
	* The VSCode LSP implementation is set up so that there can only be *one*
	* handler for a given notification.  As a result, each watcher cannot
	* independently listen to 'daedalus/run/watchResult', instead we need this
	* one centralized listener to dispatch to the correct handler.
	*/
	client.onReady().then(() => {

		context.subscriptions.push(

			client.onNotification(
				'daedalus/run/watchResult',
				({ clientHandle, result }) => {
					if (clientHandle in watchers) {
						watchers[clientHandle](result)
					} else {
						vscode.window.showErrorMessage(
							`Could not find ${clientHandle} in watchers ${Object.keys(watchers)}`
						)
					}
				}
			),

		)

	})

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
	uri: vscode.Uri,
): Promise<number> {

	const c2p = client.code2ProtocolConverter

	const params: ExecuteCommandParams = {
		command: 'run/watch',
		arguments: [
			c2p.asTextDocumentIdentifier(editor.document),
			c2p.asPosition(editor.selection.active),
			c2p.asUri(uri),
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


let daedalusWatchCounter = 0


type Watchers = { [uri: string]: (contents: string) => void }


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
	watchers: Watchers,
): Promise<void> {

	return new Promise(async resolve => {

		const cleanup = () => {
			vscode.window.showInformationMessage('Cleaning up a Deadalus watcher.')
			editorClosedDisposable.dispose()
			resolve()
		}

		// It is pleasant to have the file name in the tab label.
		const editorName = editor.document.fileName
		const uri = vscode.Uri.parse(
			`${DaedalusWatchContentProvider.scheme}:${editorName}-${daedalusWatchCounter++}`,
			true,
		)

		// Register this watcher so that it updates the document on
		// notifications.
		watchers[uri.toString()] = (result: string) => provider.update(uri, result)

		const lspCancellationToken = await daedalusRunWatch(editor, uri)

		const doc = await vscode.workspace.openTextDocument(uri)

		const lspEditor = await vscode.window.showTextDocument(doc, {
			preserveFocus: true,
			preview: false,
			viewColumn: vscode.ViewColumn.Beside,
		})

		// When the user closes the virtual editor, we send the cancel message
		// to the server.
		const editorClosedDisposable =
			vscode.window.onDidChangeVisibleTextEditors(visibleEditors => {
				// Not sure how to compare TextEditors, and the doc is lacking...
				if (!visibleEditors.some(e => e.document.uri.toString() === lspEditor.document.uri.toString())) {
					// This does not really work the way you'd expect: VSCode
					// keeps documents open way past when the tab is closed.
					// The API for accessing tab info has been in progress since
					// 2016...  See
					// https://github.com/microsoft/vscode/issues/15178
					if (lspEditor.document.isClosed) {
						daedalusRunCancel(lspCancellationToken)
						cleanup()
					}
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
