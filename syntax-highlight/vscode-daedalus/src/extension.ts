/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import * as path from 'path'
import * as vscode from 'vscode'
import * as vscodelc from 'vscode-languageclient'

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

	// If the extension is launched in debug mode then the debug server options are used
	// Otherwise the run options are used

	let serverOptions: ServerOptions = {
		run: { command: serverExecutable, transport: TransportKind.stdio },
		debug: { command: serverExecutable, transport: TransportKind.stdio }
	}

	// Options to control the language client
	let clientOptions: LanguageClientOptions = {
		documentSelector: [{ scheme: 'file', language: 'daedalus' }]
	}

	// Create the language client and start the client.
	client = new LanguageClient(
		'DaedalusLanguageServer',
		'DDL Language Server',
		serverOptions,
		clientOptions
	)

	// Start the client. This will also launch the server
	client.start()

	context.subscriptions.push(
		vscode.commands.registerTextEditorCommand(
			'daedalus.watch',
			daedalusWatch,
		)
	)

}


export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined
	}
	return client.stop()
}


function daedalusWatch(editor: vscode.TextEditor): void {
	vscode.window.withProgress(
		{
			cancellable: true,
			location: vscode.ProgressLocation.Window,
			title: "Watch at point (DAEDALUS)",
		},
		(_progress, vscodeCancellationToken) => {
			return daedalusWatchProgress(editor, vscodeCancellationToken)
		},
	)
}


/**
 * Sends the 'workspace/executeCommand' request 'run/watch' to the language
 * server.
 * @param editor - Editor to watch.
 * @param callback - Called with the cancellation token received from the
 * language server, when the request has been acknowledged.
 */
function daedalusRunWatch(
	editor: vscode.TextEditor,
	callback: (lspCancellationToken: number) => void,
) {

	const c2p = client.code2ProtocolConverter

	const params: ExecuteCommandParams = {
		command: 'run/watch',
		arguments: [
			c2p.asTextDocumentIdentifier(editor.document),
			c2p.asPosition(editor.selection.active),
			c2p.asUri(editor.document.uri),
		],
	}

	client
		.sendRequest(
			ExecuteCommandRequest.type,
			params,
		)
		.then(callback)

}


/**
 * Sends the 'workspace/executeCommand' request 'run/cancel' to the language
 * server.
 * @param lspCancellationToken - Token received from an acknowledged 'run/watch'
 * command.
 * @param callback - Called after the request to cancel has been acknowledged.
 */
function daedalusRunCancel(
	lspCancellationToken: number,
	callback: () => void,
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
	).then(callback)
}


/**
 * Sends a watch request to the language server, and awaits either the
 * 'daedalus/run/watchResult' notification, or an intent from the user to cancel
 * the request.
 * @param editor - Editor to watch.
 * @param vscodeCancellationToken - VSCode token that will indicate if the user
 * tries to cancel the task.
 * @returns - Promise resolved upon completion or cancellation.
 */
function daedalusWatchProgress(
	editor: vscode.TextEditor,
	vscodeCancellationToken: vscode.CancellationToken,
): Promise<void> {

	return new Promise(resolve => {

		const cleanup = () => {
			disposable.dispose()
			resolve()
		}

		// Listen to watchResult while the request is flying and not cancelled.
		const disposable = client.onNotification(
			'daedalus/run/watchResult',
			({ clientHandle, result }) => {
				vscode.window.showInformationMessage(result)
				cleanup()
			}
		)

		daedalusRunWatch(
			editor,
			lspCancellationToken => {
				vscodeCancellationToken.onCancellationRequested(() => {
					daedalusRunCancel(lspCancellationToken, cleanup)
				})
			},
		)

	})

}
