/* --------------------------------------------------------------------------------------------
 * Copyright (c) Microsoft Corporation. All rights reserved.
 * Licensed under the MIT License. See License.txt in the project root for license information.
 * ------------------------------------------------------------------------------------------ */

import * as path from 'path';
import { workspace, ExtensionContext } from 'vscode';

import {
	LanguageClient,
	LanguageClientOptions,
	ServerOptions,
	TransportKind
} from 'vscode-languageclient/node';

import {
	configLanguageServerPath,
	getConfiguration,
} from './configuration'

let client: LanguageClient;

export function activate(context: ExtensionContext) {

	const configuration = getConfiguration()

	let serverExecutable = configuration[configLanguageServerPath]

	// If the extension is launched in debug mode then the debug server options are used
	// Otherwise the run options are used

	let serverOptions: ServerOptions = {
		run: { command: serverExecutable, transport: TransportKind.stdio },
		debug: { command: serverExecutable, transport: TransportKind.stdio }
	};

	// Options to control the language client
	let clientOptions: LanguageClientOptions = {
		documentSelector: [{ scheme: 'file', language: 'daedalus' }]
	};

	// Create the language client and start the client.
	client = new LanguageClient(
		'DaedalusLanguageServer',
		'DDL Language Server',
		serverOptions,
		clientOptions
	);

	// Start the client. This will also launch the server
	client.start();
}

export function deactivate(): Thenable<void> | undefined {
	if (!client) {
		return undefined;
	}
	return client.stop();
}
