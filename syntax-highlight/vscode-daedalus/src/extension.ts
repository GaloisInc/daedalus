import * as vscode from 'vscode'
import * as vsc from 'vscode-languageclient/node'


let client: vsc.LanguageClient

export async function activate(_context: vscode.ExtensionContext) {

	const cfg = vscode.workspace.getConfiguration("daedalus")
	const serverExe: string = cfg.get("language-server-path", "daedalus-language-server")

	const srvCfg = { command: serverExe, transport: vsc.TransportKind.stdio }
	const srvOpt: vsc.ServerOptions = { run: srvCfg, debug: srvCfg }
	const cltOpt: vsc.LanguageClientOptions = {
		documentSelector: [{ scheme: 'file', language: 'daedalus' }]
	}

	client = new vsc.LanguageClient("daedalus", "Daedalus Language Server", srvOpt, cltOpt)

	try {
		await client.start()
	} catch (err) {
		vscode.window.showErrorMessage(`Daedalus language server failed to start: ${err}`)
	}
}


export function deactivate() {
	if (!client) { return }
	return client.stop()
}
