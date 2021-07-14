import * as vscode from 'vscode'

const configurationName = 'daedalus'

export const configLanguageServerPath = 'language-server-path'

export type Configuration = {
    [configLanguageServerPath]: string
}

export function getConfiguration(): Configuration {
    return vscode.workspace.getConfiguration(configurationName) as any
}
