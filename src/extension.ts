'use strict';

import * as vscode from 'vscode';

const Elm = require('./elm-type-explorer.min').Elm;
const main = Elm.Main.init();

type FunctionUML = {
    name: string
    , lineNumber: number
    , typeAnnotation: string
}

type TypeUML = {
    name: string
    , lineNumber: number
    , returnedBy: FunctionUML[]
    , projectionsOf: FunctionUML[]
    , updaters: FunctionUML[]
    , referencedBy: FunctionUML[]
}
type ModuleUML = { name: string, types: TypeUML[] }

export const extensionName = 'elmTypeExplorer'

type Node = vscode.TreeItem & { children?: Node[] }

const functionUMLToNode = (functionUML: FunctionUML): Node =>
({
    label: `${functionUML.name} : ${functionUML.typeAnnotation}`,
    command: {
        command: 'revealLine',
        title: 'Goto',
        arguments: [{
            'lineNumber': functionUML.lineNumber,
            'at': 'top'
        }]
    }
});

const functionKinds = (label: string, functionUMLList: FunctionUML[]): Node[] =>
    functionUMLList.length === 0 ?
        [] :
        [{
            label,
            collapsibleState: vscode.TreeItemCollapsibleState.Expanded,
            children: functionUMLList.map(functionUMLToNode)
        }];

const typeUMLToNode = (typeUML: TypeUML): Node => {
    const anyChildren = typeUML.returnedBy.length > 0
        || typeUML.updaters.length > 0
        || typeUML.projectionsOf.length > 0
        || typeUML.referencedBy.length > 0;
    return {
        label: typeUML.name,
        collapsibleState: anyChildren ? vscode.TreeItemCollapsibleState.Expanded : vscode.TreeItemCollapsibleState.None,
        children: [
            ...functionKinds('Return', typeUML.returnedBy),
            ...functionKinds('Update', typeUML.updaters),
            ...functionKinds('Project', typeUML.projectionsOf),
            ...functionKinds('Mention', typeUML.referencedBy)
        ]
    };
}

export class ElmTypeExplorerProvider implements vscode.TreeDataProvider<Node> {

    private readonly _onDidChangeTreeData: vscode.EventEmitter<undefined> = new vscode.EventEmitter<undefined>();
    readonly onDidChangeTreeData: vscode.Event<undefined> = this._onDidChangeTreeData.event;

    private moduleUML: ModuleUML | null = null;
    private text: string;
    private editor: vscode.TextEditor;
    private autoRefresh = true;

    constructor(private _context: vscode.ExtensionContext) {
        vscode.window.onDidChangeActiveTextEditor(() => this.onActiveEditorChanged());
        vscode.workspace.onDidChangeTextDocument(e => this.onDocumentChanged(e));
        this.autoRefresh = vscode.workspace.getConfiguration(extensionName).get('autorefresh');
        vscode.workspace.onDidChangeConfiguration(() => {
            this.autoRefresh = vscode.workspace.getConfiguration(extensionName).get('autorefresh');
        });
        this.onActiveEditorChanged();

        main.ports.sendModule.subscribe((module: ModuleUML) => {
            this.moduleUML = module;
            this._onDidChangeTreeData.fire(undefined);
        });
    }

    refresh(): void {
        this.parseFile();
    }

    private onActiveEditorChanged(): void {
        if (vscode.window.activeTextEditor) {
            if (vscode.window.activeTextEditor.document.uri.scheme === 'file') {
                const enabled = vscode.window.activeTextEditor.document.languageId === 'elm';
                vscode.commands.executeCommand('setContext', `${extensionName}Enabled`, enabled);
                if (enabled) {
                    this.refresh();
                }
            }
        } else {
            vscode.commands.executeCommand('setContext', extensionName, false);
        }
    }

    private onDocumentChanged(changeEvent: vscode.TextDocumentChangeEvent): void {
        if (this.autoRefresh && changeEvent.document.uri.toString() === this.editor.document.uri.toString()) {
            this.refresh();
        }
    }

    private parseFile(): void {
        this.text = '';
        this.moduleUML = null;
        this.editor = vscode.window.activeTextEditor;
        if (this.editor && this.editor.document) {
            this.text = this.editor.document.getText();
            main.ports.onModuleString.send(this.text);
        }
    }

    getChildren(element?: Node): Thenable<Node[]> {
        const { moduleUML } = this;
        return Promise.resolve(
            element
                ? element.children
                : (!moduleUML)
                    ? []
                    : moduleUML.types.map(typeUMLToNode)
        );
    }

    getTreeItem(node: Node): vscode.TreeItem {
        return node;
    }
}

export function activate(context: vscode.ExtensionContext) {
    const provider = new ElmTypeExplorerProvider(context);
    vscode.window.registerTreeDataProvider(extensionName, provider);
    vscode.commands.registerCommand(`${extensionName}.refresh`, () => provider.refresh());
}