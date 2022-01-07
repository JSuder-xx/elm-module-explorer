'use strict';

import * as vscode from 'vscode';

const Elm = require('./elm-type-explorer.min').Elm;
const main = Elm.Main.init();

type FunctionUML = {
    name: string
    , isExposed: boolean
    , lineNumber: number
    , typeAnnotation: string
}

const functionUMLByLineNumberAscending = (a: FunctionUML, b: FunctionUML) =>
    a.lineNumber - b.lineNumber;

type TypeUML = {
    name: string
    , lineNumber: number
    , inOutput: FunctionUML[]
    , inInput: FunctionUML[]
    , inInputAndOutput: FunctionUML[]
    , referencedBy: FunctionUML[]
}

type ModuleUML = {
    name: string
    , types: TypeUML[]
    , missingSignatures: FunctionUML[]
    , nomads: FunctionUML[]
}

export const extensionName = 'elmTypeExplorer'

type Node = vscode.TreeItem & { children?: Node[] }

const parentNode = (label: string, children: Node[]): Node =>
({
    label,
    collapsibleState: vscode.TreeItemCollapsibleState.Expanded,
    children
});

const conditionalParent = (label: string, children: Node[]): Node[] =>
    children.length === 0 ? [] : [parentNode(label, children)];

const functionUMLToNode = (iconPath: string) => (functionUML: FunctionUML): Node =>
({
    label: `${(functionUML.isExposed) ? '+' : '-'}${functionUML.name} ${functionUML.typeAnnotation ? (': ' + functionUML.typeAnnotation) : ''}`,
    iconPath: new vscode.ThemeIcon(iconPath),
    command: {
        command: 'revealLine',
        title: 'Goto',
        arguments: [{
            'lineNumber': functionUML.lineNumber,
            'at': 'top'
        }]
    }
});

const functionKinds = (icon: string, functionUMLList: FunctionUML[]): Node[] =>
    functionUMLList.sort(functionUMLByLineNumberAscending).map(functionUMLToNode(icon));


const typeHasChildren = (typeUML: TypeUML): boolean =>
    typeUML.inOutput.length > 0
    || typeUML.inInputAndOutput.length > 0
    || typeUML.inInput.length > 0
    || typeUML.referencedBy.length > 0;

const typeUMLToNode = (typeUML: TypeUML): Node =>
    parentNode(
        typeUML.name,
        [
            ...functionKinds('arrow-right', typeUML.inOutput)
            , ...functionKinds('arrow-both', typeUML.inInputAndOutput)
            , ...functionKinds('arrow-left', typeUML.inInput)
            , ...functionKinds('eye', typeUML.referencedBy)
        ]);

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
        main.ports.sendModule.subscribe((module: ModuleUML) => {
            this.moduleUML = module;
            this._onDidChangeTreeData.fire(undefined);
        });

        this.onActiveEditorChanged();
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
                    : [
                        ...moduleUML.types.filter(typeHasChildren).map(typeUMLToNode),
                        ...conditionalParent(
                            "[No Signatures]",
                            functionKinds('circle-slash', moduleUML.missingSignatures)
                        ),
                        ...conditionalParent(
                            "[Related to Outside Types]",
                            functionKinds('globe', moduleUML.nomads)
                        )
                    ]
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