import * as monaco from 'monaco-editor';
import { Elm } from './App.elm';
import './index.css';

const debounce = (func) => {
  let token;
  return function () {
    const later = () => {
      token = null;
      func.apply(null, arguments);
    };
    window.cancelIdleCallback(token);
    token = window.requestIdleCallback(later);
  };
};

class CustomEditor extends HTMLElement {
  _editor: monaco.editor.IStandaloneCodeEditor | undefined;
  _contents: string;
  _changed: boolean;
  _interval: number;

  constructor() {
    super();
    this._changed = false;
    this._contents = localStorage.getItem('camperdown_stored_contents') || '';
    this._interval = setInterval(() => {
      if (!this._changed) return;
      this._changed = false;
      localStorage.setItem('camperdown_stored_contents', this._contents);
    }, 30000);
  }

  connectedCallback() {
    if (this._editor) return;
    this._editor = monaco.editor.create(this, {
      value: this._contents,
      // language: 'typescript',
    });

    this._editor.onDidChangeModelContent(
      debounce(() => {
        const contents = this._editor.getModel().getValue();
        if (this._contents === contents) return;
        this._contents = this._editor.getModel().getValue();
        this._changed = true;
        this.dispatchEvent(new CustomEvent('editorChanged'));
      })
    );

    setTimeout(() => this.dispatchEvent(new CustomEvent('editorChanged')));
  }

  get editorContents() {
    return this._contents;
  }

  set editorContents(s: string) {
    if (!this._editor) return;
    if (typeof s !== 'string' || s === this._contents) return;
    this._editor.getModel().setValue(s);
  }
}
customElements.define('custom-editor', CustomEditor);

// @ts-ignore

self.MonacoEnvironment = {
  getWorkerUrl: function (moduleId, label) {
    return './build/worker.bundle.js';
  },
};

const app = Elm.App.init({
  node: document.querySelector('main'),
  flags: {
    width: window.innerWidth,
    height: window.innerHeight,
  },
});
console.log(app);

/* Boilerplate for requestIdleCallback */
type RequestIdleCallbackHandle = any;
type RequestIdleCallbackOptions = {
  timeout: number;
};
type RequestIdleCallbackDeadline = {
  readonly didTimeout: boolean;
  timeRemaining: () => number;
};

declare global {
  interface Window {
    requestIdleCallback: (
      callback: (deadline: RequestIdleCallbackDeadline) => void,
      opts?: RequestIdleCallbackOptions
    ) => RequestIdleCallbackHandle;
    cancelIdleCallback: (handle: RequestIdleCallbackHandle) => void;
  }
}
