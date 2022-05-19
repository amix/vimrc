import { Attach, NeovimClient } from '@chemzqm/neovim';
interface IApp {
    refreshPage: ((param: {
        bufnr: number | string;
        data: any;
    }) => void);
    closePage: ((params: {
        bufnr: number | string;
    }) => void);
    closeAllPages: (() => void);
    openBrowser: ((params: {
        bufnr: number | string;
    }) => void);
}
interface IPlugin {
    init: ((app: IApp) => void);
    nvim: NeovimClient;
}
export default function (options: Attach): IPlugin;
export {};
