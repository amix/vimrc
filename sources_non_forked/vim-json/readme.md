

Better JSON for VIM
===================

*Distinct highlighting of keywords vs values, JSON-specific (non-JS) warnings, quote concealing.*  
![JSON syntax coloring](https://cloud.githubusercontent.com/assets/183877/7018898/98e428e0-dccf-11e4-9ab8-c554b3556155.jpg)
Customization of Jeroen Ruigrok van der Werven's [vim-json highlighting script](http://www.vim.org/scripts/script.php?script_id=1945) with Rogerz Zhang's [indent script](https://github.com/vim-scripts/vim-json-bundle).  
[Pathogen-friendly.](https://github.com/tpope/vim-pathogen) [Vundle-friendly too.](https://github.com/elzr/vim-json/issues/25) ([Or install it manually.](https://github.com/elzr/vim-json/issues/52)) 

Specific customizations
-----------------------

* Added distinct **highlighting** for keywords vs values! (This is what made me start this plugin.)
* Added **concealing** of double quotes, for a minimalist [CoffeeScript](http://coffeescript.org/)-inspired look ([CSON](https://github.com/bevry/cson)!). 
	* ![image](https://cloud.githubusercontent.com/assets/183877/6786803/18143984-d154-11e4-841c-134241f951ae.png)<br> [Strings are colored differently than numbers & booleans.](https://github.com/elzr/vim-json/issues/37) The disambiguating purpose of double quotes is thus achieved with colors for a cleaner look.
	* *This requires Vim 7.3+.* To disable it add `let g:vim_json_syntax_conceal = 0` to your `.vimrc`.
* Added **folding** of `{...}` and `[...]` blocks. To enable it `:setlocal foldmethod=syntax` (do it permanently on the `ftplugin/json.vim` file).
* **JSON-specific warnings** (red highlights):
	* Warn about *no-quotes* in keywords and *single-quotes* in keywords and values.
	* Warn about *decimals* smaller than 1 that don't start with a 0 (`.1` gives a warning, it should be `0.1`).
	* Warn about *comments* `//` and *trailing semicolons* `;`.
	* Warn about *[missing commas](https://github.com/elzr/vim-json/issues/18)* between elements of an object [and elsewhere](https://github.com/elzr/vim-json/issues/34).
	* Warn about *trailing commas* after the last element in arrays or objects.
	* (All warnings can be turned off with a `let g:vim_json_warnings=0` in your `vimrc`.) 
* Recognize `.jsonp` file type. In [JSONP](http://stackoverflow.com/questions/2067472/what-is-jsonp-all-about), the wrapping function call at the beginning and the closing semicolon are recognized.

Screenshots
-----------

**No syntax coloring**: wall of text.<br>![No syntax coloring](https://cloud.githubusercontent.com/assets/183877/7018892/8c9965a0-dccf-11e4-9790-0e815605e3a9.jpg)

**Javascript (or for that matter, the standard json.vim) syntax coloring**: Barely an improvement from no syntax coloring.<br>![Javascript syntax coloring](https://cloud.githubusercontent.com/assets/183877/7018893/906e67c0-dccf-11e4-89b1-11c3cfe9e2ef.jpg)

**Better JSON syntax coloring**: a more meaningful, distinct and elegant presentation.<br>![JSON syntax coloring](https://cloud.githubusercontent.com/assets/183877/7018894/95fd2c1c-dccf-11e4-8cbc-0f6588f9d060.jpg)

Why use separate JSON highlighting instead of just Javascript?
--------------------------------------------------------------

Here's 2 compelling reasons:

1. **All JSON is Javascript but NOT all Javascript is JSON.** So `{property:1}` is invalid because `property` does not have double quotes around it. `{'property':1}` is also invalid, because it's single quoted while the only thing that can placate the JSON specification is double quoting. JSON is even fussy enough that `{"property":.1}` is invalid too, because you should have of course written `{"property":0.1}`. Also, don't even think about [having comments](http://stackoverflow.com/questions/244777/can-i-comment-a-json-file) or semicolons, you guessed it: they're invalid. The point being that your syntax highlighter should warn you about these errors, in realtime, which is something that the Javascript highlighter doesn't (because in Javacript they're not errors!).

2. **Distinct highlighting for keywords.** JSON is an extremely lightweight data format but at its core lies an inescapable conceptual distinction: there are keywords and there are values. There's nothing much to the format besides that, so we might as well display keywords and values differently. This is something that gets lost with Javascript-inspired syntax highlighters, which see keywords as just another string since JSON requires them double quoted. So JSON files remain an impenetrable, indistinct wall of text.

Common problems
---------------

This is the expected behavior of the plugin:  
![showcase](http://i.imgur.com/cmL1GNc.gif)

Most trouble, little as it is, has to do with Vim's newfangled concealing, which most people aren't yet familiar with, as it was introduced as recently as Vim 7.3+. If you just don't care for concealing you can easily disable it adding `let g:vim_json_syntax_conceal = 0` to your `.vimrc`.

Concealing is nice for viewing but when you want to edit it should get out of your way seamlessly so you can see the actual code. Thus the **default behavior** *should* be text shown normally on the line your cursor is at, on all modes (normal, visual, insert). If this isn't the case and the concealing doesn't go away (not even in insert mode), you most likely have **an interfering plugin**. You need to look at your `concealcursor` setting (which can be set through this plugin with `g:vim_json_syntax_concealcursor`). The specially overeager [**indentLine**](https://github.com/Yggdroot/indentLine), plugin would require _yet_ an additional `let g:indentLine_noConcealCursor=""` in your `.vimrc` as detailed [here](https://github.com/elzr/vim-json/issues/23#issuecomment-40293049).

It's a good idea to test drive with the files `json-test.json` and `jsonp-test.jsonp` first thing.

Other recommended software
--------------------------
* [JSON Formatter](https://chrome.google.com/webstore/detail/json-formatter/bcjindcccaagfpapjjmafapmmgkkhgoa): Chrome extension for printing JSON and JSONP nicely when you visit it 'directly' in a browser tab.
* [Convert JSON to YAML](http://www.json2yaml.com/): side by side, real-time conversion of the "fat and rigid" JSON to the "skinny and flexible" YAML.
