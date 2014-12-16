## 2.2 (06/10/2013)
Bugfixes:
  - Fix plugin break in PASTE mode. This fixes #44.

## 2.1 (04/26/2013)

Bugfixes:
  - Fix 1 regression where cursors could potentially get out of sync in insert mode

Features:
  - Added some logic to debug latency. Fanning out to 30 cursors in insert mode with my vimrc took over 300ms. It's like than 20ms with a plain vimrc. Need to debug what setting is causing the slowing down in insert mode and inform users.

## 2.0 (04/24/2013)

Bugfixes:
  - Fix inconsistent undo behavior. Changes made in multicursor insert mode are now undone together. This fixes #22.
  - Single key commands that do not terminate properly no longer cause ghostly cursors to linger on screen. An error message is now displayed informing the user the number of cursor locations that the input cannot be properly played back at. This fixes #28.

## 1.16 (04/23/2013)

Features:
  - Add integration tests using vimrunner. Hook up travis-ci to run continous integration on commit.

## 1.15 (04/22/2013)

Bugfixes:
  - Fix plugin causing error bell. This fixes #29.

## 1.14 (04/22/2013)

Features:
  - Allow users to separate start key from next key. (credit: @xanderman)

## 1.13 (04/22/2013)

Bugfixes:
  - Add support for switching to visual line mode from inside multicursor mode
  - Fix highlight issue where extra character at end of line is highlighted for visual selections covering more than 2 lines.

## 1.12 (04/19/2013)

Bugfixes:
  - Fix tab character causing highlight errors. This fixes #18 and fixes #32

## 1.11 (04/18/2013)

Bugfixes:
  - Fix regression where `C-n` doesn't exhibit correct behavior when all matches have been found
  - Clear echo messages when a new input is received

## 1.10 (04/17/2013)

Bugfixes:
  - `O` works now in normal mode. This fixes #24
  - Turn on `lazyredraw` during multicursor mode to prevent the sluggish screen redraws

Features:
  - Add command **MultipleCursorsFind** to add multiple virtual cursors using regexp. This closes #20

## 1.9 (04/17/2013)

Bugfixes:
  - Fix starting multicursor mode in visual line mode. This fixes #25
  - Major refactoring to avoid getting in and out of visual mode as much as possible

## 1.8 (04/16/2013)

Bugfixes:
  - Fix regression that causes call stack to explode with too many cursors

## 1.7 (04/15/2013)

Bugfixes:
  - Finally fix the annoying highlighting problem when the last virtual cursor is on the last character of the line. The solution is a hack, but it should be harmless

## 1.6 (04/15/2013)

Bugfixes:
  - Stop chaining dictionary function calls. This fixes #10 and #11

## 1.5 (04/15/2013)

Bugfixes:
  - Exit Vim's visual mode before waiting for user's next input. This fixes #14

## 1.4 (04/14/2013)

Bugfixes:
  - Don't use clearmatches(). It clears highlighting from other plugins. This fixes #13

## 1.3 (04/14/2013)

Bugfixes:
  - Change mapping from using expression-quote syntax to using raw strings

## 1.2 (04/14/2013)

Bugfixes:
  - Restore view when exiting from multicursor mode. This fixes #5
  - Remove the unnecessary user level mapping for 'prev' and 'skip' in visual mode, since we can purely detect those keys from multicursor mode

## 1.1 (04/14/2013)

Bugfixes:
  - Stop hijacking escape key in normal mode. This fixes #1, #2, and #3

## 1.0 (04/13/2013)

Initial release
