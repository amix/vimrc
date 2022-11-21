" Author:      suoto <andre820@gmail.com>
" Description: Adds support for HDL Code Checker, which wraps vcom/vlog, ghdl
"              or xvhdl. More info on https://github.com/suoto/hdl_checker

call ale#handlers#hdl_checker#DefineLinter('verilog')
