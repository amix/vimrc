"""Used in jedi-vim's jedi#debug_info()"""
import sys

import vim
from jedi_vim import PythonToVimStr, jedi


def echo(msg):
    vim.command('echo %r' % PythonToVimStr(msg))


def echo_error(msg):
    vim.command('echohl ErrorMsg')
    echo(msg)
    vim.command('echohl None')


def format_exc_info(exc_info=None, tb_indent=2):
    import traceback

    if exc_info is None:
        exc_info = sys.exc_info()

    exc_msg = traceback.format_exception_only(exc_info[0], exc_info[1])
    lines = ''.join(exc_msg).rstrip('\n').split('\n')

    lines.append('Traceback (most recent call last):')
    tb = traceback.format_tb(exc_info[2])
    lines.extend(''.join(tb).rstrip('\n').split('\n'))

    indent = ' ' * tb_indent
    return '{0}'.format(('\n' + indent).join(lines))


def get_known_environments():
    """Get known Jedi environments."""
    envs = list(jedi.find_virtualenvs())
    envs.extend(jedi.find_system_environments())
    return envs


def display_debug_info():
    echo(' - global sys.executable: `{0}`'.format(sys.executable))
    echo(' - global sys.version: `{0}`'.format(
        ', '.join([x.strip()
                   for x in sys.version.split('\n')])))
    echo(' - global site module: `{0}`'.format(__import__('site').__file__))

    try:
        import jedi_vim
    except Exception:
        echo_error('ERROR: could not import jedi_vim: {0}'.format(
            format_exc_info()))
        return

    if jedi_vim.jedi is None:
        if hasattr(jedi_vim, 'jedi_import_error'):
            error_msg = format_exc_info(jedi_vim.jedi_import_error)
        else:
            error_msg = 'unknown error'
        echo_error('ERROR: could not import the "jedi" Python module: {0}'.format(
            error_msg))
    else:
        echo('\n##### Jedi\n\n - path: `{0}`'.format(jedi_vim.jedi.__file__))
        echo(' - version: {0}'.format(jedi_vim.jedi.__version__))

        try:
            project = jedi_vim.get_project()
            environment = project.get_environment()
        except AttributeError:
            script_evaluator = jedi_vim.jedi.Script('')._evaluator
            try:
                sys_path = script_evaluator.project.sys_path
            except AttributeError:
                sys_path = script_evaluator.sys_path
        else:
            echo('\n##### Jedi environment: {0}\n\n'.format(environment))
            echo(' - executable: {0}'.format(environment.executable))
            try:
                sys_path = environment.get_sys_path()
            except Exception:
                echo_error('ERROR: failed to get sys path from environment: {0}'.format(
                    format_exc_info()))
                return

        echo(' - sys_path:')
        for p in sys_path:
            echo('    - `{0}`'.format(p))

        if environment:
            echo('\n##### Known environments\n\n')
            for environment in get_known_environments():
                echo(' - {0} ({1})\n'.format(
                    environment,
                    environment.executable,
                ))
