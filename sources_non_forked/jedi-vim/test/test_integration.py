"""Runs tests from ./vspec in vim-vspec."""
import os
import subprocess
try:
    from urllib.request import urlretrieve
except ImportError:
    from urllib import urlretrieve
import zipfile

import pytest

vspec_version = '1.9.0'

VSPEC_URL = 'https://github.com/kana/vim-vspec/archive/%s.zip' % vspec_version
root = os.path.dirname(os.path.dirname(__file__))
CACHE_FOLDER = os.path.join(root, 'build')
VSPEC_FOLDER = os.path.join(CACHE_FOLDER, 'vim-vspec-%s' % vspec_version)
VSPEC_RUNNER = os.path.join(VSPEC_FOLDER, 'bin/vspec')
TEST_DIR = os.path.join(root, 'test', 'vspec')


@pytest.fixture(scope='session')
def install_vspec():
    if not os.path.isdir(CACHE_FOLDER):
        os.mkdir(CACHE_FOLDER)

    if not os.path.exists(VSPEC_FOLDER):
        name, hdrs = urlretrieve(VSPEC_URL)
        z = zipfile.ZipFile(name)
        for n in z.namelist():
            dest = os.path.join(CACHE_FOLDER, n)
            destdir = os.path.dirname(dest)
            if not os.path.isdir(destdir):
                os.makedirs(destdir)
            data = z.read(n)
            if not os.path.isdir(dest):
                with open(dest, 'wb') as f:
                    f.write(data)
        z.close()
        os.chmod(VSPEC_RUNNER, 0o777)


def get_vspec_tests():
    for f in os.listdir(TEST_DIR):
        yield os.path.relpath(os.path.join(TEST_DIR, f))


@pytest.mark.parametrize('path', get_vspec_tests())
def test_integration(install_vspec, path):
    output = subprocess.check_output(
        [VSPEC_RUNNER, '.', VSPEC_FOLDER, os.path.relpath(path, root)],
        cwd=root,
    )
    had_ok = False
    for line in output.splitlines():
        if (line.startswith(b'not ok') or
                line.startswith(b'Error') or
                line.startswith(b'Bail out!')):
            pytest.fail(u"{0} failed:\n{1}".format(
                path, output.decode('utf-8')), pytrace=False)
        if not had_ok and line.startswith(b'ok'):
            had_ok = True
    if not had_ok:
        pytest.fail(u"{0} failed: no 'ok' found:\n{1}".format(
            path, output.decode('utf-8')), pytrace=False)
