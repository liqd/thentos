from os.path import abspath, dirname, join
from subprocess import call

package_root = abspath(dirname(__file__))
exec_path = join(package_root, 'bin', 'thentos-adhocracy')
cfg_path = join(package_root, 'etc', 'thentos.yaml')

def main():
    """Start the thentos-adhocracy proxy."""
    call([exec_path, "--config", cfg_path])
