from setuptools import setup

setup(
    name = 'thentos_adhocracy',
    version = '0.0.1.1',

    packages = ['thentos_adhocracy'],
    package_dir = {'thentos_adhocracy': 'thentos_adhocracy'},
    package_data = {'thentos_adhocracy': ['bin/thentos-adhocracy', 'etc/thentos.yaml']},
    zip_safe = False,

    entry_points = {
        'console_scripts': [
            'thentos_adhocracy = thentos_adhocracy:main'
        ]
    },

    author = 'liquid democracy e.V.',
    author_email = 'info@liqd.net',
    description = 'A tool for privacy-preserving identity management (PPIM)',
    license = 'AGPL',
    keywords = 'user data login identity privacy',
    url = 'http://github.com/liqd/thentos',
);
