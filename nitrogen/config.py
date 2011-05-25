"""Given a nitrogen source directory, this script:
- builds nitrogen
- syncs local nitrogen directories to rel/nitrogen
- expands ./etc/vm.args.tmpl with appropriate variables for build environment
- create symlink from rel/nitrogen/site/include/schema.hrl to ../include/schema/hrl
- create symlink from rel/nitrogen/deps/subdomain to ../../subdomain
"""

import optparse
import os.path
import shutil
import subprocess
import sys

join = os.path.join
isdir = os.path.isdir

SRC_DIR = os.path.dirname(os.path.abspath(__file__))

TEMPLATE_SUFFIX = '.tmpl'

def shell(command):
    """Run shell command and return tuple of (stdout, stderr, returncode)"""
    proc = subprocess.Popen(command, shell=True,
                            stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    stdout, stderr = proc.communicate()
    return proc.returncode, stdout, stderr

def build_nitrogen(nitrogen_src_dir):
    print('building %s' % nitrogen_src_dir)
    return shell('cd %s && make rel_inets' % nitrogen_src_dir)

def sync(name, rel_dir):
    src_dir = join(SRC_DIR, name)
    assert isdir(src_dir)
    print('syncing %s to %s' % (src_dir, rel_dir))
    return shell('rsync -avP %s %s' % (src_dir, rel_dir))

def sync_src_to_rel(rel_dir):
    for name in [d for d in os.listdir(rel_dir) if isdir(d)]: #['etc', 'site']:
        #print(name)
        sync(name, rel_dir)

def inject_template_vars(template_file, vals):
    target_file = os.path.splitext(template_file)[0]
    #print(target_file)
    if os.path.exists(target_file):
        print('overwriting %s' % target_file)
    lines = [l.format(**vals) for l in open(template_file).readlines()]
    #print(lines)
    with open(target_file, 'w') as f:
        for l in lines:
            f.write(l)
        f.close()

def default_nitrogen_src_dir(platform=os.uname()[0]):
    if platform == 'Darwin':
        return os.path.expanduser('~/proj/sandbox/nitrogen.git')
    elif platform == 'Linux':
        return os.path.expanduser('~/pkg/nitrogen.git')

def assure_symlink(src, symlink):
    if not os.path.exists(symlink):
        print('creating symlink from %s to %s' % (src, symlink))
        os.symlink(src, symlink)
    assert os.path.islink(symlink)

def main():
    parser = optparse.OptionParser()
    parser.add_option('-n', '--nitrogen-src-dir', default=default_nitrogen_src_dir())
    opts, _args = parser.parse_args()

    assert isdir(opts.nitrogen_src_dir)
    retcode, out, err = build_nitrogen(opts.nitrogen_src_dir)
    assert retcode == 0
    print(out)
    nitrogen_rel_dir = join(opts.nitrogen_src_dir, 'rel/nitrogen')
    assert isdir(nitrogen_rel_dir)
    sync_src_to_rel(nitrogen_rel_dir)

    template_file = join(nitrogen_rel_dir, 'etc/vm.args%s' % TEMPLATE_SUFFIX)
    assert os.path.exists(template_file)
    vals = {
        'shorthostname': shell('hostname -s')[1].strip(),
        'cookie': open(os.path.expanduser('~/.erlang.cookie')).read()}
    print('injecting %s into template %s' % (vals, template_file))
    inject_template_vars(template_file, vals)

    schema_file = join(SRC_DIR, '../include/schema.hrl')
    assert os.path.exists(schema_file)
    schema_symlink = join(nitrogen_rel_dir, 'site/include/schema.hrl')
    assure_symlink(schema_file, schema_symlink)

    subdomain_dir = os.path.normpath(join(SRC_DIR, '../../subdomain'))
    assert isdir(subdomain_dir)
    deps_dir = join(nitrogen_rel_dir, 'deps')
    if not isdir(deps_dir):
        os.makedirs(deps_dir)
    subdomain_symlink = join(nitrogen_rel_dir, 'deps/subdomain')
    assure_symlink(subdomain_dir, subdomain_symlink)

    site_ebin_dir = join(nitrogen_rel_dir, 'site/ebin')
    assert isdir(site_ebin_dir)
    print('removing .beam files in %s' % site_ebin_dir) # so make will compile index.erl
    print(shell('cd %s && rm *.beam' % site_ebin_dir))

    print('making synced nitrogen')
    print(shell('cd %s && make' % nitrogen_rel_dir))

if __name__ == '__main__':
    sys.exit(main())
