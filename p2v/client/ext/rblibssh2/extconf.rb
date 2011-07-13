require 'mkmf'

# Allow compiler to be overridden by the environment
RbConfig::MAKEFILE_CONFIG['CC'] = ENV['CC'] if ENV['CC']

# XXX: Need to work out how to permanently enable -Werror
#RbConfig::MAKEFILE_CONFIG['cflags'] = '-Werror'

extension_name = 'rblibssh2'

raise "libssh2 not found" unless pkg_config('libssh2')

dir_config(extension_name)
create_makefile(extension_name)
