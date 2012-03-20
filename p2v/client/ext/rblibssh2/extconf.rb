require 'mkmf'

# Allow compiler to be overridden by the environment
RbConfig::MAKEFILE_CONFIG['CC'] = ENV['CC'] if ENV['CC']

extension_name = 'rblibssh2'

raise "libssh2 not found" unless pkg_config('libssh2')

with_cflags("-Wall -Werror -g -O2") do
    dir_config(extension_name)
    create_makefile(extension_name)
end
