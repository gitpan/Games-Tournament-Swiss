use strict;
use warnings;
use Module::Build;

my $builder = Module::Build->new(
    module_name         => 'Games::Tournament::Swiss',
    license             => 'perl',
    dist_author         => 'Dr Bean <drbean, followed by the at mark (@), cpan, then a dot, and finally, org>',
    dist_version_from   => 'lib/Games/Tournament/Swiss.pm',
    create_makefile_pl  => 'traditional',
    build_requires => {
        'Test::More' => 0,
	'YAML' => 0,
	},
    script_files	=> [qw(
        		    script_files/pair
        		    script_files/pairingtable
        		    script_files/crosstable
        		    script_files/pairstately
        			)],
    add_to_cleanup      => [ 'Games-Tournament-Swiss-*' ],
			);

			$builder->create_build_script();
