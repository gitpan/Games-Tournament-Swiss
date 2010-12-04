
use strict;
use warnings;



use ExtUtils::MakeMaker 6.31;



my %WriteMakefileArgs = (
  'ABSTRACT' => 'FIDE Swiss Same-Rank Contestant Pairing',
  'AUTHOR' => 'Dr Bean <drbean at (a) cpan dot (.) org>',
  'BUILD_REQUIRES' => {},
  'CONFIGURE_REQUIRES' => {
    'ExtUtils::MakeMaker' => '6.31'
  },
  'DISTNAME' => 'Games-Tournament-Swiss',
  'EXE_FILES' => [
    'script_files/pairingtable',
    'script_files/pairstately',
    'script_files/pair.pl',
    'script_files/collectGames',
    'script_files/pair',
    'script_files/rematch',
    'script_files/crosstable',
    'script_files/whoMetWho',
    'script_files/markCards',
    'script_files/tables',
    'script_files/pairyaml2txt',
    'script_files/pairtable2yaml'
  ],
  'LICENSE' => 'perl',
  'NAME' => 'Games::Tournament::Swiss',
  'PREREQ_PM' => {},
  'VERSION' => '0.18',
  'test' => {
    'TESTS' => 't/*.t'
  }
);


unless ( eval { ExtUtils::MakeMaker->VERSION(6.56) } ) {
  my $br = delete $WriteMakefileArgs{BUILD_REQUIRES};
  my $pp = $WriteMakefileArgs{PREREQ_PM};
  for my $mod ( keys %$br ) {
    if ( exists $pp->{$mod} ) {
      $pp->{$mod} = $br->{$mod} if $br->{$mod} > $pp->{$mod};
    }
    else {
      $pp->{$mod} = $br->{$mod};
    }
  }
}

delete $WriteMakefileArgs{CONFIGURE_REQUIRES}
  unless eval { ExtUtils::MakeMaker->VERSION(6.52) };

WriteMakefile(%WriteMakefileArgs);



