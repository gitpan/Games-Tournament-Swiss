#!usr/bin/perl

# testing script_files/pairingtable

use lib qw/t lib/;

use strict;
use warnings;
use Test::More;
use YAML qw/Load LoadFile DumpFile/;
use File::Spec;
use File::Basename;

use Config;
my $secure_perl_path = $Config{perlpath};
if ($^O ne 'VMS')
{
	$secure_perl_path .= $Config{_exe}
		unless $secure_perl_path =~ m/$Config{_exe}$/i;
}

BEGIN {
    $Games::Tournament::Swiss::Config::firstround = 1;
    @Games::Tournament::Swiss::Config::roles      = qw/Black White/;
    %Games::Tournament::Swiss::Config::scores      = (
    Win => 1, Draw => 0.5, Loss => 0, Absence => 0, Bye => 1 );
    $Games::Tournament::Swiss::Config::algorithm  =
      'Games::Tournament::Swiss::Procedure::FIDE';
}
use Games::Tournament::Contestant::Swiss;
use Games::Tournament::Swiss;
use Games::Tournament::Swiss::Bracket;

my $orig_dir = File::Spec->rel2abs( '.' );

my @members = Load(<<'...');
---
id: 1
name: Your New Nicks
rating: 12
title: Unknown
---
id: 2
name: LaLa Lakers
rating: 8
title: Unknown
---
id: 3
name: Jose Capablanca
rating: 4
title: Unknown
---
id: 4
name: Alexander Alekhine
rating: 2
title: Unknown
...

DumpFile './league.yaml', {member => \@members};
mkdir '1';
chdir '1' or warn "No round 1 directory: $!";
system "$secure_perl_path ../script_files/pairstately";

chdir '..' or warn "No tourney directory: $!";
mkdir './scores';
my $scores = Load(<<'...');
---
0:
  'Your New Nicks': 1
  'Jose Capablanca': 0
1:
  'LaLa Lakers': 1
  'Alexander Alekhine': 0
...
DumpFile './scores/1.yaml', $scores;
my $table = qx{$secure_perl_path ./script_files/pairingtable};

my @tests = (
[ $table, qr/^		Round 2 Pairing Groups\n-------------------------------------------------------------------------\nPlace  No  Opponents     Roles     Float Score\n1-2\n       1   3              (W|B)             1\n       2   4              (W|B)             1\n3-4\n       3   1              (W|B)             0\n       4   2              (W|B)             0\n/m, 'round 1 table'],
);

mkdir '2';
chdir '2';
system "$secure_perl_path ../script_files/pairstately";
$scores = Load(<<'...');
---
0:
  'Your New Nicks': 1
  'LaLa Lakers': 0
1:
  'Jose Capablanca': 1
  'Alexander Alekhine': 0
...
chdir '..';
DumpFile './scores/2.yaml', $scores;
$table = qx{$secure_perl_path ./script_files/pairingtable};
push @tests, (
[ $table, qr/^		Round 3 Pairing Groups\n-------------------------------------------------------------------------\nPlace  No  Opponents     Roles     Float Score\n1\n       1   3,2            (WB|BW)            2\n2-3\n       2   4,1            (WB|BW)            1\n       3   1,4            (WB|BW)            1\n4\n       4   2,3            (WB|BW)            0\n/m,
'round 2 table'],
);

my @rounds = ( 1..2 );
my $pwd = File::Spec->rel2abs( '.' );
die "The pwd, $pwd is not the original $orig_dir." unless $pwd eq $orig_dir;
for my $dir ( 'scores', @rounds )
{
	chdir "$dir" or die "Cannot change to $dir";
	my @files = glob '*';
	unlink @files;
	chdir '..';
	rmdir "./$dir";
}
unlink './league.yaml', './league.yaml.bak';

plan tests => $#tests + 1;

map { like( $_->[0], $_->[ 1, ], $_->[ 2, ] ) } @tests;
