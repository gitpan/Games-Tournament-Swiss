package Games::Tournament::Swiss::Config;

# Last Edit: 2007 Aug 20, 10:20:23 PM
# $Id: $

use warnings;
use strict;

=head1 NAME

Games::Tournament::Swiss::Config - Swiss Competition Configuration

=head1 VERSION

Version 0.02

=cut

our $VERSION = '0.02';

=head1 SYNOPSIS

    use constant ROLES => @Games::Tournament::Swiss::Config::roles = qw/Black White/;
    use constant ROLES => @Games::Tournament::Swiss::Config::ROLES;
    $Games::Tournament::Swiss::Config::firstRound = 11;

=head1 DESCRIPTION

Actually, a swiss tournament is not just one kind of tournament, but a whole genre of tournaments. If you are using Games::Tournament::Swiss for other than chess tournaments, where the players take black and white roles, and score 0,0.5, or 1, for example, you probably want to configure it. You also might want to start swiss pairing at a random round in the tournament, in which case you will set firstround.

The roles, scores, firstround, algorithm methods in this module are here just to stop perl warning about 'only one use, possible typo' warnings, with the use of fully qualified Games::Tournament::Swiss::Config package variables.

=head1 METHODS

=head2 new

Getter/setter of the genre of competition, eg chess, basketball, football, school exam, etc, the tournament is being held as.

=cut

sub new {
    my $self = shift;
    my %args = @_;
    return bless \%args, $self;
}


=head2 frisk

Die if the configuration contains anything but [A-Za-z0-9:,.]

=cut

sub frisk {
    my $self    = shift;
    my $suspect = shift;
    unless ( ref $suspect ) {
        die "We are afraid you may be importing nasty characters with $suspect.
Please use only [A-Za-z0-9:.,] in your configuration files"
          unless $suspect =~ m/^[A-Za-z0-9:.,]*$/;
    }
    elsif ( ref($suspect) eq "ARRAY" ) {
        for (@$suspect) { $self->frisk($_); }
    }
    elsif ( ref($suspect) eq 'HASH' ) {
        for ( keys %$suspect ) { $self->frisk( $suspect->{$_} ); }
    }
    else {
        die "We are afraid you may be importing nasty objects with $suspect.
Please use only arrays and hashes in your configuration files";
    }
    return $suspect;
}


=head2 roles

Getter/setter of the roles the 2 players take, eg Black, White, or Home, Away. The default is Black, White.

=cut

sub roles {
    my $self  = shift;
    my $roles = shift;
    if ($roles) { $self->{roles} = $roles; }
    elsif ( $self->{roles} ) { return @{ $self->{roles} }; }
    else { return qw/White Black/; }
}


=head2 scores

Getter/setter of the scores the 2 players can get, eg win: 1, loss: 0, draw: 0.5, absent: 0, bye: 1, which is the default.

=cut

sub scores {
    my $self   = shift;
    my $scores = shift;
    if ($scores) { $self->{scores} = $scores; }
    elsif ( $self->{scores} ) { return %{ $self->{scores} }; }
    else { return ( win => 1, loss => 0, draw => 0.5, absent => 0, bye => 1 ) }
}


=head2 algorithm

Getter/setter of the algorithm by which swiss pairing is carried out. There is no default. Pass a name as a string. I recommend Games::Tournament::Swiss::Procedure::FIDE. Make sure something is set.

=cut

sub algorithm {
    my $self      = shift;
    my $algorithm = shift;
    die "$algorithm name is like Games::Tournament::Swiss::Procedure::AlgoName"
      unless $algorithm =~ m/^Games::Tournament::Swiss::Procedure::\w+$/;
    if ($algorithm) { $self->{algorithm} = $algorithm; }
    elsif ( $self->{algorithm} ) { return @{ $self->{algorithm} }; }
}


=head2 firstround

Getter/setter of the first round in which swiss pairing started. Perhaps some other pairing method was used in rounds earlier than this. The default is 1.

=cut

sub firstround {
    my $self       = shift;
    my $firstround = shift;
    if ($firstround) { $self->{firstround} = $firstround; }
    elsif ( $self->{firstround} ) { return @{ $self->{firstround} }; }
    else { return 1; }
}

=head1 AUTHOR

Dr Bean, C<< <drbean, followed by the at mark (@), cpan, then a dot, and finally, org> >>

=head1 BUGS

Please report any bugs or feature requests to
C<bug-games-tournament-swiss at rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Games-Tournament-Swiss>.
I will be notified, and then you'll automatically be notified of progress on
your bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Games::Tournament::Swiss::Config

You can also look for information at:

=over 4

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Games-Tournament-Swiss>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Games-Tournament-Swiss>

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Games-Tournament-Swiss>

=item * Search CPAN

L<http://search.cpan.org/dist/Games-Tournament-Swiss>

=back

=head1 ACKNOWLEDGEMENTS

See L<http://www.fide.com/official/handbook.asp?level=C04> for the FIDE's Swiss rules.

=head1 COPYRIGHT & LICENSE

Copyright 2006 Dr Bean, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut

1;    # End of Games::Tournament::Swiss::Config

# vim: set ts=8 sts=4 sw=4 noet:
