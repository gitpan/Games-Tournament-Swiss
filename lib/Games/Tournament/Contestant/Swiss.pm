package Games::Tournament::Contestant::Swiss;

# Last Edit: 2007 Aug 21, 01:23:41 PM
# $Id: $

use warnings;
use strict;

use base qw/Games::Tournament::Contestant/;

# use overload qw/0+/ => 'pairingNumber', qw/""/ => 'name', fallback => 1;

=head1 NAME

Games::Tournament::Contestant::Swiss  A competitor in a FIDE-Swiss-Rules event

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';

=head1 SYNOPSIS

    my $foo = Games::Tournament::Contestant::Swiss->new( rating => '15', name => 'Deep Blue', pairingNumber => 2 );
    ...

=head1 DESCRIPTION

Subclasses Games::Tournament::Contestant with Games::Tournament::Swiss-specific data and methods, like pairingNumber, floats.

Games::Tournament::Swiss will use this class when constructing a 'Bye' contestant.

=head1 METHODS

=head2 new

	Games::Tournament::Contestant::Swiss->new( rating => '15',
	    name => 'Red Chessman', pairingNumber => 2,
	    floats => [qw/None Down None None],
	    roles => [qw/Black White Black White/] );

Actually, you don't want to assign pairing numbers this way. Let the assignPairingNumbers method in Games::Tournament::Swiss do it.

=cut

sub new() {
    my $self = shift;
    my %args = @_;
    # $args{roles} = [] unless $args{roles};
    return bless \%args, $self;
}


=head2 preference

	$member->preference

Gets (sets) $member's preference, or right (duty) to take a role, eg White or Black, in the next round, calculated as a function of the difference between the number of games previously played in the different roles, and accommodated according to its value, Mild, Strong, or Absolute. An Absolute preference of +2 for White is given when the contestant has played 2 (or a larger number) more of the previous rounds as Black than as White, or when the last 2 rounds were played as Black. A Strong preference of +1 for White represents having played one more round as Black than as White. A Mild preference of +0 occurs when the number of games played with both colors is the same, but the last game was played as Black. A Mild preference of -0 is the same, but with the last game being as White, the preference is for Black. Preferences of -1 and -2 represent the same situations as for +1 and +2, but with the roles reversed. Before the first round, the preference of the highest ranked player (+-0) is determined by lot.  A7

=cut

sub preference {
    my $self = shift;
    my $preference = shift() || $self->{preference};
    $self->{preference} = $preference;
    return $preference;
}


=head2 pairingNumber

	$member->pairingNumber(1)

Sets/gets the pairing number of the contestant, used to identify participants when pairing them with others. This index is assigned in order of a sorting of the participants by ranking, title and name. You know what you're doing with this number, don't you?

=cut

sub pairingNumber {
    my $self = shift;
    $self->{pairingNumber} = shift if @_;
    $self->{pairingNumber};
}


=head2 oldId

	$member->oldId

Sets/gets an original, possibly unreliable id of the contestant, supplied before the Games::Tournament::Swiss::assignPairingNumbers function/method is applied, and the result substituted for it.

=cut

sub oldId {
    my $self  = shift;
    my $oldId = shift;
    if ( defined $oldId ) { $self->{oldId} = $oldId; }
    elsif ( $self->{oldId} ) { return $self->{oldId}; }
}

=head2 opponents

	$member->opponents( 0, 5, 11 )
	$rolehistory = $member->opponents

If ids (ie pairing numbers) are passed, adds them to the end of the list representing the latest opponents that $member has had in this tournament. (Normally one and only one parameter, the pairing number of the opponent in the latest round, will be passed.) If no parameter is passed, returns a reference to the list. If the member had no game or played no game, because of a bye, or an absence, pass 0, 'Bye' or 'Absence'.

=cut

sub opponents {
    my $self = shift;
    my @opponents = @_;
    if ( @opponents ) { push @{ $self->{opponents} }, @opponents; return }
    elsif ( $self->{opponents} ) { return $self->{opponents}; }
    else { return []; }
}


=head2 roles

	$member->roles( 'Black' )
	$rolehistory = $member->roles

If parameters are passed, adds them to the end of the list representing the latest roles that $member has had in this tournament. (Normally one and only one parameter, the role in the latest round, will be passed.) If no parameter is passed, returns a reference to the list. If the member had no game or played no game, because of a bye, or an absence, pass 'None'.

=cut

sub roles {
    my $self = shift;
    my @roles = @_;
    if ( @roles ) { push @{ $self->{roles} }, @roles; return }
    elsif ( $self->{roles} ) { return $self->{roles}; }
    else { return []; }
}


=head2 floating

        $member->floating
        $member->floating( 'Up'|'Down'|'' )

Sets/gets the direction in which the contestant is floating in the next round, "Up", "Down". If nothing is returned, the contestant is not floating. A4

=cut

sub floating {
    my $self      = shift;
    my $direction = shift;
    if ( defined $direction and $direction =~ m/^(?:Up|Down|)$/ ) {
        $self->{floater} = $direction;
    }
    elsif ( $self->{floater} ) { return $self->{floater}; }
}

=head2 floats

	$member->floats( $round, 'Down' )
	$rolehistory = $member->floats

If a round number and float is passed, inserts this in an anonymous array representing the old floats that $member has had in this tournament. If only a round is passed, returns the float for that round. If no parameter is passed,  returns a anonymous array of all the floats indexed by the round. (Watch out for round 0, there. Heh-hey.) If the player was not floated, pass 'None'.

=cut


sub floats {
    my $self  = shift;
    my $round = shift;
    my $float = shift;
    if ( defined $round and defined $float ) {
        $self->{floats}->[$round] = $float;
    }
    elsif ( defined $round )  { return $self->{floats}->[$round]; }
    elsif ( $self->{floats} ) { return $self->{floats}; }
}

=head2 importPairtableRecord

    $member->importPairtableRecord(
	{ opponents => [ 6,4 ]
	  roles => [ 'Win', 'Loss' ],
	  floats => [ undef, 'None', 'Down' ],
	  score => 1.5 } )

Populate $member with data about opponents met, roles played, and floats received in previous rounds, which together with the total score will allow it to be paired with an appropriate opponent in the next round. Set $member's preference. Delete any pre-existing opponents, roles, floats, scores, score, or preference data.

=cut


sub importPairtableRecord {
    my $self  = shift;
    my $record = shift;
    #die $self->name . ", " . $self->id . " pairtable record field lengths"
    #    unless @{$record->{opponents}} == @{$record->{roles}} and
    #    @{$record->{roles}} == @{$record->{floats}} - 1;
    my ($opponents, $roles, $floats) = @$record{qw/opponents roles floats/};
    delete @$self{qw/opponents roles floats scores score preference/};
    $self->opponents(@$opponents);
    $self->roles(@$roles);
    for my $i ( 0 .. $#$floats ) { $self->floats( $i, $floats->[$i] ); }
    use Games::Tournament::Contestant::Swiss::Preference;
    $self->preference(Games::Tournament::Contestant::Swiss::Preference->new);
    $self->preference->update( [ @$roles[0..$_] ] ) for 0.. $#$roles;
    $self->{score} = $record->{score};
    return;
}

=head1 AUTHOR

Dr Bean, C<< <drbean, followed by the at mark (@), cpan, then a dot, and finally, org> >>

=head1 BUGS

Please report any bugs or feature requests to
C<bug-games-tournament-contestant at rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Games-Tournament-Contestant-Swiss>.
I will be notified, and then you'll automatically be notified of progress on
your bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Games::Tournament::Contestant::Swiss

You can also look for information at:

=over 4

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Games-Tournament-Contestant-Swiss>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Games-Tournament-Contestant-Swiss>

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Games-Tournament-Contestant-Swiss>

=item * Search CPAN

L<http://search.cpan.org/dist/Games-Tournament-Contestant-Swiss>

=back

=head1 ACKNOWLEDGEMENTS

=head1 COPYRIGHT & LICENSE

Copyright 2006 Dr Bean, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut

1;    # End of Games::Tournament::Contestant::Swiss

# vim: set ts=8 sts=4 sw=4 noet:
