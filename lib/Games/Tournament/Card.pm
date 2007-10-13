package Games::Tournament::Card;

# Last Edit: 2007 Oct 12, 02:35:16 PM
# $Id: $

use warnings;
use strict;
use Carp;

use List::Util qw/min reduce sum/;

use constant ROLES => @Games::Tournament::Swiss::Config::roles;

=head1 NAME

Games::Tournament::Card - A record of the results of a match

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';

=head1 SYNOPSIS

    $action = Games::Tournament:Card->new(round => 1, contestants => {Black => $knicks, White => $deepblue}, result => { Black => 'Win', White => 'Loss' });

=head1 DESCRIPTION

In a tournament, matches take place in rounds between contestants, who are maybe floated, and who have roles, and there is a result for these matches, which can be written on a card. 

=head1 METHODS

=head2 new

    $action = Games::Tournament:Card->new(
	    round => 1,
	    contestants => {Black => $knicks, White => $deepblue},
	    result => { Black => 'Win', White => 'Loss' },
	    floats => { Black => 'Up', White => 'Down' }, or 
	    floats => { Black => 'Not', White => 'Not' }
    );
    $bye = Games::Tournament:Card->new(
	    round => 1,
	    contestants => {Bye => $player},
	    result => "Bye");

'contestants' is a hash ref of player objects, keyed on Black and White, or Home and Away, or some other role distinction that needs to be balanced over the tournament. The players are probably instances of the Games::Tournament::Contestant::Swiss class. 'result' is a hash reference, keyed on the same keys as contestants, containing the results of the match. 'floats' is a hash of  which role was floated up and which down. The default is neither contestant was floated, and 'Down' for a Bye. A4. What are the fields in NoShow and byes? NoShow has no special form. Bye is { Bye => $player }. TODO Perhaps the fields should be Winner and Loser, and Down and Up?

=cut 

sub new {
    my $self = shift;
    my %args = @_;
    return bless \%args, $self;
}


=head2 canonize

    $game->canonize

Fleshes out a partial statement of the result. From an abbreviated match result (eg, { Black => 'Win' }), works out a canonical representation (eg, { Black => 'Win', White => 'Loss' }). A bye result is represented as { Bye => 'Bye' }.

=cut 

sub canonize {
    my $self        = shift;
    my $round       = $self->round;
    my $contestants = $self->contestants;
    my $result      = $self->result;
    my %result;
    my %roles = map { $contestants->{$_}->{id} => $_ } keys %$contestants;
    warn
"Incomplete match of @{[values( %roles )]} players @{[map {$_->id} values %$contestants]} in round $round.\n"
      unless keys %roles == 2
      or grep m/bye/i, values %roles;
  ROLE: foreach my $contestant ( values %$contestants ) {
        my $role = $roles{ $contestant->{id} };
        if ( $role eq 'Bye' ) {
                $result{$role} = $result->{$role} = 'Bye';
            }
        elsif ( exists $result->{$role} ) {
            if ( $result->{$role} =~ m/^(?:Win|Loss|Draw|Absent)$/i ) {
                $result{$role} = $result->{$role};
            }
            else {
                warn
"$result->{$role} result for player $contestant->{id} in round $round";
            }
            next ROLE;
        }
        elsif ( values %$contestants != 1 ) {
            my @opponents =
              grep { $contestant->id ne $_->id } values %$contestants;
            my $opponent = $opponents[0];
            my $other    = $roles{ $opponent->id };
            if ( exists $result->{$other} ) {
                $result{$role} = 'Loss'
                  if $result->{$other} =~ m/^Win$/i;
                $result{$role} = 'Win'
                  if $result->{$other} =~ m/^Loss$/i;
                $result{$role} = 'Draw'
                  if $result->{$other} =~ m/^Draw$/i;
            }
            else {
                warn
"$result->{$role}, $result->{$other} result for player $contestant->{id} and opponent $opponent->{id} in round $round";
            }
        }
	else {
		die "Not a Bye, but no result or no partner";
	}
    }
    $self->result( \%result );
}


=head2 myResult

    $game->myResult($player)

Returns the result for $player from $game, eg 'Win', 'Loss' or 'Draw'.
TODO Should return 0,0.5,1 in numerical context.

=cut 

sub myResult {
    my $self       = shift;
    my $contestant = shift;
    $self->canonize;
    my $contestants = $self->contestants;
    my $result      = $self->result;
    my %result;
    my %roles = map { $contestants->{$_}->id => $_ } keys %$contestants;
    my $role = $roles{ $contestant->id };
    return $result->{$role};
}


=head2 myPlayers

    $game->myPlayers

Returns an array of the players from $game, eg ($alekhine, $yourNewNicks).

=cut 

sub myPlayers {
    my $self        = shift;
    my $contestants = $self->contestants;
    my @players     = values %$contestants;
    return @players;
}


=head2 myRole

    $game->myRole($player)

Returns the role for $player from $game, eg 'White', 'Banker' or 'Away'.

=cut 

sub myRole {
    my $self       = shift;
    my $contestant = shift;
    my $id = $contestant->id;
    my $contestants = $self->contestants;
    my @contestants = values %$contestants;
    my %dupes;
    for my $contestant ( @contestants )
    {
	die "Player $contestant isn't a contestant"
	unless $contestant and
		$contestant->isa('Games::Tournament::Contestant::Swiss');
    }
    my @dupes = grep { $dupes{$_->id}++ } @contestants;
    croak "Players @dupes had more than one role" if @dupes;
    my %roleReversal;
    for my $role ( keys %$contestants )
    {
	my $id = $contestants->{$role}->id;
	$roleReversal{$id} = $role;
    }
    my $role        = $roleReversal{ $id };
    carp "No role for player $id in round " . $self->round unless $role;
    return $role;
}


=head2 myFloat

    $game->myFloat($player)

Returns the float for $player in $game, eg 'Up', 'Down' or 'Not'.

=cut 

sub myFloat {
    my $self       = shift;
    my $contestant = shift;
    # $self->canonize;
    my $float = $self->float($contestant);
    return $float;
}


=head2 round

 $action->round

Returns the round in which the match is taking place.

=cut

sub round {
    my $self = shift;
    return $self->{round};
}


=head2 contestants

	$action->contestants

Gets/sets the participants as an anonymous array of player objects.

=cut

sub contestants {
    my $self        = shift;
    my $contestants = shift;
    if ( defined $contestants ) { $self->{contestants} = $contestants; }
    else { return $self->{contestants}; }
}


=head2 result

	$action->result

Gets/sets the results of the match.

=cut

sub result {
    my $self   = shift;
    my $result = shift;
    if ( defined $result ) { $self->{result} = $result; }
    else { return $self->{result}; }
}


=head2 float

	$card->float($player[,'Up|Down|Not'])

Gets/sets whether the player was floated 'Up', 'Down', or 'Not' floated.

=cut

sub float {
    my $self   = shift;
    my $player = shift;
    die "Player is $player ref"
      unless $player and $player->isa('Games::Tournament::Contestant::Swiss');
    my $role = $self->myRole($player);
    croak $player->id . " has $role role in round $self->{round}?"
      unless $role eq 'Bye'
      or $role     eq (ROLES)[0]
      or $role     eq (ROLES)[1];
    my $float = shift;
    if ( defined $float ) { $self->{floats}->{$role} = $float; }
    elsif ( $self->{floats}->{$role} ) { return $self->{floats}->{$role}; }
    elsif ( $role eq 'Bye' ) { return 'Down'; }
    else { return 'Not'; }
}

=head1 AUTHOR

Dr Bean, C<< <drbean, followed by the at mark (@), cpan, then a dot, and finally, org> >>

=head1 BUGS

Please report any bugs or feature requests to
C<bug-games-tournament-match at rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Games-Tournament-Card>.
I will be notified, and then you'll automatically be notified of progress on
your bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Games::Tournament::Card

You can also look for information at:

=over 4

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Games-Tournament-Card>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Games-Tournament-Card>

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Games-Tournament-Card>

=item * Search CPAN

L<http://search.cpan.org/dist/Games-Tournament-Card>

=back

=head1 ACKNOWLEDGEMENTS

=head1 COPYRIGHT & LICENSE

Copyright 2006 Dr Bean, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut

1;    # End of Games::Tournament::Card

# vim: set ts=8 sts=4 sw=4 noet:
