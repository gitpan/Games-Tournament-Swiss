package Games::Tournament::Contestant::Swiss::Preference;

# Last Edit: 2007 Aug 20, 10:17:28 PM
# $Id: $

use warnings;
use strict;

use constant ROLES => @Games::Tournament::Swiss::Config::roles;

use base qw/Games::Tournament/;

# use overload qw/0+/ => 'next', qw/""/ => 'value', fallback => 1;

=head1 NAME

Games::Tournament::Contestant::Swiss::Preference  A competitor's right to a role.

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';

=head1 SYNOPSIS

    pray if $preference->role eq 'Black' and $preference->strength eq 'Strong';

=head1 DESCRIPTION

The preference, or expectation/right/duty one has with reference to a role, eg White, in the next round depends on the difference between the number of games previously played in it and in the alternative roles, and is either Mild, Strong, or Absolute. The more games played in other roles than in this role, the greater the right/duty to play the next game in this role. The FIDE Swiss Rules (C04.1) represent the difference as the number of Games as White minus the number as Black, so a greater number of games as Black is a negative number and of White a positive number. For equal number of games, +0 indicates the last game was as White, and -0 indicates the last game was as Black. So +0 represents a Mild preference for Black and -0 for White. This implementation uses a 'direction' field to perform the same function as the +/- sign.
As an API, the strength method returns 'Mild', 'Strong', or 'Absolute' and the role method returns 'Black', 'White', or whatever the preferred role is, respecting the 2 consecutive games in the same role rule. A7

=head1 METHODS

=head2 new

    $pref = Games::Tournament::Contestant::Swiss::Preference->new(
	difference => 0, direction => 'Black', round => 0 );

The default round and difference are 0. The default direction is ''.

=cut

sub new {
    my $self = shift;
    my %args = @_;
    $args{direction}  = '' unless $args{direction};
    $args{difference} = 0  unless $args{difference};

    # $args{lastTwo} = ',' unless $args{lastTwo};
    my $pref = bless \%args, $self;
    return $pref;
}


=head2 update

	$pref->update( $oldRoles  )

	Updates the difference (ie, the internal representation of preference) on the basis of the last 2 given roles. Minimal sanity check is performed. $oldRoles is a history of roles in previous rounds. The two last roles are needed to determine the preference if the same role was taken in the last 2 games. (Is this true? $self->direction can be other than the last role?) Byes are passed over. TODO What about absences?

=cut

sub update {
    my $self     = shift;
    my $oldRoles = shift;
    return unless grep { $_ eq $oldRoles->[-1] } ROLES;
    my @reverseRoles = grep {
        my $role = $_;
        grep { $_ eq $role } ROLES
    } reverse @$oldRoles;
    my $lastRole       = $reverseRoles[0];
    my $before         = $reverseRoles[1];
    my $difference     = $self->difference;
    my $direction      = $self->direction;
    my $otherDirection = ( grep { $_ ne $direction } ROLES )[0];
    unless ( $direction and defined $difference ) {
        $direction  = $lastRole;
        $difference = 1;
    }
    elsif ( $lastRole ne $direction ) {
        if ( $difference >= 2 ) {
            $difference = 1;
        }
        elsif ( $difference == 1 ) {
            $difference = 0;
            $direction  = $otherDirection;
        }
        elsif ( $difference == 0 ) {
            $difference = 1;
            $direction  = $otherDirection;
        }
        elsif ( $direction eq '' ) {
            $difference = 1;
            $direction  = $lastRole;
        }
        else {
            die "$difference game lead as $direction after $lastRole role?";
        }
    }
    elsif ( $lastRole eq $direction ) {
        if ( $difference == 2 ) {
            warn "More than 2 games extra as $lastRole";
        }
        elsif ( $difference == 1 ) {
            $difference = 2;
        }
        elsif ( $difference == 0 ) {
            $difference = 1;
        }
        else {
            die "$difference difference for $direction after $lastRole role?";
        }
    }
    else {
        die
"$lastRole role update on ${difference}-game difference for $direction role?";
    }
    $self->direction($direction);
    $self->difference($difference);
    if ($before) { $self->lastTwo( [ $before, $lastRole ] ); }
    else { $self->lastTwo( [$lastRole] ); }
}


=head2 asString

	$pref->asString

	The difference as a string, ^[+-][012]$. '0' represents a mild preference, '1' a strong one and '2' an absolute one. '-' represents a preference for White, or the first element of @Games::Tournament::Swiss::Config::roles, and '+' represents a preference for Black or the second element.

=cut


sub asString {
    my $self   = shift;
    my $string = $self->direction eq (ROLES)[0] ? '-' : '+';

    #my $strength = $self->strength;
    #$string .= $strength eq 'Mild'? 0:
    #    	$strength eq 'Strong'? 1:
    #    	$strength eq 'Absolute'? 2:
    #    	die "$strength strength for " . $self->role . "preference?";
    $string .= $self->difference;
}


=head2 difference

	$pref->difference(2)

	Sets/gets the value of the difference in games played in one role over those played in other alternative roles. Equals either 0,1,2.

=cut


sub difference {
    my $self       = shift;
    my $difference = shift();
    $self->{difference} = $difference if defined $difference;
    return $self->{difference};
}


=head2 direction

	$pref->direction('Black')

Sets/gets the role which the player has taken more often, or more recently, than other alternative roles. The preference is thus for the other role.

=cut

sub direction {
    my $self = shift;
    my $direction = shift() || $self->{direction};
    $self->{direction} = $direction;
    return $direction;
}


=head2 strength

	$pref->strength

Gets the strength of the preference, 'Mild,' 'Strong,' or 'Absolute.'

=cut

sub strength {
    my $self      = shift;
    my @table     = qw/Mild Strong Absolute/;
    my $diff      = $self->difference;
    my $strength  = $table[$diff];
    my @lastRoles = @{ $self->lastTwo };
    if ( @lastRoles == 2 ) {
        $strength = 'Absolute' if $lastRoles[0] eq $lastRoles[1];
    }
    return $strength;
}


=head2 role

	$pref->role

Gets the role which the preference entitles/requires the player to take in the next round.

=cut

sub role {
    my $self = shift;
    my $role;
    $role = ( grep { $_ ne $self->direction } ROLES )[0]
      if $self->direction;
    return $role;
}


=head2 round

	$pref->round

Sets/gets the round in this game up to which play is used to calculate the preference . The default is 0.

=cut

sub round {
    my $self = shift;
    my $round = shift() || $self->{round};
    $self->{round} = $round;
    return $round;
}


=head2 lastTwo

	$pref->lastTwo

Sets/gets a list of the roles in the last 2 games. If the 2 roles are the same, there is an absolute preference for the other role. (Is that actually the definition of absolute preference?)

=cut

sub lastTwo {
    my $self    = shift;
    my $lastTwo = shift;
    if ( defined $lastTwo ) { $self->{lastTwo} = $lastTwo; }
    elsif ( $self->{lastTwo} ) { return $self->{lastTwo}; }
    else { return []; }
}

=head1 AUTHOR

Dr Bean, C<< <drbean, followed by the at mark (@), cpan, then a dot, and finally, org> >>

=head1 BUGS

Please report any bugs or feature requests to
C<bug-games-tournament-contestant at rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Games-Tournament-Swiss>.
I will be notified, and then you'll automatically be notified of progress on
your bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Games::Tournament::Contestant::Swiss::Preference

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

=head1 COPYRIGHT & LICENSE

Copyright 2006 Dr Bean, all rights reserved.

This program is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=cut

1;    # End of Games::Tournament::Contestant::Preference

# vim: set ts=8 sts=4 sw=4 noet:
