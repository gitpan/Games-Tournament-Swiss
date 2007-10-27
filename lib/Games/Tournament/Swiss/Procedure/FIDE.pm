package Games::Tournament::Swiss::Procedure::FIDE;

# Last Edit: 2007 Oct 27, 11:32:34 AM
# $Id: /swiss/trunk/lib/Games/Tournament/Swiss/Procedure/FIDE.pm 1533 2007-10-27T03:40:00.331146Z greg  $

use warnings;
use strict;
use Carp;

use List::Util qw/first/;
use List::MoreUtils qw/all/;

use constant ROLES      => @Games::Tournament::Swiss::Config::roles;
use constant FIRSTROUND => $Games::Tournament::Swiss::Config::firstround;

use base qw/Games::Tournament::Swiss/;
use Games::Tournament::Contestant::Swiss;

use constant    C1        => 'C1';
use constant    C2        => 'C2';
use constant    C3        => 'C3';
use constant    C4        => 'C4';
use constant    C5        => 'C5';
use constant    C6PAIRS   => 'C6PAIRS';
use constant    C6OTHERS  => 'C6OTHERS';
use constant    C7        => 'C7';
use constant    C8        => 'C8';
use constant    C9        => 'C9';
use constant    C10       => 'C10';
use constant    C11       => 'C11';
use constant    C12       => 'C12';
use constant    C13       => 'C13';
use constant    BYE       => 'bye';
use constant    C14       => 'C14';
use constant    FLOAT     => "FLOAT";
use constant    START     => "START";
use constant    LAST      => "LAST";
use constant    ERROR     => "ERROR";
use constant    MATCH     => "MATCH";
use constant    NEXT      => "NEXT";
use constant    PREV      => "PREV";

=head1 NAME

Games::Tournament::Swiss::Procedure::FIDE - FIDE Swiss Rules Based on Rating 04.1

=head1 VERSION

Version 0.12

=cut

our $VERSION = '0.12';


=head1 SYNOPSIS

 $tourney = Games::Tournament::Swiss->new( rounds => 2, entrants => [ $a, $b, $c ] );
 %groups = $tourney->formBrackets;
 $pairing = $tourney->pairing( \%groups );
 @pairs = $pairing->matchPlayers;


    ...

=head1 DESCRIPTION

FIDE Swiss Rules C 04.1 Based on Rating describes an algorithm to pair players. The algorithm starts with the highest bracket, and then pairs each bracket in turn. ending with the lowest bracket, floating players up and down to find acceptable matches, but also undoing pairings in higher score groups, if this will help the pairing of lower score groups. This module pairs players on the basis of that algorithm.

=head1 METHODS

=head2 new

 $pairing = Games::Tournament::Swiss::Procedure->new( \@groups );

Creates a FIDE C 04.1 algorithm object on a reference to a list of scoregroups ordered by score, the group with highest score first, the one with lowest score last. This object has a matches accessor to the games (cards) the algorithm has made, an incompatibles accessor to previous matches ofthe players, a stack of groups previous to this one at this point in the pairing to which we can go back and XXX. This constructor is called in the Games::Tournament::Swiss::pairing method.

=cut 

sub new {
    my $self     = shift;
    my %args     = @_;
    my $round    = $args{round};
    my $brackets = $args{brackets};
    my $banner   = "Round $round:  ";
    for my $bracket ( reverse sort keys %$brackets ) {
        my $members = $brackets->{$bracket}->members;
        my $score   = $brackets->{$bracket}->score;
        $banner .= "@{[map { $_->id } @$members]} ($score), ";
    }
    print $banner . "\n";
    return bless {
        round        => $round,
        brackets     => $brackets,
        whoPlayedWho => $args{whoPlayedWho},
        colorClashes => $args{colorClashes},
        badpair      => undef,
        byes         => $args{byes},
        matches      => {},
	previousBracket => [],
	logged => {}
      },
      "Games::Tournament::Swiss::Procedure";
}

=head2 matchPlayers

 @pairs = $pairing->matchPlayers;

Run the FIDE C 04.1 algorithm adding matches to $pairing->matches. NOTE: At one point in deveopment of this module, I was passing round the args, rather than storing them in the object, because of problems with storing. What were those problems? What does matchPlayers return? Is it a hash or the matches or what?
=cut 

sub matchPlayers {
    my $self    = shift;
    my %machine = (
        START, [ \&next, C1, LAST ],
        C1, [ \&c1, C2, NEXT, C13, C12 ],
        C2,      [ \&c2,      C3 ],
        C3,      [ \&c3,      C4 ],
        C4,      [ \&c4,      C5 ],
        C5,      [ \&c5,      C6PAIRS ],
        C6PAIRS, [ \&c6pairs, C6OTHERS, C7, NEXT ],
        C6OTHERS, [ \&c6others, NEXT, C1, C2, BYE ],
        C7, [ \&c7, C6PAIRS, C8, C9, C10 ],
        C8,  [ \&c8,  C5, C9,  C10 ],
        C9,  [ \&c9,  C4, C10 ],
        C10, [ \&c10, C7, C4,  C11 ],
        C11, [ \&c11, C12, C3, C7 ],
        C12,    [ \&c12,    C13, C7, ],
        C13,    [ \&c13,    C14, C7, C1, BYE ],
        C14,    [ \&c14,    NEXT, C4, C13 ],
	BYE,	[ \&bye,    LAST, C13 ],
        NEXT,   [ \&next,   C1, LAST ],
        PREV,   [ \&prev,   C1, LAST ],
        LAST,  [ undef, LAST ],
        ERROR, [ undef, ERROR ],
    );
    my $state    = START;
    my $oldState = $state;
    my %args     = %$self;
    for ( ; ; ) {
        my $transitions = $machine{$state};
	die "$oldState, $state, $transitions" unless $transitions and ref($transitions) eq 'ARRAY';
        my ( $action, @alterStates ) = @$transitions;
        $oldState = $state;
        ( $state, %args ) = $action->( $self, %args ) if $action;
	if ( $self->loggedProcedures->{$oldState} )
	{
	    my %log = $self->tailLog($oldState);
	    print $log{$oldState} if %log;
	}
        if ( $state eq ERROR ) {
            die
qq/Pairing error: @{[map { "$_ => $args{$_}, " } keys %args]} Pairing NOT complete\n/;
        }
        if ( $state eq LAST ) { print "Pairing complete\n"; return $self; }
        die "No transition defined from $oldState to $state"
          unless grep m/$state/, @alterStates;
    }
}


=head2 next

 $pairing->next;

Pair the next bracket. End if this is the last bracket. If we are not pairing any bracket now, set the next bracket to the firstBracket.

=cut 

sub next {
    my $self  = shift;
    my $index = $self->thisBracket;
    my $groups = $self->brackets;
    return LAST if defined $index and $index eq $self->lastBracket;
    my $next    = $self->nextBracket;
    $next = $self->firstBracket unless defined $next;
    my $nextBracket = $groups->{$next};
    die "Next bracket is: $next Bracket?" unless defined $nextBracket
		    and $nextBracket->isa('Games::Tournament::Swiss::Bracket');
    my $members = $nextBracket->members;
    my @ids = map {$_->id} @$members;
    my $number = $nextBracket->number;
    $self->thisBracket($next);
    $self->log( "Bracket $number ($next): @ids" );
    return C1;
}


=head2 prev

 $pairing->prev;

Pair the previous bracket. End if this is the first bracket.

=cut 

sub prev {
    my $self  = shift;
    my $brackets = $self->brackets;
    my $index = $self->thisBracket;
    my $bracket = $brackets->{$index};
    return LAST if defined $index and $index eq $self->firstBracket;
    my $prevIndex = $self->previousBracket;
    my $prevBracket = $brackets->{$prevIndex};
    my $members = $prevBracket->members;
    my $number = $prevBracket->number;
    $self->thisBracket($prevIndex);
    my @ids = map {$_->id} @$members;
    $self->log( "Previous, Bracket $number ($prevIndex): @ids");
    return C1;
}


=head2 c1

 $pairing->c1;

If the score group contains a player for whom no opponent can be found (B1,B2), and if this player is a downfloater, go to C12 to find another player to downfloat instead. Or if this is the last group, go to C13. Otherwise, downfloat the unpairable player.

=cut 

sub c1 {
    my $self          = shift;
    my $groups        = $self->brackets;
    my $alreadyPlayed = $self->whoPlayedWho;
    my $colorClashes  = $self->colorClashes;
    my $index = $self->thisBracket;
    my $group         = $groups->{$index};
    my $number         = $group->number;
    my $members       = $group->residents;
    my @unpairables;
    my $nokmessage = 'NOK.';
    if ( @$members == 1 ) {
	my $member = $members->[0];
        push @unpairables, $member;
	my $id = $member->id;
	$nokmessage .= " $id";
	$self->log( $nokmessage . " only member in $number ($index)." );
    }
    else {
	  for my $player (@$members) {
	  my $id = $player->id;
            my $rejections  = 0;
            my @candidates = grep { $_ != $player } @$members;
            my @ids       = map { $_->pairingNumber } @candidates;;
            foreach my $candidate ( @ids ) {
		if ( $alreadyPlayed->{$id}->{$candidate} ) { $rejections++; }
		elsif ( $colorClashes->{$id}->{$candidate} ) { $rejections++; }
            }
            if ( $rejections >= @candidates or @candidates == 0 ) {
                $nokmessage .= " $id";
                push @unpairables, $player;
            }
        }
    }
    if (@unpairables) {
        if ( $index eq $self->lastBracket ) {
	    $self->log( $nokmessage . " in last bracket, $number ($index)." );
            return C13;
        }
        elsif ((grep {$_->floating and $_->floating eq 'Down'} @unpairables)
		    and @{$groups->{$self->previousBracket}->members})
	{
	    $self->log(
		$nokmessage . ": partnerless floaters in $number($index)" );
	    return C12;
        }
        else {
	    my $next = $self->nextBracket;
	    my $nextBracket = $groups->{$next};
	    my $nextNumber = $nextBracket->number;
	    $self->log( $nokmessage . ": floating down from $number" );
	    $group->exit($_) for @unpairables;
	    $_->floating('Down') for @unpairables;
	    $nextBracket->entry($_) for @unpairables;
	    my @originals = map {$_->id} @{$group->members};
	    my @new = map {$_->id} @{$nextBracket->members};
            $self->log( "[$number] @originals & [$nextNumber] @new" );
            if ( @unpairables == @$members ) {
		$self->log( "Bracket $number ($index) dissolved." );
		$next = $self->nextBracket;
		$self->thisBracket($next);
		$group->dissolved(1);
		return C2;
	    }
	    else { return C2; }
        }
    }
    else {   
	$self->log( "B1,2 test: OK, no unpairables" );
	return C2;
    }
    return ERROR, msg => "Fell through C1 in $number ($index)";
}




=head2 c2

 $pairing->c2

Determine x according to A8.

=cut 

sub c2 {
    my $self   = shift;
    my $groups = $self->brackets;
    my $this = $self->thisBracket;
    my $group  = $groups->{$this};
    my $number  = $group->number;
    my $x      = $group->x;
    $group->xprime( $group->x );
    $self->log( "x=$x" );
    return C3;
}


=head2 c3

 $pairing->c3

Determine p according to A6.

=cut 

sub c3 {
    my $self   = shift;
    my $groups = $self->brackets;
    my $this = $self->thisBracket;
    my $group  = $groups->{$this};
    my $number  = $group->number;
    my $p      = $group->p;
    $group->pprime( $group->p );
    if ( $group->hetero ) { $self->log( "p=$p. Heterogeneous."); }
    else { $self->log( "p=$p. Homogeneous."); }
    return C4;
}


=head2 c4

 $pairing->c4

The highest players in S1, the others in S2.

=cut 

sub c4 {
    my $self    = shift;
    my $groups  = $self->brackets;
    my $group   = $groups->{$self->thisBracket};
    my $members = $group->members;
    my $number   = $group->number;
    $group->resetS12;
    my $s1      = $group->s1;
    my $s2      = $group->s2;
    my @s1ids = map {$_->id} @$s1;
    my @s2ids = map {$_->id} @$s2;
    $self->log( "C4, S1: @s1ids & S2: @s2ids" );
    die "Empty S1 in Bracket $number with S2: @s2ids." unless @$s1;
    die "Empty Bracket $number with  S1: @s1ids." unless @$s2;
    return C5;
}


=head2 c5

 $pairing->c5

Order the players in S1 and S2 according to A2.

=cut 

sub c5 {
    my $self   = shift;
    my $groups = $self->brackets;
    my $group   = $groups->{ $self->thisBracket };
    my $number  = $group->number;
    my $members = $group->members;
    my $x       = $group->xprime;
    my $s1      = $group->s1;
    my $s2      = $group->s2;
    my @s1      = $self->rank(@$s1);
    my @s2      = $self->rank(@$s2);
    my @s1ids = map {$_->id} @$s1;
    my @s2ids = map {$_->id} @$s2;
    $self->log( "ordered: @s1ids\n           & @s2ids" );
    $group->s1( \@s1 );
    $group->s2( \@s2 );
    for my $member ( @{ $group->s2 } ) {
        die "$member->{id} was in ${number}th bracket more than once"
          if ( grep { $_->id == $member->id } @{ $group->s2 } ) > 1;
    }
    $groups->{ $self->thisBracket } = $group;
    $self->brackets($groups);
    return C6PAIRS;
}


=head2 c6pairs

 Games::Tournament::Swiss::Procedure->c6pairs($group, $matches)

Pair the pprime players in the top half of the scoregroup in order with their counterparts in the bottom half, and return an array of tentative Games::Tournament::Card matches if B1, B2 and the relaxable B4-6 tests pass. In addition, as part of the B6,5 tests, check none of the UNpaired players in a homogeneous bracket were downfloated in the round before (B5) or the round before that (B6), or that there is not only one UNpaired, previously-downfloated player in a heterogeneous group, special-cased following Bill Gletsos' advice at http://chesschat.org/showpost.php?p=142260&postcount=158. If more than pprime tables are paired, we take the first pprime tables.

=cut 

sub c6pairs {
    my $self   = shift;
    my $groups = $self->brackets;
    my $index = $self->thisBracket;
    my $group  = $groups->{ $index };
    my $number  = $group->number;
    my $pprime      = $group->pprime;
    my $s1     = $group->s1;
    my $s2     = $group->s2;
    return NEXT unless @$s1 and @$s2;
    die "More players in S1 than in S2 in $number($index)." if $#$s1 > $#$s2;
    die "zero players in S1 or S2 in $number($index)" unless @$s1 and @$s2;
    my $whoPlayedWho = $self->whoPlayedWho;
    my $colorClashes = $self->colorClashes;
    $group->badpair(undef);
    my @testee;
    for my $pos ( 0..$#$s1, $#$s1+1..$#$s2 )
    {
	$testee[$pos] = [ $s1->[$pos], $s2->[$pos] ] if $pos <= $#$s1;
	$testee[$pos] = [ undef, $s2->[$pos] ] if $pos > $#$s1;
    }
    my ($badpos, @B1passer, @B2passer, @Xpasser, @B56passer, $passer);
    B1: for my $pos (0..$#$s1)
    {
	my @pair = @{$testee[$pos]};
	my $test = not defined $whoPlayedWho->{$pair[0]->id}->{$pair[1]->id};
	if ( $test ) { $B1passer[$pos] = \@pair; }
	else { $badpos = defined $badpos? $badpos: $pos; }
    }
    unless ( (grep { defined $_ } @B1passer) >= $pprime )
    {
	my $pluspos = $badpos+1;
	$self->log( "B1a: table $pluspos NOK" );
	$group->badpair($badpos);
	return C7;
    }
    $badpos = undef;
    die "no pairs after B1 test in $number($index)" unless @B1passer;
    B2: for my $pos (0..$#$s1)
    {
	next unless defined $B1passer[$pos];
	my @pair = ( $B1passer[$pos]->[0], $B1passer[$pos]->[1] );
	my $test = not defined $colorClashes->{$pair[0]->id}->{$pair[1]->id};
	if ( $test ) { $B2passer[$pos] = \@pair; }
	else { $badpos = defined $badpos? $badpos: $pos; }
    }
    unless ( (grep { defined $_ } @B2passer) >= $pprime )
    {
	my $pluspos = $badpos+1;
	$self->log( "B2a: table $pluspos NOK" );
	$group->badpair($badpos);
	return C7;
    }
    die "no pairs after B2 test in $number($index)" unless @B2passer;
    my $x = $group->xprime;
    my $quota = 0;
    $badpos = undef;
    B4: for my $pos ( 0 .. $#$s1 ) {
	next unless defined $B2passer[$pos];
	my @pair = ( $B2passer[$pos]->[0], $B2passer[$pos]->[1] );
	$quota += ( defined $pair[0]->preference->role
              and defined $pair[1]->preference->role
              and $pair[0]->preference->role eq
              $pair[1]->preference->role );
	if ( $quota <= $x ) { $Xpasser[$pos] = \@pair; }
	else { $badpos = defined $badpos? $badpos: $pos; last B4; }
    }
    unless ( (grep { defined $_ } @Xpasser) >= $pprime )
    {
	my $pluspos = $badpos+1;
	$self->log( "B4: x=$x, table $pluspos NOK" );
	$group->badpair($badpos);
	return C7;
    }
    die "no pairs after B4 test in $number($index)" unless @Xpasser;
    $badpos = undef;
    # my @nonpaired = $group->_getNonPaired(@Xpasser);
    my $checkLevels = $self->floatCriteriaInForce( $group->floatCheckWaive );
    my %b65TestResults = $group->_floatCheck( \@Xpasser, $checkLevels );
    $badpos = $b65TestResults{badpos};
    $self->log( $b65TestResults{message} );
    if ( defined $badpos )
    {
	my $pluspos = $badpos+1;
	$group->badpair($badpos);
	return C7;
    }
    $passer = $b65TestResults{passer};
    die "no pairs after B65 test in $number($index)" unless @$passer;
    for my $pos ( 0 .. $#$passer ) {
	next unless defined $passer->[$pos];
	my @pair = @{$passer->[$pos]};
	if ( $pair[0]-> score > $pair[1]->score )
	{
	    $pair[0]->floating('Down');
	    $pair[1]->floating('Up');
	}
	elsif ( $pair[0]-> score == $pair[1]->score )
	{
	    map { $_->floating('Not') } @pair;
	}
	else {
	    $pair[0]->floating('Up');
	    $pair[1]->floating('Down');
	}
    }
    my @nonpaired = $group->_getNonPaired(@$passer);
    my @paired = grep { defined } @$passer;
    if ( $#paired >= $pprime )
    {
	my @unrequired = @paired[ $pprime .. $#paired ];
	splice @paired, $pprime;
	unshift @nonpaired, @unrequired;
    }
    @nonpaired = map { my $pair=$_; grep { defined } @$pair } @nonpaired;
    my @tables = grep { defined $passer->[$_-1] } 1..@$passer;
    $self->log( "@tables paired. OK" );
    $self->nonpaired(\@nonpaired) if @nonpaired;
    my $allMatches = $self->matches;
    my $undoneBracket = $allMatches->{$index};
    my ($pairmessage, @matches) = $self->colors( paired => \@paired ) if @paired;
    $self->log( $pairmessage );
    if ( defined $undoneBracket )
    {
	$allMatches->{$index} = \@matches;
    }
    #elsif ($group->{remainderof})
    #{
    #     $allMatches->{"${thisBracket}Remainder"} = \@matches;
    #}
    else { $allMatches->{$index} = \@matches; }
    if (@paired) {if ( @nonpaired ) { return C6OTHERS } else { return NEXT } }
    return ERROR, msg => "No paired in C6PAIRS";
}


=head2 c6others

 Games::Tournament::Swiss::Procedure->c6others($group, $matches)

After pairing players, if there are remaining players in a homogeneous group, float them down to the next score group and continue with C1. In a heterogeneous group, start at C2 with the remaining players, now a homogeneous remainder group. XXX I am restarting at NEXT. See the problem http://chesschat.org/showpost.php?p=140199&postcount=150, which may be connected, even though C1 leads to C12 only if the player was moved down! As for what to do with remaining players in a last homogeneous group: C13 has to prevent that state from occurring.

=cut 

sub c6others {
    my $self   = shift;
    my $groups = $self->brackets;
    my $index =  $self->thisBracket;
    my $group     = $groups->{$index};
    my $number = $group->number;
    my $nonpaired = $self->nonpaired;
    die "Unpaired players are: $nonpaired?" unless defined $nonpaired and
							    @$nonpaired;
    if ( @$nonpaired == 1 and $index eq $self->lastBracket ) {
	$self->log( "One unpaired player in last $number bracket." );
	$self->byer($nonpaired->[0]);
	return BYE;
    }
    if ( not $group->hetero or @$nonpaired == 1 ) {
	my $next = $self->nextBracket;
	my $nextBracket = $groups->{$next};
	my $nextNumber = $nextBracket->number;
        for my $evacuee (@$nonpaired) {
            $group->exit($evacuee);
            $evacuee->floating('Down');
            $nextBracket->entry($evacuee);
        }
	my @floaters = map {$_->id} @$nonpaired;
	my @members = map {$_->id} @{$group->members};
	my @next = map {$_->id} @{$nextBracket->members};
        $self->log( "Floating remaining @floaters Down. [$number] @members & [$nextNumber] @next" );
        return NEXT;
    }
    else {
	# print
# "$index Bracket $number($index)'s Remainder Group: @{[map{$_->id}@$nonpaired]}\n";
        my $remainderGroup = Games::Tournament::Swiss::Bracket->new(
            score       => $group->score,
            remainderof => $group,
	    number      => "${number}'s Remainder Group"
        );
	# $group->{remainder} ||= $remainderGroup;
	$group->{remainder} = $remainderGroup;
	my $newIndex = "${index}Remainder";
        $groups->{$newIndex} = $remainderGroup;
	my $next = $self->nextBracket;
	my $nextBracket = $groups->{$next};
	my $nextNumber = $nextBracket->number;
        for my $remainer (@$nonpaired) {
            $group->exit($remainer);
	    # $nextBracket->entry($remainer);
	    $remainderGroup->entry($remainer);
        }
	my @remains = map {$_->id} @$nonpaired;
	my @members = map {$_->id} @{$group->members};
	# my @next = map {$_->id} @{$nextBracket->members};
	my @next = map {$_->id} @{$remainderGroup->members};
        $self->log( "Remaindering @remains.\n\t[$number] @members & [$nextNumber] @next" );
	$self->brackets($groups);
	$self->thisBracket($next);
        return C2;
    }
}


=head2 c7

	$next = $pairing->c7
	while ( my @s2 = &$next )
	{
	    create match cards unless this permutation is incompatible;
	}

Apply a new transposition of S2 according to D1 and restart at C6. But take precautions to prevent transposing players who are no longer in the bracket, when finding a different pairing, returning from C10,12,13

=cut 

sub c7 {
    my $self   = shift;
    my $groups = $self->brackets;
    my $group   = $groups->{ $self->thisBracket };
    my $number  = $group->number;
    my $s1      = $group->s1;
    my $s2      = $group->s2;
    my $badpair = $group->badpair;
    $badpair = $#$s2 if not defined $badpair;
    my @newS2   = $group->c7shuffler($badpair);
    unless (@newS2) {
        $self->log("last transposition");
        $group->resetS12;
        return C8 unless $group->hetero;
	return C10 if $group->{c10repairof};
        return C9;
    }
    my @newOrder = map { $_->id } @newS2;
    $self->log( "         @newOrder");
    $group->s2( \@newS2 );
    $group->members( [ @$s1, @newS2 ] );
    $groups->{ $self->thisBracket } = $group;
    return C6PAIRS;
}


=head2 c8

	$next = $pairing->c8
	while ( my ($s1, $s2) = &$next )
	{
	    create match cards unless this exchange is incompatible;
	}

In case of a homogeneous (remainder) group: apply a new exchange between S1 and S2 according to D2. Restart at C5.

=cut 

sub c8 {
    my $self   = shift;
    my $groups = $self->brackets;
    my $this = $self->thisBracket;
    my $group = $groups->{$this};
    my $number  = $group->number;
    my $swapper;
    if ( $group->c8swapper ) {
        $swapper = $group->c8swapper;
    }
    else {
        $swapper = $group->c8iterator;
        $group->c8swapper($swapper);
    }
    my ($message, @newMembers) = &$swapper;
    $self->log( "$message in $number" );
    unless (@newMembers) {
      $swapper = $group->c8iterator;
        $group->c8swapper($swapper);
        return C9;
    }
    my $p  = $group->p;
    my @s1 = @newMembers[ 0 .. $p - 1 ];
    my @s2 = @newMembers[ $p .. $#newMembers ];
    $group->s1( \@s1 );
    $group->s2( \@s2 );
    $self->log( "@{[map { $_->id } @s1]}, @{[map { $_->id } @s2]}" );
    $groups->{$this} = $group;
    $self->{brackets} = $groups;
    return C5;
}


=head2 c9

 Games::Tournament::Swiss::Procedure->c9

Drop, in order, criterion B6 (no identical float to 2 rounds before) and B5 (no identical float to previous round) for downfloats and restart at C4.

=cut 

sub c9 {
    my $self    = shift;
    my $groups  = $self->brackets;
    my $group   = $groups->{ $self->thisBracket };
    my $number   = $group->number;
    if ( $group->floatCheckWaive eq 'None' ) {
        $group->floatCheckWaive('B6Down');
        $self->log( "Dropping B6 for Downfloats" );
        return C4;
    }
    elsif ( $group->floatCheckWaive eq 'B6Down' ) {
        $group->floatCheckWaive('B5Down');
        $self->log( "Dropping B5 for Downfloats" );
        return C4;
    }
    $self->log( " B5,6 already dropped for Downfloats in Bracket $number." );
    return C10;
}


=head2 c10

 Games::Tournament::Swiss::Procedure->c10

In case of a homogeneous remainder group: undo the pairing of the lowest moved down player paired and try to find a different opponent for this player by restarting at C7. If no alternative pairing for this player exists then drop criterion B6 first and then B5 for upfloats and restart at C2 (C4 to avoid p, x resetting.) If we are in a C13 loop (check penultpPrime), avoid the C10 procedure. Why?

=cut 

sub c10 {
    my $self   = shift;
    my $brackets = $self->brackets;
    my $index = $self->thisBracket;
    my $group  = $brackets->{ $index };
    my $groupNumber = $group->number;
    if ( defined $group->{c10repairof} )
    {
	if ( $group->floatCheckWaive eq 'B5Down' ) {
	    $group->floatCheckWaive('B6Up');
	    $self->log(
		"Dropping B6 for Upfloats in Bracket $groupNumber($index)");
	    return C4;
	}
	elsif ( $group->floatCheckWaive eq 'B6Up' ) {
	    $group->floatCheckWaive('B5Up');
	    $self->log(
		"Dropping B5 for Upfloats in Bracket $groupNumber($index)");
	    return C4;
	}
	my $id = $group->s1->[0]->id;
	my $index = $self->thisBracket;
	$self->log( "No more opponents for Player $id" );
	$self->log( "Giving up on Bracket $groupNumber ($index)" );
	return C11;
    }
    elsif ( defined $group->{remainderof} ) {
	if ( $group->{remainderof}->{c11repairof} or
			$group->{remainderof}->{c12repairof} )
	{
	    # $self->log( "Passing $groupNumber ($index) to C11." );
	    return C11;
	}
	elsif ( $group->{remainderof}->{c10repairof} )
	{
	    my $repairgroupRemainder = $group;
	    my $c10RepairGroup = $group->{remainderof};
	    my $lowest = $c10RepairGroup->s1->[0];
	    my $id = $lowest->id;
	    my $inadequateS2member = $c10RepairGroup->s2->[0];
	    my $partnerId = $inadequateS2member->id;
	    my $unpaired = $repairgroupRemainder->members;
	    $repairgroupRemainder->exit($_) for @$unpaired;
	    $_->floating('')            for @$unpaired;
	    $c10RepairGroup->entry($_)   for @$unpaired;
	    $c10RepairGroup->floatCheckWaive('None');
	    $c10RepairGroup->badpair(0);
	    my $repairGroupNumber = $c10RepairGroup->number;
	    my $repairGroupIndex = $self->previousBracket;
	    $self->thisBracket($repairGroupIndex);
	    $repairgroupRemainder->dissolved(1);
	    $self->log(
		"Player $id and $partnerId pairing in $repairGroupNumber" );
	    $self->log(
		"failed in $groupNumber. Returning to $repairGroupNumber" );
	    return C7;
	}
	elsif ( not $group->{remainderof}->{c10repairof} ) {
	    my $heteroBracket = $group->{remainderof};
	    my $number  = $heteroBracket->number;
	    my $score = $heteroBracket->score;
	    my $index = $self->index($heteroBracket);
	    my $matches = $self->matches->{$index};
	    my $s1 = $heteroBracket->s1;
	    my $s2 = $heteroBracket->s2;
	    my $lowest = $s1->[-1];
	    my $id = $lowest->id;
	    my $match = delete $matches->[-1];
	    my $partner = first { $_->id != $id } $match->myPlayers;
	    my $partnerId = $partner->id;
	    $self->log(
		"Unpairing $id and $partnerId in Bracket $number($index)");
	    my $remaindered = $group->members;
	    my @ids = map {$_->id} @$remaindered;
	    $self->log( "Bracket ${number}'s C10Repair Group: $id $partnerId @ids" );
	    my $key = $score . "C10Repair";
	    my $c10RepairGroup = Games::Tournament::Swiss::Bracket->new(
		score       => $score,
		c10repairof => $heteroBracket,
		number      => "${number}'s C10 Repair Group"
		);
	    $heteroBracket->{c10repair} = $c10RepairGroup;
	    $heteroBracket->exit($_) for ( $lowest, $partner );
	    $_->floating('')            for ( $lowest, $partner );
	    $c10RepairGroup->entry($_)   for ( $lowest, $partner );
	    $group->exit($_) for @$remaindered;
	    $_->floating('')            for @$remaindered;
	    $c10RepairGroup->entry($_)   for @$remaindered;
	    # $c10RepairGroup->dissolved(0);
	    $group->dissolved(1);
	    $c10RepairGroup->floatCheckWaive('None');
	    $brackets->{$key} = $c10RepairGroup;
	    $self->thisBracket( $key );
	    $c10RepairGroup->badpair(0);
	    return C7;
	}
    }
    if ( $group->floatCheckWaive eq 'B5Down' ) { # XXX remainder group too?jjjj
	$group->floatCheckWaive('B6Up');
	$self->log("Dropping B6 for Upfloats in Bracket $groupNumber($index)");
	return C4;
    }
    elsif ( $group->floatCheckWaive eq 'B6Up' ) { # XXX remainder group too?jjjj
	$group->floatCheckWaive('B5Up');
	$self->log("Dropping B5 for Upfloats in Bracket $groupNumber($index)");
	return C4;
    }
    elsif ( $group->floatCheckWaive eq 'B5Up' ) {
	$group->floatCheckWaive('All');
	$self->log("Float checks all dropped in Bracket $groupNumber($index)");
	return C11;
    }
    elsif ( $group->floatCheckWaive eq 'All' ) {
	$group->floatCheckWaive('None');
	$self->log("Float checks already off in Bracket $groupNumber($index)");
	return C11;
    }
    return ERROR, msg => "$groupNumber($index) fell through C10", pairing => $self;
}


=head2 c11

 Games::Tournament::Swiss::Procedure->c11

As long as x (xprime) is less than p: increase it by 1. When pairing a remainder group undo all pairings of players moved down also. Restart at C3. We restart at C7 after resetting the C7shuffler transposer.
        
=cut 

sub c11 {
    my $self    = shift;
    my $brackets  = $self->brackets;
    my $index = $self->thisBracket;
    my $group   = $brackets->{ $index };
    my $number   = $group->number;
    my $heteroBracket = $group->{c10repairof};
    my $x       = $group->x;
    my $xprime  = $group->xprime;
    my $pprime       = $group->pprime;
    $xprime = defined $xprime ? $xprime + 1 : $x;
    $group->xprime($xprime);
    if ( $group->{c11repairof} )
    {
	$heteroBracket = $group->{c11repairof};
	if ( $xprime <= $pprime ) {
	    $group->{c8swapper} = $group->c8iterator;
	    $group->floatCheckWaive('None');
	    $self->log(
		"x=$xprime. All float checks on in Bracket $number($index)" );
	    return C3;
	}
	else {
	    $self->log(
	    "x=p=$pprime, no more x increases in Bracket $number ($index).
	    Giving up on C11 Repair of Bracket $number($index). Trying C12");
	    return C12;
	}
    }
    elsif ( $group->{c10repairof} ) {
	if (not $group->{c10repairof}->{c11repairof}) {
	    my $heteroBracket = $group->{c10repairof};
	    my $number  = $heteroBracket->number;
	    my $score = $heteroBracket->score;
	    my $index = $self->index($heteroBracket);
	    my $matches = $self->matches->{$index};
	    delete $self->matches->{$index} if $matches;
	    $self->log( "Deleting matches in Bracket $number($index)");
	    my $paired = $heteroBracket->members;
	    my $unpaired = $group->members;
	    my @floaters = $group->downFloaters;
	    die "2 or more downfloaters in C10 repair group." if @floaters > 1;
	    my @ids = map {$_->id} @$unpaired, @$paired;
	    my $key = $score . "C11Repair";
	    my $c11RepairGroup = Games::Tournament::Swiss::Bracket->new(
	    score       => $score,
	    c11repairof => $heteroBracket,
	    number      => "$number (post-C11)"
	    );
	    $self->log( "Bracket ${number}'s C11 Repairing: @ids" );
	    $group->exit($_) for @$unpaired;
	    $heteroBracket->exit($_) for @$paired;
	    $_->floating('')            for @$unpaired, @$paired;
	    $c11RepairGroup->entry($_) for @$unpaired, @$paired;
	    $heteroBracket->{c11repair} = $c11RepairGroup;
	    $c11RepairGroup->{c11repairof} = $heteroBracket;
	    $group->dissolved(1);
	    $heteroBracket->dissolved(1);
	    $c11RepairGroup->floatCheckWaive('None');
	    $c11RepairGroup->{c8swapper} = $heteroBracket->c8iterator;
	    $c11RepairGroup->resetS12;
	    my $s2 = $c11RepairGroup->s2;
	    $c11RepairGroup->badpair($#$s2);
	    $brackets->{$key} = $c11RepairGroup;
	    $self->thisBracket($key);
	    return C7;
	}
    }
    elsif ( $group->{remainderof} )
    {
	if ( $group->{remainderof}->{c12repairof} )
	{
	    $self->log( "Passing to C12." );
	    return ERROR, msg => "$number($index) shouldn't pass this way";
	    return C12;
	}
	elsif ( $group->{remainderof}->{c11repairof} )
	{
	    if ( $xprime <= $pprime )
	    {
		$self->log( "Trying next pairing in $number($index)" );
		return C7;
	    }
	    my $c11RepairRemainder = $group;
	    my $heteroBracket = $group->{remainderof};
	    my $unpaired = $c11RepairRemainder->members;
	    my $paired = $heteroBracket->members;
	    $c11RepairRemainder->exit($_) for @$unpaired;
	    $_->floating('')            for @$unpaired;
	    $heteroBracket->entry($_)   for @$unpaired;
	    $heteroBracket->floatCheckWaive('None');
	    my $repairGroupNumber = $heteroBracket->number;
	    my $repairGroupIndex = $self->previousBracket;
	    $self->thisBracket($repairGroupIndex);
	    $c11RepairRemainder->dissolved(1);
	    my @unpairedids = map { $_->id } @$unpaired;
	    my @pairedids = map { $_->id } @$paired;
	    $heteroBracket->resetS12;
	    my $s2 = $c11RepairRemainder->s2;
	    $heteroBracket->badpair($#$s2);
	    $self->log(
"Repairing @pairedids in $repairGroupIndex-Bracket ($repairGroupNumber) failed.
@unpairedids in $index-Bracket ($number) unpairable." );
	    my $x       = $heteroBracket->x;
	    my $xprime  = $heteroBracket->xprime;
	    my $pprime       = $heteroBracket->pprime;
	    if ( $xprime <= $pprime ) {
		$self->log( "x=$xprime" );
	    $xprime = defined $xprime ? $xprime + 1 : $x;
	    $heteroBracket->xprime($xprime);
	    $self->log( "Trying next pairing in $repairGroupNumber" );
	    return C7;
	    }
	    else {
		$self->log(
"No repairings in $repairGroupIndex-Bracket ($repairGroupNumber). Go to C12." );
		return C12;
	    }
	}
    }
    elsif ( $xprime <= $pprime ) {
        $self->log( "x=$xprime" );
        if ( $heteroBracket )
        {
            delete $self->matches->{$index};
            $self->log("Undoing all hetero Bracket $number ($index) matches.");
	    $self->log( "All float checks on in Bracket $number($index)" );
	    $heteroBracket->floatCheckWaive('None');
	    $heteroBracket->resetShuffler;
	    return C7;
        }
	else {
	    $group->{c8swapper} = $group->c8iterator;
	    $group->floatCheckWaive('None');
	    $self->log( "All float checks on in Bracket $number ($index)" );
	    return C3;
	}
    }
    else {
	$self->log(
         "x=p=$pprime, no more x increases in Bracket $number ($index)." );
	return C12;
    }
    return ERROR, msg => "$number($index) fell through C11", pairing => $self;
}


=head2 c12

 Games::Tournament::Swiss::Procedure->c12

If the group contains a player who cannot be paired without violating B1 or B2 and this is a heterogeneous group, undo the pairing of the previous score bracket. If in this previous score bracket a pairing can be made whereby another player will be moved down to the current one, and this now allows p pairing to be made then this pairing in the previous score bracket will be accepted. (If there was only one (or two) players in the previous score bracket, obviously (heh-heh) there is no use going back and trying to find another pairing). Using a c12backupto flag to tell if this is the 2nd time through (but what if this is a backtrack to a different bracket?).

=cut 

sub c12 {
    my $self   = shift;
    my $brackets = $self->brackets;
    my $index = $self->thisBracket;
    my $group = $brackets->{$index};
    my $number  = $group->number;
    my $first = $self->firstBracket;
    if ( $index eq $first )
    {  
	$self->log( "No C12 repair from first bracket $number ($index)" );
	return C13;
    }
    my $prevIndex = $self->previousBracket;
    my $previous = $brackets->{$prevIndex};
    my $prevNumber = $previous->number;
    my $previousMembers = $previous->members;
    if ( $group->{c12up} or $previous->{c12down} )
    {
	$self->log(
"Repairing of Bracket $prevIndex ($prevNumber) failed to pair $index ($number). Go to C13");
	return C13;
    }
    elsif ( $group->{c11repairof} )
    {
	# if (not $group->{c11repairof}->{c12repairof}) 
        if (not $group->{c12up}) {
	    my $c11Repair = $group;
	    my $c11RepairIndex = $prevIndex;
	    my $c11RepairNumber = $prevNumber;
	    if ( $previous and $previous->hetero )
	    {
		my @downfloaters = $c11Repair->downFloaters;
		my @floatIds = map { $_->id } @downfloaters;
		my $score = $previous->score;
		my $matches = $self->matches->{$prevIndex};
		delete $self->matches->{$prevIndex} if $matches;
		$self->log("Deleting matches in $prevIndex, home of @floatIds");
		my $paired = $previous->members;
		my @ids = map {$_->id} @downfloaters, @$paired;
		my $key = $score . "C12Repair";
	       my $c12Repair = Games::Tournament::Swiss::Bracket->new(
		    score       => $score,
		   c12repairof => $previous,
		    c12down => $c11Repair,
		    number      => "$prevNumber (post-C12)",
		);
		$self->log("Bracket ${prevIndex}'s C12 Repairing: @ids");
		$c11Repair->exit($_) for @downfloaters;
		$previous->exit($_) for @$paired;
		$_->floating('')            for @downfloaters;
		$c12Repair->entry($_) for @downfloaters, @$paired;
		$previous->{c12repair} = $c12Repair;
		$previous->dissolved(1);
		$c12Repair->floatCheckWaive('None');
		$c12Repair->{c8swapper} = $c12Repair->c8iterator;
		$c12Repair->resetS12;
		my $s2 = $c12Repair->s2;
		$self->thisBracket($key);
		return C7;
	    }
	    elsif ( $previous and not $previous->hetero )
	    {
		$self->log(
		    "Previous $prevIndex-bracket not heterogeneous. Go to C13");
		return C13;
	    }
	    else {
		die
		"No previous $prevIndex-Bracket before $index-Bracket in C12";
	    }
        }
    }
    # elsif ( $group->{remainderof} and $group->{remainderof}->{c12repairof} )
    elsif ( $group->{remainderof} and $group->{remainderof}->{c12down} )
    {
	my $repairGroupIndex = $self->previousBracket;
	my $heteroBracket = $group->{remainderof};
	my $repairGroupNumber = $heteroBracket->number;
	my $c11RepairRemainder = $group;
    }
    elsif ( $group->{remainderof} and $group->{remainderof}->{c11repairof} )
    {
	my $c11Remainder = $group;
	my $c11RepairIndex = $prevIndex;
	my $c11RepairGroup = $previous;
	my $c11RepairNumber = $prevNumber;
	my $paired = $previousMembers;
	my $score = $c11RepairGroup->score;
	my @ids = map {$_->id} @$paired;
	my $matches = $self->matches;
	delete $matches->{ $c11RepairIndex };
	delete $matches->{$c11Remainder} if $matches->{$c11Remainder};
	$self->log(
    "Undoing Bracket $c11RepairIndex-Bracket ($c11RepairNumber) pairs, @ids.");
	$self->thisBracket($c11RepairIndex);
	my $remainderMembers = $c11Remainder->members;
	$c11Remainder->exit($_) for @$remainderMembers;
	$_->floating('')            for @$remainderMembers;
	$c11RepairGroup->entry($_) for @$remainderMembers;
	$c11Remainder->dissolved(1);
	$self->log( "Dissolving $c11RepairIndex-Bracket's Remainder Group" );
	my $newPrevIndex = $self->previousBracket;
	my $bracketAbove = $brackets->{$newPrevIndex};
	my $aboveNumber = $bracketAbove->number;
	if ( $bracketAbove and $bracketAbove->hetero )
	{
	    my $key = $score . "C12Repair";
	    my $c12RepairGroup = Games::Tournament::Swiss::Bracket->new(
	    score       => $score,
	    c12repairof => $bracketAbove,
	    c12down => $c11RepairGroup,
	    number      => "$aboveNumber(post-C12)"
	    );
	    my @downfloaters = $c11RepairGroup->downFloaters;
	    $c11RepairGroup->exit($_) for @downfloaters;
	    $_->floating('')            for @downfloaters;
	    $c12RepairGroup->entry($_) for @downfloaters;
	    $c11RepairGroup->{c12up} = $c12RepairGroup;
	    my @floatIds = map {$_->id} @downfloaters;
	    my @prevIds = map {$_->id} @{$c12RepairGroup->members};
	    my @thisIds = map {$_->id} @{$group->members};
	    $self->log("C12 Repairing of previous $newPrevIndex-Bracket");
	    $self->log(qq/Unfloating @floatIds back from $number ($index). /);
	    $self->log(
		"Bracket $number: @thisIds & Bracket $prevNumber: @prevIds");
	    $bracketAbove->dissolved(1);
	    $c12RepairGroup->floatCheckWaive('None');
	    $c12RepairGroup->{c8swapper} = $c12RepairGroup->c8iterator;
	    $c12RepairGroup->resetS12;
	    $brackets->{$key} = $c12RepairGroup;
	    $self->thisBracket($key);
	    return C7;
	}
	elsif ( not $bracketAbove->hetero ) {
	    $self->log(
    "No C11 OR C12 repairings of $c11RepairIndex-Bracket ($c11RepairNumber)");
	    return C13;
	}
    }
    elsif ( $group->hetero )
    {
	my @downfloaters = $group->downFloaters;
	my $floaterSourceIndex = $prevIndex;
	my $floaterSource = $previous;
	my $floaterSourceNumber = $prevNumber;
	my $paired = $floaterSource->members;
	my $score = $floaterSource->score;
	my @ids = map {$_->id} @$paired;
	my $matches = $self->matches;
	delete $matches->{ $prevIndex };
	$self->log(
    "Undoing Bracket $floaterSourceNumber($floaterSourceIndex) pairs, @ids.");
	my $key = $score . "C12Repair";
	my $c12RepairGroup = Games::Tournament::Swiss::Bracket->new(
	score       => $score,
	c12repairof => $floaterSource,
	c12down => $group,
	number      => "$floaterSourceNumber(post-C12)"
	);
	$group->exit($_) for @downfloaters;
	$floaterSource->exit($_) for @$paired;
	$_->floating('')            for @downfloaters;
	$c12RepairGroup->entry($_) for @downfloaters, @$paired;
	$floaterSource->{c12repair} = $c12RepairGroup;
	$group->{c12up} = $c12RepairGroup;
	my @floatIds = map {$_->id} @downfloaters;
	my @prevIds = map {$_->id} @{$c12RepairGroup->members};
	my @thisIds = map {$_->id} @{$group->members};
	$self->log(qq/Unfloating @floatIds back from $number ($index). /);
	$self->log("Bracket $number: @thisIds & Bracket $prevNumber: @prevIds");
	$floaterSource->dissolved(1);
	$c12RepairGroup->floatCheckWaive('None');
	$c12RepairGroup->{c8swapper} = $c12RepairGroup->c8iterator;
	$c12RepairGroup->resetS12;
	my $s2 = $c12RepairGroup->s2;
	$c12RepairGroup->badpair($#$s2);
	$brackets->{$key} = $c12RepairGroup;
	$self->thisBracket($key);
	return C7;
    }
    elsif ( not $group->hetero )
    {
	$self->log( "$number($index) not heterogeneous. Passing to C13.");
	return C13;
    }
    return ERROR, msg => "$number($index) fell through C12", pairing => $self;
}


=head2 c13

 Games::Tournament::Swiss::Procedure->c13

If the lowest score group contains a player who cannot be paired without violating B1 or B2 or who, if they are the only player in the group, cannot be given a bye (B2b), the pairing of the penultimate score bracket is undone.  Try to find another pairing in the penultimate score bracket which will allow a pairing in the lowest score bracket. If in the penultimate score bracket p becomes zero (i.e. no pairing can be found which will allow a correct pairing for the lowest score bracket) then the two lowest score brackets are joined into a new lowest score bracket. Because now another score bracket is the penultimate one C13 can be repeated until an acceptable pairing is obtained.  XXX  Perhaps all the players from the old penultimate bracket were floated down. eg, t/cc6619.t. As a hack unfloat only those with the same score as the new penultimate bracket.

=cut 

sub c13 {
    my $self    = shift;
    my $brackets  = $self->brackets;
    my $matches = $self->matches;
    my $index = $self->thisBracket;
    my $group   = $brackets->{$index};
    my $number   = $group->number;
    my $members = $group->members;
    unless ($index eq $self->lastBracket)
    {
	$self->log( "$number ($index) not last group. Passing to C14" ) ;
	return C14;
    }
    my $penultimateIndex = $self->previousBracket;
    my $penultimateBracket = $brackets->{$penultimateIndex};
    my $penultimateNumber = $penultimateBracket->number;
    my $penultpPrime = $penultimateBracket->pprime;
    my $penultScore = $penultimateBracket->score;
    $self->log( "penultimate p=$penultpPrime." );
    if ( @$members == 1 and not $self->byes->{$members->[0]->id} ) {
	$self->byer($members->[0]);
	return BYE;
    }
    if ( $index eq $self->firstBracket and defined $penultpPrime )
    {
	return ERROR,
	msg => "All joined into one $index bracket, but no pairings! Sorry";
    }
    #if ( $group->{remainderof} )
    #{
    #    $group = $self->previousBracket;  # XXX
    #    $penultimate = $self->previousBracket;  # XXX
    #}
    #else {
    #    $penultimate = $self->previousBracket;
    #}
    delete $matches->{ $penultimateIndex };
    $self->log("Undoing Bracket $penultimateNumber($penultimateIndex) matches");
    my @immigrants      = $group->downFloaters;
    my @returnees = grep { $_->score == $penultScore } @immigrants;
    my @floaterIds = map { $_->id } @returnees if @returnees;
    $self->log( "Unfloating @floaterIds back from $number." ) if @returnees;
    $group->exit($_) for @returnees;
    $_->floating('')            for @returnees;
    $penultimateBracket->entry($_)   for @returnees;
    $_->floating('') for ( $penultimateBracket->upFloaters );
    $brackets->{ $index } = $group;
    if ( defined $penultpPrime and $penultpPrime == 0 ) {
	my @evacuees = $penultimateBracket->members;
	$penultimateBracket->exit($_) for @evacuees;
	$_->floating('Down') for @evacuees;
	$group->entry($_) for @evacuees;
	# $group->naturalize($_) for @evacuees;
	$penultimateBracket->dissolved(1);
	my @finalIds = map { $_->id } @$members;
	my @penultimateIds = map { $_->id } @{$penultimateBracket->members};
        $self->log( "Joining Bracket $penultimateNumber, $number." );
        $self->log( "Bracket $number: @finalIds => Bracket $penultimateNumber: @penultimateIds" );
	$group->resetShuffler;
        return C1;
    }
    if ( not defined $penultpPrime )
    {
	$penultpPrime = $penultimateBracket->pprime;
    }
    if ( defined $penultpPrime and $penultpPrime > 0 ) {
	my @penultids = map {$_->id} @{$penultimateBracket->members};
	my @finalids = map {$_->id} @{$group->members};
        $self->log( "Re-pairing Bracket $penultimateNumber." );
        $self->log( "Bracket $penultimateNumber: @penultids &" );
	$self->log( "Bracket $number: @finalids" );
	my $s2 = $penultimateBracket->s2;
	$penultimateBracket->badpair($#$s2);
	$penultimateBracket->resetShuffler;
	$self->thisBracket($penultimateIndex);
	$self->penultpPrime( $penultpPrime );
        return C7;
    }
    else { return ERROR, msg => "Fell through C13 in $number ($index)"; }
}


=head2 bye

 $self->bye

The last, single, unpairable player is given a bye. B2

=cut 

sub bye {
    my $self = shift;
    my $index = $self->thisBracket;
    my $brackets = $self->brackets;
    my $bracket = $brackets->{$index};
    my $members = $bracket->members;
    my $byer = $self->byer;
    my $id = $byer->id;
    my $byes = $self->byes;
    my $round = $self->round;
    my $matches = $self->matches;
    my $byeindex = $index . 'Bye';
    $matches->{$byeindex} = [
      Games::Tournament::Card->new(
	round       => $round,
	result      => { Bye => 'Bye' },
	contestants => { Bye => $byer } )
      ];
    $self->log( "OK." );
    $byes->{$id} = $round;
    return LAST;
}




=head2 c14

 Games::Tournament::Swiss::Procedure->c14

Decrease p (pprime) by 1 (and if the original value of x was greater than zero decrease x by 1 as well). As long as p is unequal to zero restart at C4. (At C13, if this is final bracket, because this means it is unpairable.) If p equals zero the entire score bracket is moved down to the next one. Restart with this score bracket at C1. (If it is the penultimate bracket, and the final bracket is unpairable, the final bracket is moved up, but I guess that's the same thing. C13 )

=cut 

sub c14 {
    my $self   = shift;
    my $groups = $self->brackets;
    my $index = $self->thisBracket;
    my $group   = $groups->{ $index };
    my $number  = $group->number;
    my $members = $group->members;
    my $p       = $group->p;
    my $x       = $group->xprime;
    my $pprime  = $group->pprime;
    if ($pprime) {
        $pprime -= 1;
        $x -= 1 if $x;
    }
    $group->pprime($pprime);
    $group->xprime($x);
    $group->floatCheckWaive('None');
    $self->log( "Bracket $number, now p=$pprime" );
    my $next = $self->nextBracket;
    my $nextgroup = $groups->{$next};
    if ( $pprime == 0 and $index eq $self->lastBracket and defined
						    $self->penultpPrime ) {
	$self->penultpPrime(undef);
	$self->previousBracket($group);
	return C13;
    }
    elsif ( $pprime < $p and $index eq $self->lastBracket )
    {
	$self->penultpPrime(undef);
        return C13;
    }
    elsif ($pprime > 0) 
    {
	return C4;
    }
    elsif ( $nextgroup->{remainderof} )
    {
	my $returners = $nextgroup->members;
        $nextgroup->exit($_)  for @$returners;
        $_->floating('')           for @$returners;
        $group->entry($_)      for @$returners;
	$group->naturalize($_) for @$returners;
	my $remainderNumber = $nextgroup->number;
	my @remainderIds = map { $_->id } @$returners;
	my @heteroIds = map { $_->id } @{$group->members};
        $self->log( "Moving all Group $remainderNumber members back to $number." );
        $self->log( "@remainderIds => Bracket $number: @heteroIds" );
	$self->thisBracket($index);
	$nextgroup->resetShuffler;
	$nextgroup->dissolved(1);
        return C1;
    }
    else {
	my @evacuees = @$members;
        $group->exit($_)  for @evacuees;
        $_->floating('Down')           for @evacuees;
        $nextgroup->entry($_)      for @evacuees;
	$nextgroup->naturalize($_) for @evacuees;
	my $nextNumber = $nextgroup->number;
	my @thisMemberIds = map { $_->id } @evacuees;
	my @nextMemberIds = map { $_->id } @{$nextgroup->members};
        $self->log( "Moving down all Bracket $number($next), to $nextNumber." );
        $self->log( "@thisMemberIds => Bracket $nextNumber: @nextMemberIds" );
	$self->thisBracket($next);
	$nextgroup->resetShuffler;
	$group->dissolved(1);
        return C1;
    }
}


=head2 colors

	$next = $pairing->c7
	while ( my @s2 = &$next )
	{
	    create match cards unless this permutation is incompatible;
	}

After an acceptable pairing is achieved that doesn't violate the one-time match only principle (B1) and the 2-game maximum on difference between play in one role over that in the other role (B2), roles are allocated so as to grant the preferences of both players, or grant the stronger preference, or grant the opposite roles to those they had when they last played a round in different roles, or grant the preference of the higher ranked player, in that order. (E) A Games::Tournament::Card object, records round, contestants, (undefined) result, and floats (A4).
 

=cut 

sub colors {
    my $self       = shift;
    my %args       = @_;
    my $groups     = $self->brackets;
    my $round      = $self->round;
    my $thisGroup = $self->thisBracket;
    my $group = $groups->{$thisGroup};
    my $number      = $group->number;
    my $pairs = $args{paired};
    my ($message, @bracketMatches);
    for my $pair ( @$pairs ) {
        my @pair = @$pair;
        my @rolehistory = ( map { $pair[$_]->roles } 0, 1 );
	my @lastdiff;
	for my $round ( 1 .. $round - FIRSTROUND )
	{
	    my $s1role = $rolehistory[0]->[-$round];
	    my $s2role = $rolehistory[1]->[-$round];
	    my @ids = map {$_->id} @pair;
	    warn "Roles for @ids $round rounds ago?" unless $s1role and $s2role;
	    next if $s1role eq $s2role;
            next unless 2 == grep { $_ eq (ROLES)[0] or $_ eq (ROLES)[1] }
		    ($s1role, $s2role);
	    @lastdiff = ($s1role, $s2role);
	    last;
	}
        my ( $contestants, $stronger, $diff );
        my @roles     = map { $_->preference->role } @pair;
        my @strengths = map { $_->preference->strength } @pair;
        my $rule;
        if ( not $roles[0] and not $roles[1] ) {
            ( $roles[0], $roles[1] ) = $self->randomRole;
            $rule = 'No prefs';
        }
        if ( not $roles[0] ) {
            $roles[0] =
                ( $roles[1] eq (ROLES)[1] )
              ? (ROLES)[0]
              : (ROLES)[1];
            $rule = 'No S1 pref';
        }
        if ( not $roles[1] ) {
            $roles[1] =
                ( $roles[0] eq (ROLES)[1] )
              ? (ROLES)[0]
              : (ROLES)[1];
            $rule = 'No S2 pref';
        }
        if ( $roles[0] ne $roles[1] ) {
            $contestants = { $roles[0] => $pair[0], $roles[1] => $pair[1] };
            $rule = 'E1';
        }
        elsif ( $strengths[0] ne $strengths[1] ) {
            if (
                defined(
                    $stronger = (
                        grep { $pair[$_]->preference->strength eq 'Absolute' }
                          0 .. 1
                      )[0]
                )
              )
            {
                1;
            }
            elsif (
                defined(
                    $stronger = (
                        grep { $pair[$_]->preference->strength eq 'Strong' }
                          0 .. 1
                      )[0]
                )
              )
            {
                1;
            }
            elsif (
                defined(
                    $stronger = (
                        grep { $pair[$_]->preference->strength eq 'Mild' }
                          0 .. 1
                      )[0]
                )
              )
            {
                1;
            }
            my $strongerRole = $pair[$stronger]->preference->role;
            my $weaker       = $stronger == 0 ? 1 : 0;
            my $weakerRole   = ( grep { $_ ne $strongerRole } ROLES )[0];
            $contestants = {
                $strongerRole => $pair[$stronger],
                $weakerRole   => $pair[$weaker]
            };
            $rule = 'E2';
        }
        elsif ( @lastdiff )
        {
            $contestants = {$lastdiff[1] => $pair[0], $lastdiff[0] => $pair[1]};
            $rule = 'E3';
        }
        else {
            my $rankerRole = $pair[0]->preference->role;
            my $otherRole = ( grep { $_ ne $rankerRole } ROLES )[0];
            $contestants = { $rankerRole => $pair[0], $otherRole => $pair[1] };
            $rule = 'E4';
        }
        $message .=  $rule . ' ' . $contestants->{ (ROLES)[0] }->id . "&" . $contestants->{ (ROLES)[1] }->id . ' ';
        my %floats =
          map { (ROLES)[$_] => $contestants->{ (ROLES)[$_] }->floating } 0 .. 1;
        push @bracketMatches,
          Games::Tournament::Card->new(
            round       => $self->round,
            result      => undef,
            contestants => $contestants,
            floats      => \%floats
          );
    }
    # $self->previousBracket($group);
    return $message, @bracketMatches;
}


=head2 brackets

	$pairing->brackets

Gets/sets all the brackets which we are pairing. The order of this array is important. The brackets are paired in order. I was storing these as an anonymous array of score group (bracket) objects. But the problem of remainder groups has forced me to store as a hash.

=cut

sub brackets {
    my $self     = shift;
    my $brackets = shift;
    if ( defined $brackets ) { $self->{brackets} = $brackets; }
    elsif ( $self->{brackets} ) { return $self->{brackets}; }
}


=head2 _buildBracketOrder

	$pairing->_buildBracketOrder

Gets an array of homogeneous and heterogeneous brackets in order with remainder groups (iff they have been given bracket status and only until this status is withdrawn) coming after the heterogeneous groups from which they are formed. This ordered array is necessary, because remainder groups come into being and it is difficult to move back to them. Do we re-pair the remainder group, or the whole group from which it came? Remember to keep control of remainder groups' virtual bracket status with the dissolved field. This method depends on each bracket having an index made up of the bracket score and a 'Remainder' or 'C10Repair' suffix, if it is a remainder or other kind of sub-bracket. We rely on the lexico ordering of the suffixes.

=cut

sub _buildBracketOrder {
    my $self     = shift;
    my $brackets = $self->brackets;
    my @indexes = grep { not $brackets->{$_}->dissolved } keys %$brackets;
    my @scoresAndTags = map { m/^(\d*\.?\d+)(\D.*)?$/; [$1,$2] } @indexes;
    my %index;
    @index{@indexes} = map {{score => $_->[0], tag => $_->[1] || '' }} 
				@scoresAndTags;
    my @indexOrder = sort { $index{$b}->{score} <=> $index{$a}->{score} ||
			$index{$a}->{tag} cmp $index{$b}->{tag} }
				@indexes;
    return @indexOrder;
}


=head2 firstBracket

	$pairing->firstBracket

Gets/sets the firstBracket. This is the bracket with the highest score.

=cut

sub firstBracket {
    my $self     = shift;
    my @scoreOrder = $self->_buildBracketOrder;
    return shift @scoreOrder;
}


=head2 lastBracket

	$pairing->lastBracket

Gets/sets the lastBracket. With the joining of score brackets and addition of remainder groups, this bracket may change.

=cut

sub lastBracket {
    my $self     = shift;
    my @scoreOrder = $self->_buildBracketOrder;
    return pop @scoreOrder;
}


=head2 nextBracket

	$pairing->nextBracket

Gets the nextBracket to that which we are pairing now. This may or may not be a remainder group, depending on whether they have been given virtual bracket status.

=cut

sub nextBracket {
    my $self     = shift;
    my $place = $self->thisBracket;
    my @scoreOrder = $self->_buildBracketOrder;
    my $nextBracket;
    if (defined $place)
    {
	my $next = 0;
	for my $index ( @scoreOrder ) {
		$nextBracket = $index;
		last if $next;
		$next++ if $index eq $place;
	    }
	return $nextBracket;
    }
    return;
}


=head2 previousBracket

	$pairing->previousBracket

Gets the previousBracket to that which we are pairing now. This may or may not be a remainder group, depending on whether they have been given virtual bracket status.

=cut

sub previousBracket {
    my $self     = shift;
    my $place = $self->thisBracket;
    my @indexOrder = $self->_buildBracketOrder;
    my $previousBracket;
    for my $index ( @indexOrder ) {
	last if $index eq $place;
	$previousBracket = $index;
    }
    return $previousBracket;
}


=head2 index

	$pairing->index($bracket)

Gets the index of $bracket, possibly a changing label, because remainder groups coming into being and are given virtual bracket status.

=cut

sub index {
    my $self     = shift;
    my $brackets = $self->brackets;
    my $bracket = shift;
    my $score = $bracket->score;
    my $number = $bracket->number;
    my @order = $self->_buildBracketOrder;
    my $index = first { m/^\d+(\.5)?$/ and $brackets->{$_}->score==$score }
					    @order;
    confess "No index for Bracket $number, with score $score. Is it dissolved?"
				    unless defined $index;
    $index .= 'C11Repair' if $bracket->{c11repairof};
    $index .= 'C10Repair' if $bracket->{c10repairof};
    $index .= 'Remainder' if $bracket->{remainderof};
    return $index;
}


=head2 round

	$pairing->round

What round is this round's results we're pairing on the basis of?

=cut

sub round {
    my $self  = shift;
    my $round = shift;
    if ( defined $round ) { $self->{round} = $round; }
    elsif ( $self->{round} ) { return $self->{round}; }
}


=head2 thisBracket

	$pairing->thisBracket
	$pairing->thisBracket($pairing->firstBracket)

What bracket is this? Gets/sets a string of the form $score, or
${score}Remainder if it is a remainder group. (In C10, create an 'C10Repair' group.) You need to set this when moving from one bracket to another. And test the value returned. If no bracket is set, undef is returned.

=cut

sub thisBracket {
    my $self  = shift;
    my $thisBracket = shift;
    if ( defined $thisBracket ) { $self->{thisBracket} = $thisBracket; }
    elsif ( defined $self->{thisBracket} ) { return $self->{thisBracket}; }
    return;
}


=head2 byer

	$group->byer

Gets/sets the player set to take the bye.

=cut

sub byer {
    my $self    = shift;
    my $byer = shift;
    if ( defined $byer ) { $self->{byer} = $byer; }
    elsif ( $self->{byer} ) { return $self->{byer}; }
    return;
}


=head2 paired

	$group->paired

Gets/sets an array of paired players, arranged pair by pair, in the bracket being paired.

=cut

sub paired {
    my $self    = shift;
    my $paired = shift;
    if ( defined $paired ) { $self->{paired} = $paired; }
    elsif ( $self->{paired} ) { return $self->{paired}; }
    return;
}


=head2 nonpaired

	$group->nonpaired

Gets/sets an array of nonpaired players in the bracket being paired.

=cut

sub nonpaired {
    my $self    = shift;
    my $nonpaired = shift;
    if ( defined $nonpaired ) { $self->{nonpaired} = $nonpaired; }
    elsif ( $self->{nonpaired} ) { return $self->{nonpaired}; }
    return;
}


=head2 matches

	$group->matches

Gets/sets the matches which we have made. Returned is an anonymous hash of the matches in the round, keyed on a bracket index. Each value of the hash is an anonymous array of the matches in that bracket. So to get each actual match, you need to break up the matches in the individual brackets.

=cut

sub matches {
    my $self    = shift;
    my $matches = shift;
    if ( defined $matches ) { $self->{matches} = $matches; }
    elsif ( $self->{matches} ) { return $self->{matches}; }
    return;
}


=head2 whoPlayedWho

	$group->whoPlayedWho

Gets/sets a anonymous hash, keyed on the pairing numbers of the opponents, of the preference of individual pairs of @grandmasters, if they both have the same absolute preference, and so can't play each other. This has probably been calculated by Games::Tournament::Swiss::whoPlayedWho B1a

=cut

sub whoPlayedWho {
    my $self         = shift;
    my $whoPlayedWho = shift;
    if ( defined $whoPlayedWho ) { $self->{whoPlayedWho} = $whoPlayedWho; }
    elsif ( $self->{whoPlayedWho} ) { return $self->{whoPlayedWho}; }
}


=head2 colorClashes

	$group->colorClashes

Gets/sets a anonymous hash, keyed on the pairing numbers of the opponents, of their preference, if (and only if) they both have an Absolute preference for the same role and so can't play each other. This has probably been calculated by Games::Tournament::Swiss::colorClashes B2a

=cut

sub colorClashes {
    my $self         = shift;
    my $colorClashes = shift;
    if ( defined $colorClashes ) { $self->{colorClashes} = $colorClashes; }
    elsif ( $self->{colorClashes} ) { return $self->{colorClashes}; }
}


=head2 incompatibles

	$group->incompatibles

Gets/sets a anonymous hash, keyed on the pairing numbers of the opponents, of a previous round in which individual pairs of @grandmasters, if any, met. Or of their preference if they both have an Absolute preference for the same role and can't play each other. This has probably been calculated by Games::Tournament::Swiss::incompatibles. B1

=cut

sub incompatibles {
    my $self          = shift;
    my $incompatibles = shift;
    if ( defined $incompatibles ) { $self->{incompatibles} = $incompatibles; }
    elsif ( $self->{incompatibles} ) { return $self->{incompatibles}; }
}


=head2 byes

	$group->byes
	return BYE unless $group->byes->{$id}

Gets/sets a anonymous hash, keyed on pairing numbers of players, of a previous round in which these players had a bye. This has probably been calculated by Games::Tournament::Swiss::byes. B1

=cut

sub byes {
    my $self = shift;
    my $byes = shift;
    if ( defined $byes ) { $self->{byes} = $byes; }
    elsif ( $self->{byes} ) { return $self->{byes}; }
}


=head2 penultpPrime

	$pairing->penultpPrime
	$pairing->penultpPrime($previousBracket->pprime)

Gets/sets an accessor to the number of pairs in the penultimate bracket. When this reaches 0, the penultimate and final brackets are joined. C14

=cut

sub penultpPrime {
    my $self = shift;
    my $penultpPrime = shift;
    if ( defined $penultpPrime ) { $self->{penultpPrime} = $penultpPrime; }
    elsif ( $self->{penultpPrime} ) { return $self->{penultpPrime}; }
    return;
}


=head2 clearLog

	$pairing->clearLog(qw/C10 C11/)

Discards the logged messages for the passed procedures.

=cut

sub clearLog {
    my $self = shift;
    my @states = @_;
    my $log = $self->{log};
    delete $log->{$_} for @states;
    return;
}


=head2 catLog

	$pairing->catLog(qw/C10 C11/)

Returns the messages logged for the passed procedures, as a hash keyed on the procedures. If no messages were logged, because the procedures were not loggedProcedures, no messages will be returned.

=cut

sub catLog {
    my $self = shift;
    my @states = @_;
    @states = $self->loggedProcedures unless @states;
    my $log = $self->{log};
    my %report = map { $_ => $log->{$_}->{strings} } @states;
    return %report;
}


=head2 tailLog

	$pairing->tailLog(qw/C10 C11/)

Returns the new messages logged for the passed procedures since they were last tailed, as a hash keyed on the procedures. If no messages were logged, because the procedures were not loggedProcedures, no messages will be returned.

=cut

sub tailLog {
    my $self = shift;
    my @states = @_;
    @states = $self->loggedProcedures unless @states;
    my $log = $self->{log};
    my %report = map { $_ => $log->{$_}->{strings} } @states;
    my %tailpos = map { $_ => $log->{$_}->{tailpos} } @states;
    my (%newpos, %lastpos, %tailedReport);
    for my $state ( @states )
    {
	if ( defined $tailpos{$state} )
	{
	    $newpos{$state} = $tailpos{$state} + 1;
	    $lastpos{$state} = $#{ $report{$state} };
	    $tailedReport{$state} = join '',
		@{$report{$state}}[ $newpos{$state}..$lastpos{$state} ];
	    $log->{$_}->{tailpos} = $lastpos{$_} for @states;
	}
	elsif ( $report{$state} ) {
	    $newpos{$state} = 0;
	    $lastpos{$state} = $#{ $report{$state} };
	    $tailedReport{$state} = join '',
		@{$report{$state}}[ $newpos{$state}..$lastpos{$state} ];
	    $log->{$_}->{tailpos} = $lastpos{$_} for @states;
	}
    }
    return %tailedReport;
}


=head2 log

	$pairing->log('x=p=1, no more x increases in Bracket 4 (2).')

Saves the message in a log iff this procedure is logged.

=cut

sub log {
    my $self = shift;
    my $message = shift;
    return unless $message;
    (my $method = uc((caller 1)[3])) =~ s/^.*::(\w+)$/$1/;
    my $loggable = $self->loggedProcedures;
    push @{ $self->{log}->{$method}->{strings} }, "${method}, $message\n" if $loggable->{$method};
    return;
}


=head2 loggedProcedures

	$group->loggedProcedures(qw/C10 C11 C12/)
	$group->loggedProcedures(qw/C5 C6PAIRS C7 C8/)

Adds messages generated in the procedures named in the argument list to a reportable log. Without an argument returns the logged procedures as the keys of an anon hash.

=cut

sub loggedProcedures {
    my $self = shift;
    my @states = @_;
    unless ( @states ) { return $self->{logged}; }
    my %logged;
    @logged{qw/C1 C2 C3 C4 C5 C6 C7 C8 C9 C10 C11 C12 C13 C14/} = (1) x 14;
    for my $state (@states)
    {   
	carp "$state is unloggable procedure" if not defined $logged{$state};
	$self->{logged}->{$state} = 1;
    }
    return;
}


=head2 loggingAll

	$group->loggingAll

Adds messages generated in all the procedures to a reportable log

=cut

sub loggingAll {
    my $self = shift;
    my %logged;
    @logged{qw/NEXT PREV C1 C2 C3 C4 C5 C6PAIRS C6OTHER C7 C8 C9 C10 C11 C12 C13 C14/} = (1) x 15;
    for my $state ( keys %logged )
    {   
	# carp "$state is unloggable procedure" if not defined $logged{$state};
	$self->{logged}->{$state} = 1;
    }
    return;
}


=head2 disloggedProcedures

	$group->disloggedProcedures
	$group->disloggedProcedures(qw/C6PAIRS C7 C8/)

Stops messages generated in the procedures named in the argument list being added to a reportable log. Without an argument stops logging of all procedures.

=cut

sub disloggedProcedures {
    my $self = shift;
    my @states = @_;
    unless ( @states )
    {
	my @methods = keys %{ $self->{logged} };
	@{$self->{logged}}{@methods} = (0) x @methods;
    }
    my %logged;
    @logged{qw/C1 C2 C3 C4 C5 C6 C7 C8 C9 C10 C11 C12 C13 C14/} = (1) x 14;
    for my $state (@states)
    {   
	carp "$state is unloggable procedure" if not defined $logged{$state};
	$self->{logged}->{$state} = 0;
    }
    return;
}


=head2 floatCriteriaInForce

	$group->floatCriteriaInForce( $group->floatCheckWaive )

Given the last criterion at which level checks have been waived, returns an anonymous array of the levels below this level for which checking is still in force. B5,6 C6,9,10 TODO All is nice, but creates problems.

=cut

sub floatCriteriaInForce {
    my $self = shift;
    my $level = shift;
    my @levels = qw/None B6Down B5Down B6Up B5Up All None/;
    my $oldLevel = '';
    $oldLevel = shift @levels until $oldLevel eq $level;
    return \@levels;
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

    perldoc Games::Tournament::Swiss

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

1;    # End of Games::Tournament::Swiss::Procedure

# vim: set ts=8 sts=4 sw=4 noet:
