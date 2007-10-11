package Games::Tournament::Swiss::Procedure::FIDE;

# Last Edit: 2007 Oct 10, 08:01:39 PM
# $Id: /swiss/trunk/lib/Games/Tournament/Swiss/Procedure/FIDE.pm 1466 2007-10-11T04:49:54.336710Z greg  $

use warnings;
use strict;
use Carp;

use List::Util qw/first/;
use List::MoreUtils qw/all notall zip/;

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
use constant    REPEATC10 => 'REPEATC10';
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

Version 0.05

=cut

our $VERSION = '0.05';


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
      },
      "Games::Tournament::Swiss::Procedure";
}

=head2 matchPlayers

 @pairs = $pairing->matchPlayers;

Run the FIDE C 04.1 algorithm adding matches to $pairing->matches.
TODO I started passing round the args, rather than storing them in the object, because of problems with storing. What were those problems? What does matchPlayers return? Is it a hash or the matches or what?
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
        C6OTHERS, [ \&c6others, NEXT, C1, BYE ],
        C7, [ \&c7, C6PAIRS, C8, C9, REPEATC10 ],
        C8,  [ \&c8,  C5, C9,  C10 ],
        C9,  [ \&c9,  C4, C10, C11 ],
        C10, [ \&c10, C7, C2,  C11 ],
        REPEATC10, [ \&repeatc10, C2, C11 ],
        C11, [ \&c11, C12, C3, ],
        C12,    [ \&c12,    C13, C7 ],
        C13,    [ \&c13,    C14, C7, C1 ],
        C14,    [ \&c14,    NEXT, C4, C13 ],
	BYE,	[ \&bye,    LAST, C13 ],
        NEXT,   [ \&next,   C1, LAST ],
        PREV,   [ \&prev,   C1 ],
        LAST,  [ undef, LAST ],
        ERROR, [ undef, ERROR ],
    );
    my $state    = START;
    my $oldState = $state;
    my %args     = %$self;
    for ( ; ; ) {
        my $transitions = $machine{$state};
        my ( $action, @alterStates ) = @$transitions;
        $oldState = $state;
        ( $state, %args ) = $action->( $self, %args ) if $action;
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

Match the next bracket. End if this is the last bracket. If we are not pairing any bracket now, set the next bracket to the firstBracket.

=cut 

sub next {
    my $self  = shift;
    my %args  = @_;
    my $thisGroup = $self->thisBracket;
    my $groups = $self->brackets;
    return LAST if defined $thisGroup and $thisGroup eq $self->lastBracket;
    my $next    = $self->nextBracket;
    $next = $self->firstBracket unless defined $next;
    my $nextBracket = $groups->{$next};
    die "Next bracket is: $next?" unless defined $nextBracket
		    and $nextBracket->isa('Games::Tournament::Swiss::Bracket');
    my $members = $nextBracket->members;
    my @ids = map {$_->id} @$members;
    my $number = $nextBracket->number;
    $self->thisBracket($next);
    print "Next, Bracket $number: @ids\n";
    return C1, %args;
}


=head2 prev

 $pairing->prev;

Match the previous bracket. End if this is the first bracket.

=cut 

sub prev {
    my $self  = shift;
    my %args  = @_;
    my $brackets = $self->brackets;
    my $thisBracket = $self->thisBracket;
    my $bracket = $brackets->{$thisBracket};
    return LAST if defined $thisBracket and $thisBracket eq $self->firstBracket;
    my $prevIndex = $self->previousBracket;
    my $prevBracket = $brackets->{$prevIndex};
    my $members = $prevBracket->members;
    my $number = $prevBracket->number;
    $self->thisBracket($prevIndex);
    my @ids = map {$_->id} @$members;
    print "Previous, Bracket $number: @ids\n";
    return C1, %args;
}


=head2 c1

 $pairing->c1;

If the score group contains a player for whom no opponent can be found (B1,B2), and if this player is a downfloater, go to C12 to find another player to downfloat instead. Or if this is the last group, go to C13. Otherwise, downfloat the unpairable player.

=cut 

sub c1 {
    my $self          = shift;
    my %args          = @_;
    my $groups        = $self->brackets;
    my $alreadyPlayed = $self->whoPlayedWho;
    my $colorClashes  = $self->colorClashes;
    my $thisBracket = $self->thisBracket;
    my $group         = $groups->{$thisBracket};
    my $number         = $group->number;
    my $members       = $group->residents;
    print "C1,  ";
    my @unpairables;
    if ( @$members == 1 ) {
        push @unpairables, $members->[0];
    }
    else {
      PLAYER: for my $player (@$members) {
	  my $id = $player->id;
            my $rejections  = 0;
            my @candidates = grep { $_ != $player } @$members;
            my @ids       = map { $_->pairingNumber } @candidates;;
            foreach my $candidate ( @ids ) {
		if ( $alreadyPlayed->{$id}->{$candidate} ) { $rejections++; }
		elsif ( $colorClashes->{$id}->{$candidate} ) { $rejections++; }
            }
            if ( $rejections >= @candidates or @candidates == 0 ) {
                print "NOK. " unless @unpairables;
                print $id . " ";
                push @unpairables, $player;
            }
        }
    }
    if (@unpairables) {
        if ( $thisBracket eq $self->lastBracket ) {
            print "\n";
            return C13, %args;
        }
        elsif ((grep {$_->floating and $_->floating eq 'Down'} @unpairables)
		    and @{$groups->{$self->previousBracket}->residents})
	{
	    print "\n";
	    return ( C12, %args );
        }
        else {
            for my $out (@unpairables) {
                $group->exit($out);
                $out->floating('Down');
                $groups->{$self->nextBracket}->entry($out);
            }
	    my $next = $self->nextBracket;
	    my $nextBracket = $groups->{$next};
	    my $nextNumber = $nextBracket->number;
            my @floaters = map {$_->id." ".$_->floating.", "} @unpairables;
	    print qq/Bracket $number. Floating @floaters/;
	    my @originals = map {$_->id} @{$group->members};
	    my @new = map {$_->id} @{$nextBracket->members};
            print "[$number] @originals & [$nextNumber] @new\n";
            if ( @unpairables == @$members ) { return NEXT, %args; }
        }
    }
    print "B1,2 test: OK, no unpairables\n" unless @unpairables;
    return C2, %args;
}


=head2 float

 $pairing->float( $index, $unpairables, $direction )

Float a single contestant, or an anonymous array of contestants, from the ${index}th bracket, but do not remove them from its membership list. If half or more of the bracket is then made up of downfloaters, the bracket is treated as homogeneous. The default $direction is 'Down.' If $direction = 'Up', they are floated up. A3

=cut 

sub float {
    my $self     = shift;
    my %args     = @_;
    my $index    = $args{index};
    my $groups   = delete $args{brackets};
    my $floaters = $args{floaters};
    print "Floating ";
    my @floaters = ref $floaters eq 'ARRAY' ? @$floaters : ($floaters);
    my $direction = $args{direction};
    print "$direction: ";
    my $oldBracket = $groups->[$index];
    my ( $newBracket, $newIndex );

    for my $floater (@floaters) {
        print "$floater->{id} ";
        if ( $direction =~ m/^Up/ ) {
            die "Player $floater->{id} => @{[$index-1]}th group"
              unless $index - 1 >= 0;
        }
        else {
            die "Player $floater->{id} => @{[$index+2]}th group"
              unless $index + 1 <= $#$groups;
        }
        $floater->floating($direction);
        $oldBracket->emigrants($floater);
        $newIndex = $direction =~ m/^Up/i ? $index - 1 : $index + 1;
        if ( $newIndex <= $#$groups ) { $newBracket = $groups->[$newIndex]; }
        else {
            $newBracket = Games::Tournament::Swiss::Bracket->new(
                score   => $groups->[$newIndex]->{score},
                members => undef
            );
        }
        my $members = $newBracket->members;
        unshift @$members, $floater;
        $newBracket->members($members);
        $newBracket->immigrants($floater);
        splice( @$groups, $newIndex, 1, $newBracket );
    }
    my ( $bracketN, $newBracketN ) = ( $index + 1, $newIndex + 1 );

# print "[$bracketN] @{[map {$_->id} @{$groups->[$index]->members}]} => [$newBracketN] @{[map {$_->id} @{$groups->[$newIndex]->members}]}\n";
    print
"[$bracketN] @{[map {$_->id} @{$oldBracket->members}]} => [$newBracketN] @{[map {$_->id} @{$newBracket->members}]}\n";
    return $groups;
}


=head2 reassign

 $pairing->reassign( $unpairables, $direction )

Float a single contestant, or an anonymous array of contestants, to the next bracket and remove them from the old bracket's membership list, if they were a member of the list. If half or more of the bracket is then made up of downfloaters, the bracket is treated as homogeneous. The default $direction is 'Down.' If $direction = 'Up', they are floated up. A3

=cut 

sub reassign {
    my $self     = shift;
    my $index    = $self->{index};
    my $floaters = shift;
    print "Reassigning ";
    my $direction = shift;
    print "$direction: ";
    $self->float( $floaters, $direction );
    my $groups     = $self->brackets;
    my $oldBracket = $groups->[$index];
    my $oldMembers = $oldBracket->members;
    my @floaters   = ref $floaters eq 'ARRAY' ? @$floaters : ($floaters);
    print "[$index] @{[map {$_->id} @{$groups->[$index]->members}]} => ";

    for my $floater (@floaters) {
        my @positions =
          grep { $oldMembers->[$_]->{id} == $floater->{id} } 0 .. $#$oldMembers;
        if (@positions) {
            die
              "floater $floater->{id} was in ${index}th bracket more than once"
              if $#positions;
            my $position;
            $position = shift @positions;
            splice( @$oldMembers, $position, 1 );
            $oldBracket->members($oldMembers);
            splice( @$groups, $index, 1, $oldBracket );
        }
    }
    $self->brackets($groups);
    print "[$index] @{[map {$_->id} @{$groups->[$index]->members}]}\n";
    return;
}


=head2 c2

 $pairing->c2

Determine x according to A8.

=cut 

sub c2 {
    my $self   = shift;
    my %args   = @_;
    my $groups = $self->brackets;
    my $this = $self->thisBracket;
    my $group  = $groups->{$this};
    my $number  = $group->number;
    my $x      = $group->x;
    $group->xprime( $group->x );
    print "C2, x=$x\n";
    return C3, %args;
}


=head2 c3

 $pairing->c3

Determine p according to A6.

=cut 

sub c3 {
    my $self   = shift;
    my %args   = @_;
    my $groups = $self->brackets;
    my $this = $self->thisBracket;
    my $group  = $groups->{$this};
    my $number  = $group->number;
    my $p      = $group->p;
    $group->pprime( $group->p );
    print "C3, p=$p\t";
    if ( $group->hetero ) { print "Heterogeneous.\n"; }
    else { print "Homogeneous.\n"; }
    return C4, %args;
}


=head2 c4

 $pairing->c4

The highest players in S1, the others in S2.

=cut 

sub c4 {
    my $self    = shift;
    my %args    = @_;
    my $groups  = $self->brackets;
    my $group   = $groups->{$self->thisBracket};
    my $members = $group->members;
    my $number   = $group->number;
    $group->resetS12;
    my $s1      = $group->s1;
    my $s2      = $group->s2;
    print
      "C4, S1 & S2: @{[map{ $_->{id} } @$s1]} & @{[map{ $_->{id} } @$s2]}\n";
    for my $member ( @{ $group->s2 } ) {
        die "$member->{id} was in ${number}th bracket more than once"
          if ( grep { $_->id == $member->id } @{ $group->s2 } ) > 1;
    }
    return C5, %args;
}


=head2 c5

 $pairing->c5

Order the players in S1 and S2 according to A2.

=cut 

sub c5 {
    my $self   = shift;
    my %args   = @_;
    my $groups = $self->brackets;
    my $group   = $groups->{ $self->thisBracket };
    my $number  = $group->number;
    my $members = $group->members;
    my $x       = $group->xprime;
    my $s1      = $group->s1;
    my $s2      = $group->s2;
    my @s1      = $self->rank(@$s1);
    my @s2      = $self->rank(@$s2);
    print "C5, ordered: @{[map{ $_->{id} } @s1]} &
             @{[map{ $_->{id} } @s2]}\n";
    # $group->members([@s1, @s2]);
    $group->s1( \@s1 );
    $group->s2( \@s2 );
    for my $member ( @{ $group->s2 } ) {
        die "$member->{id} was in ${number}th bracket more than once"
          if ( grep { $_->id == $member->id } @{ $group->s2 } ) > 1;
    }
    $groups->{ $self->thisBracket } = $group;
    $self->brackets($groups);
    return C6PAIRS, %args;
}


=head2 c6pairs

 Games::Tournament::Swiss::Procedure->c6pairs($group, $matches)

Pair the pprime players in the top half of the scoregroup in order with their counterparts in the bottom half, and return an array of tentative Games::Tournament::Card matches if B1, B2 and the relaxable B4-6 tests pass. In addition, as part of the B6,5 tests, check none of the UNpaired players in a homogeneous bracket were downfloated in the round before (B5) or the round before that (B6), or that there is not only one UNpaired, previously-downfloated player in a heterogeneous group, special-cased following Bill Gletsos' advice at http://chesschat.org/showpost.php?p=142260&postcount=158. If more than pprime tables are paired, we take the first pprime tables.

=cut 

sub c6pairs {
    my $self   = shift;
    my %args   = @_;
    my $groups = $self->brackets;
    my $thisBracket = $self->thisBracket;
    my $group  = $groups->{ $thisBracket };
    my $number  = $group->number;
    my $pprime      = $group->pprime;
    my $s1     = $group->s1;
    my $s2     = $group->s2;
    return NEXT, %args unless @$s1 and @$s2;
    print "C6, ";
    die "@{[$#$s1+1]} players in group $number\'s S1, only @{[$#$s2+1]} in S2."
      if $#$s1 > $#$s2;
    my $whoPlayedWho = $self->whoPlayedWho;
    my $colorClashes = $self->colorClashes;
    delete $args{badpair};
    my ($badpos, @B1passers, @B2passers, @Xpassers, @B56passers, $passers);
    B1: for my $pos (0..$#$s1)
    {
	my @pair = ( $s1->[$pos], $s2->[$pos] );
	my $test = not defined $whoPlayedWho->{$pair[0]->id}->{$pair[1]->id};
	if ( $test ) { $B1passers[$pos] = \@pair; }
	else { $badpos = $pos; last B1; }
    }
    unless ( (grep { defined $_ } @B1passers) >= $pprime )
    {
	my $pluspos = $badpos+1;
	print "B1a: table $pluspos NOK\n";
	return C7, badpair => $badpos, %args;
    }
    $badpos = undef;
    B2: for my $pos (0..$#B1passers)
    {
	next unless defined $B1passers[$pos];
	my @pair = ( $B1passers[$pos]->[0], $B1passers[$pos]->[1] );
	my $test = not defined $colorClashes->{$pair[0]->id}->{$pair[1]->id};
	if ( $test ) { $B2passers[$pos] = \@pair; }
	else { $badpos = $pos; last B2; }
    }
    unless ( (grep { defined $_ } @B2passers) >= $pprime )
    {
	my $pluspos = $badpos+1;
	print "B2a: table $pluspos NOK\n";
	return C7, badpair => $badpos, %args;
    }
    my $x = $group->xprime;
    my $quota = 0;
    $badpos = undef;
    B4: for my $pos ( 0 .. $#B2passers ) {
	next unless defined $B2passers[$pos];
	my @pair = ( $B2passers[$pos]->[0], $B2passers[$pos]->[1] );
	$quota += ( defined $pair[0]->preference->role
              and defined $pair[1]->preference->role
              and $pair[0]->preference->role eq
              $pair[1]->preference->role );
	if ( $quota <= $x ) { $Xpassers[$pos] = \@pair; }
	else { $badpos = $pos; last B4; }
    }
    unless ( (grep { defined $_ } @Xpassers) >= $pprime )
    {
	my $pluspos = $badpos+1;
	print "B4: x=$x, table $pluspos NOK\n";
	return C7, badpair => $badpos, %args;
    }
    $badpos = undef;
    my $checkLevels = $self->floatCriteria( $group->floatCheckWaive );
    my %b65TestResults = $group->_floatCheck( \@Xpassers, $checkLevels );
    $badpos = $b65TestResults{badpos};
    if ( defined $badpos )
    {
	my $pluspos = $badpos+1;
	return C7, badpair => $badpos, %args;
    }
    $passers = $b65TestResults{passers};
    for my $pos ( 0 .. $#$passers ) {
	next unless defined $passers->[$pos];
	my @pair = @{$passers->[$pos]};
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
    my @nonpaired = $group->_getNonPaired(@$passers);
    my @paired = grep { defined } @$passers;
    if ( $#paired >= $pprime )
    {
	my @unrequired = @paired[ $pprime .. $#paired ];
	splice @paired, $pprime;
	unshift @nonpaired, @unrequired;
    }
    @nonpaired = map { my $pair=$_; grep { defined } @$pair } @nonpaired;
    my @tables = grep { defined $passers->[$_-1] } 1..@$passers;
    print @tables . " paired. ";
    $args{nonpaired} = \@nonpaired if @nonpaired;
    my $allMatches = $self->matches;
    my $undoneBracket = $allMatches->{$thisBracket};
    my @matches = $self->colors( paired => \@paired, %args ) if @paired;
    if ( defined $undoneBracket )
    {
	$allMatches->{$thisBracket} = \@matches;
    }
    #elsif ($group->{remainderof})
    #{
    #     $allMatches->{"${thisBracket}Remainder"} = \@matches;
    #}
    else { $allMatches->{$thisBracket} = \@matches; }
    return C6OTHERS, %args if @nonpaired;
    return NEXT if @paired;
    return ERROR, msg => "No paired or unpaired in C6PAIRS, ", %args;
}


=head2 c6others

 Games::Tournament::Swiss::Procedure->c6others($group, $matches)

After pairing players, if there are remaining players in a homogeneous group, float them down to the next score group and continue with C1. In a heterogeneous group, start at C2 with the remaining players, now a homogeneous remainder group. XXX I am restarting at NEXT. See the problem http://chesschat.org/showpost.php?p=140199&postcount=150, which may be connected, even though C1 leads to C12 only if the player was moved down! As for what to do with remaining players in a last homogeneous group: C13 has to prevent that state from occurring.

=cut 

sub c6others {
    my $self   = shift;
    my %args   = @_;
    my $groups = $self->brackets;
    print "C6others: ";
    my $thisBracket =  $self->thisBracket;
    my $group     = $groups->{$thisBracket};
    my $number = $group->number;
    my $nonpaired = delete $args{nonpaired};
    die "Unpaired players are: $nonpaired?" unless defined $nonpaired and
							    @$nonpaired;
    if ( @$nonpaired == 1 and $thisBracket eq $self->lastBracket ) {
	print "One unpaired player in last $number bracket.\n";
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
        print
"Floating remaining @floaters Down. [$number] @members & [$nextNumber] @next\n";
        return NEXT, brackets => $groups, %args;
    }
    else {
        print
"Bracket ${number}'s Remainder Group: @{[map{$_->id}@$nonpaired]}\n";
        my $remainderGroup = Games::Tournament::Swiss::Bracket->new(
            score       => $group->score,
            members     => $nonpaired,
            remainderof => $group,
	    number      => "${number}'s Remainder Group"
        );
	$group->{remainder} ||= $remainderGroup;
	# my $newIndex = $thisBracket;
	my $newIndex = "${thisBracket}Remainder";
        $groups->{$newIndex} = $remainderGroup;
	$self->brackets($groups);
        return NEXT, brackets => $groups, %args;
    }
}


=head2 _cleanC7

    $previous->entry($_) for @returnees;
    $pairing->_cleanC7;
    return C7;

Take precautions to prevent transposing players who are no longer in the bracket in S2, when finding a different pairing, before returning from C10,12,13 (C11?). Do this by resetting S1 and S2. Don't use this in the wrong place. We don't want to try the same pairing twice.

=cut 

sub _cleanC7 {
    my $self   = shift;
    my $groups = $self->brackets;
    my $bracket   = $groups->{ $self->previousBracket };
    my $members = $bracket->members;
    my $s1      = $bracket->s1;
    my $s2      = $bracket->s2;
    my %s1 = map { $_->id => $_ } @$s1;
    my %s2 = map { $_->id => $_ } @$s2;
    my %members = map { $_->id => $_ } @$members;
    my $memberChangeTest = ( (notall { exists $members{$_} } keys %s1) or
		(notall { exists $members{$_} } keys %s2) );
    $bracket->resetS12 if $memberChangeTest;
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
    my %args   = @_;
    my $groups = $self->brackets;
    print "C7, ";
    my $group   = $groups->{ $self->thisBracket };
    my $number  = $group->number;
    my $s1      = $group->s1;
    my $s2      = $group->s2;
    my $badpair = $args{badpair};
    $badpair = $#$s2 if not defined $badpair;
    my @newS2   = $group->c7shuffler($badpair);
    unless (@newS2) {
        print "last transposition\n";
        $group->resetS12;
        return ( C8, brackets => $groups, %args ) unless $group->hetero;
        return ( REPEATC10, brackets => $groups, %args )
          if $group->hetero
          and $group->{didC10};
        return C9, brackets => $groups, %args;
    }
    print "         @{[map { $_->id } @newS2]}\n";
    $group->s2( \@newS2 );
    $group->members( [ @$s1, @newS2 ] );
    $groups->{ $self->thisBracket } = $group;
    # print "\n";
    return C6PAIRS, brackets => $groups, %args;
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
    my %args   = @_;
    my $groups = $self->brackets;
    my $this = $self->thisBracket;
    my $group = $groups->{$this};
    my $number  = $group->number;
    print "C8, ";
    die "Group $number does not exist"
      unless first { defined } keys %$groups;
    my $swapper;

    if ( $group->c8swapper ) {
        $swapper = $group->c8swapper;
    }
    else {
        $swapper = $group->c8iterator;
        $group->c8swapper($swapper);
    }
    my ($message, @newMembers) = &$swapper;
    print "$message:\t";
    unless (@newMembers) {
        print "\n";
        return ( C10, brackets => $groups, %args )
          if $group->{remainderof}
          and not $group->{didC10};
      $swapper = $group->c8iterator;
        $group->c8swapper($swapper);
        return ( C9, brackets => $groups, %args ) unless $group->{hetero};
        return ( ERROR, msg => "Only homogeneous (remainder) groups here" );
    }
    my $p  = $group->p;
    my @s1 = @newMembers[ 0 .. $p - 1 ];
    my @s2 = @newMembers[ $p .. $#newMembers ];
    $group->s1( \@s1 );
    $group->s2( \@s2 );
    print "@{[map { $_->id } @s1]}, @{[map { $_->id } @s2]}\t";
    $groups->{$this} = $group;
    $self->{brackets} = $groups;
    print "\n";
    return C5, brackets => $groups, %args;
}


=head2 c9

 Games::Tournament::Swiss::Procedure->c9

Drop, in order, criterion B6 (no identical float to 2 rounds before) and B5 (no identical float to previous round) for downfloats and restart at C4.

=cut 

sub c9 {
    my $self    = shift;
    my %args    = @_;
    my $groups  = $self->brackets;
    my $group   = $groups->{ $self->thisBracket };
    my $number   = $group->number;
    print "C9, ";
    if ( $group->floatCheckWaive eq 'None' ) {
        $group->floatCheckWaive('B6Down');
        print "Dropping B6 for Downfloats\n";
        return C4, brackets => $groups, %args;
    }
    elsif ( $group->floatCheckWaive eq 'B6Down' ) {
        $group->floatCheckWaive('B5Down');
        print "Dropping B5 for Downfloats\n";
        return C4, brackets => $groups, %args;
    }
    print " B5,6 already dropped for Downfloats in Bracket $number.\n";
    return C11, brackets => $groups, %args;
}


=head2 c10

 Games::Tournament::Swiss::Procedure->c10

In case of a homogeneous remainder group: undo the pairing of the lowest moved down player paired and try to find a different opponent for this player by restarting at C7. If no alternative pairing for this player exists then drop criterion B6 first and then B5 for upfloats and restart at C2. We do this in a separate subroutine, repeatc10. If we are in a C13 loop (check penultpPrime), avoid the C10 procedure. Why?

=cut 

sub c10 {
    my $self   = shift;
    my %args   = @_;
    # return C11, %args if defined $args{penultpPrime};
    my $groups = $self->brackets;
    my $thisRemainderGroup = $self->thisBracket;
    my $remainderGroup  = $groups->{ $thisRemainderGroup };
    my $bracket = $remainderGroup->{remainderof};
    my $number  = $bracket->number;
    my $index = $self->previousBracket;
    die "No group $number to undo"
      unless $bracket
      and $bracket->isa('Games::Tournament::Swiss::Bracket');
    my $s1 = $bracket->s1;
    my $s2 = $bracket->s2;
    print "C10,re-pairing Player $s1->[-1]->{id} in Bracket $number\n";
    for my $member (@$s2) { $member->floating(''); }
    my $matches = delete $args{matches};
    delete $matches->{$thisRemainderGroup};
    $bracket->{didC10} = 1;
    # $groups->{ $thisRemainderGroup } = $bracket;
    $self->thisBracket( $index );
    return C7, matches => $matches, brackets => $groups, %args;
}


=head2 repeatc10

 Games::Tournament::Swiss::Procedure->c10repeat

In C10, the pairing of the lowest moved down player of a heterogeneous group was undone and a different opponent was sought for this player by restarting at C7. Assuming no alternative pairing for this player was found, in REPEATC10, drop criterion B6 first and then B5 for upfloats and restart at C2. 

=cut 

sub repeatc10 {
    my $self    = shift;
    my %args    = @_;
    my $groups  = $self->brackets;
    my $bracket = $groups->{ $self->thisBracket };
    print "C10,again\t";
    my $number   = $bracket->number;
    if ( $bracket->{didC10} ) {
        if ( $bracket->floatCheckWaive eq 'B5Down' ) {
            $bracket->floatCheckWaive('B6Up');
            print "Dropping B6 for Upfloats\n";
            return C2, %args;
        }
        elsif ( $bracket->floatCheckWaive eq 'B6Up' ) {
            $bracket->floatCheckWaive('B5Up');
            print "Dropping B5 for Upfloats\n";
            return C2, %args;
        }
    }
    # $self->brackets->[$number]->{didC10} = 0;
    return C11, %args;
}


=head2 c11

 Games::Tournament::Swiss::Procedure->c11

As long as x (xprime) is less than p: increase it by 1. When pairing a remainder group undo all pairings of players moved down also. Restart at C3. But wait! Have all B6,5 checks been waived first? Although not in C, waive them before increasing xprime. 
        
=cut 

sub c11 {
    my $self    = shift;
    my %args    = @_;
    my $groups  = $self->brackets;
    my $index = $self->thisBracket;
    my $group   = $groups->{ $index };
    my $number   = $group->number;
    if ( $group->floatCheckWaive eq 'B5Down' )
    {
	$group->floatCheckWaive('B6Up');
	return C3, brackets => $groups, %args;
    }
    elsif ( $group->floatCheckWaive eq 'B6Up' )
    {
	$group->floatCheckWaive('B5Up');
	return C3, brackets => $groups, %args;
    }
    my $pprime       = $group->pprime;
    my $x       = $group->x;
    my $xprime  = $group->xprime;
    $xprime = defined $xprime ? $xprime + 1 : $x;
    $group->xprime($xprime);

    unless ( $xprime > $pprime ) {
        print "C11, x=$xprime, ";
        my $matches = $self->matches;
        if (    $group->{remainderof}
            and $group->{remainderof}->isa("Games::Tournament::Swiss::Bracket")
          )
        {
            $group = $group->{remainderof};
	    my $previousIndex = $self->previousBracket;
	    my $heterogenousParent = $groups->{$previousIndex};
	    my $previousNumber = $heterogenousParent->number;
            delete $matches->{$previousIndex};
            print "Undoing all parent Bracket $previousNumber matches";
	    $self->thisBracket($previousIndex);
        }
        $group->{c8swapper} = $group->c8iterator;
	print "\n";
        return C3, matches => $matches, brackets => $groups, %args;
    }
    print
	"C11, x=p=$pprime already, no more x increases in Bracket $number.\n";
    return ( C12, brackets => $groups, %args ) ; # if $number < $#$groups;
    # return C13, brackets => $groups, %args;
}


=head2 c12

 Games::Tournament::Swiss::Procedure->c12

If the group contains a player who cannot be paired without violating B1 or B2 and this is a heterogeneous group, undo the pairing of the previous score bracket. If in this previous score bracket a pairing can be made whereby another player will be moved down to the current one, and this now allows p pairing to be made then this pairing in the previous score bracket will be accepted. (If there was only one (or two) players in the previous score bracket, obviously (heh-heh) there is no use going back and trying to find another pairing).

=cut 

sub c12 {
    my $self   = shift;
    my %args   = @_;
    my $groups = $self->brackets;
    print "C12, ";
    my $index = $self->thisBracket;
    my $group = $groups->{$index};
    my $number  = $group->number;
    print "Heterogenous group $number now homogeneous\n" unless $group->hetero;
    if ( $group->xprime == 0 and not $group->hetero )
    {
        return C13, brackets => $groups, %args;
    }
    my $previousIndex = $self->previousBracket;
    my $previous = $groups->{$previousIndex};
    $previous = $previous->{remainderof} if
	    $previous->{remainderof};
    my $prevNumber = $previous->number;
    my $previousMembers = $previous->members;
    # die "No $prevNumber group to undo" unless @$previousMembers;
    if ( @$previousMembers <= 2 or not $group->hetero )
    {
	print "No repairings of previous score bracket\n";
	return C13, brackets => $groups, %args;
    }
    my $members = $group->members;
    my $pprime = $previous->pprime;
    my $matches = $self->matches;
    delete $matches->{ $previousIndex };
    print "Undoing Bracket $prevNumber matches. ";
    my @lastMemberIds = map { $_->id } @$members;
    my $residents     = $group->residents;
    my @returnees      = $group->downFloaters;
    $group->exit($_) for @returnees;
    $_->floating('')            for @returnees;
    $previous->entry($_)   for @returnees;
    # delete $args{badpair};
    $groups->{ $self->thisBracket } = $group;
    $groups->{ $self->previousBracket } = $previous;

    	my @returneeIds = map {$_->id} @returnees;
	my @prevIds = map {$_->id} @{$previous->members};
	my @thisIds = map {$_->id} @{$group->members};

    print "Re-pairing Bracket $prevNumber, ";
    print qq/Unfloating @returneeIds back. / if @returnees;
    print "Bracket $number: @thisIds & Bracket $prevNumber: @prevIds"
      if @returnees;
    print "\n";
	$self->_cleanC7;
    $self->thisBracket($previousIndex);
    return C7,
    # number   => @{ [ $number - 1 ] },
      badpair => @{ [ $pprime - 1 ] },
      matches  => $matches,
      brackets => $groups,
      %args;
}


=head2 backtrack

 Games::Tournament::Swiss::Procedure->backtrack

If the group contains a player who cannot be paired without violating B1 or B2, the pairing of the previous score bracket may be undone, in some circumstances. Another pairing may then be sought in that previous score bracket which will allow a pairing in this score bracket. This backtracking may or may not involve unfloating downfloated players.)

=cut 

sub backtrack {
    my $self  = shift;
    my $index = $self->{index};
    print "backtrack:\t";
    die "Undoing pairing in group[index-1] but index=0; " if $index == 0;
    my $groups   = $self->brackets;
    my $bracket  = $groups->[$index];
    my $previous = $index - 1;
    warn "No $previous group to undo"
      unless @{ $groups->[ $index - 1 ]->members };
    my $oldPairing = pop @{ $self->matches };
    $self->{index} = $previous;
    $self->c7;
    return ERROR, msg => "Fell through backtrack";
}


=head2 c13

 Games::Tournament::Swiss::Procedure->c13

If the lowest score group contains a player who cannot be paired without violating B1 or B2 or who, if they are the only player in the group, cannot be given a bye (B2b), the pairing of the penultimate score bracket is undone.  Try to find another pairing in the penultimate score bracket which will allow a pairing in the lowest score bracket. If in the penultimate score bracket p becomes zero (i.e. no pairing can be found which will allow a correct pairing for the lowest score bracket) then the two lowest score brackets are joined into a new lowest score bracket. Because now another score bracket is the penultimate one C13 can be repeated until an acceptable pairing is obtained. TODO The first condition subsumes B2b. I need to move the bye granting code out of here. XXX I am popping the last match off if $number == $#$matches. Perhaps all the players from the penultimate bracket were floated down. Do I need to identify which bracket the  matches came from more securely? Perhaps for more than one bracket, all were floated down.

=cut 

sub c13 {
    my $self    = shift;
    my %args    = @_;
    my $groups  = $self->brackets;
    my $matches = $self->matches;
    my $thisBracket = $self->thisBracket;
    my $group   = $groups->{$thisBracket};
    my $number   = $group->number;
    my $members = $group->members;
    my $penultimateIndex = $self->previousBracket;
    my $penultimateBracket = $self->brackets->{$penultimateIndex};
    my $penultimateNumber = $penultimateBracket->number;
    my $penultpPrime = $penultimateBracket->pprime;
    print "C13, ";
    unless ($thisBracket eq $self->lastBracket)
    {
	print "Not last group. Passing to C14\n";
	return ( C14, number => $number, brackets => $groups, %args )
    }
    print "p=$penultpPrime. ";
    if ( @$members == 1 and $members->[0]->unbyable ) {
	return BYE;
    }
    if ( $thisBracket eq $self->firstBracket and defined $penultpPrime )
    {
	return ERROR,
	msg => "All joined into one bracket, but no pairings! Sorry", %args;
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
    print "Undoing Bracket $penultimateNumber matches. ";
    my $residents     = $group->residents;
    my @returnees      = $group->downFloaters;
    my @floaterIds = map { $_->id } @returnees;
    print "Unfloating @floaterIds back. " if @returnees;
    $group->exit($_) for @returnees;
    $_->floating('')            for @returnees;
    $penultimateBracket->entry($_)   for @returnees;
    $_->floating('') for ( $penultimateBracket->upFloaters );
    delete $args{badpair};
    $groups->{ $thisBracket } = $group;
    if ( defined $penultpPrime and $penultpPrime == 0 ) {
	my @evacuees = $penultimateBracket->residents;
	$penultimateBracket->exit($_) for @evacuees;
	$_->floating('Down') for @evacuees;
	$group->entry($_) for @evacuees;
	$group->naturalize($_) for @evacuees;
	$penultimateBracket->annexed(1);
	#my @aboriginals        = grep {
	#    my $resident = $_;
	#    not grep { $resident->id == $_->id } @returnees
	#} @$residents;
        #$group->exit($_)  for @aboriginals;
        #$_->floating('Up')           for @aboriginals;
        #$penultimateBracket->entry($_)      for @aboriginals;
        #$penultimateBracket->naturalize($_) for @aboriginals;
	## $groups->{ $thisBracket } = $group;
	## $groups->{$penultimateIndex} = $penultimateBracket;
	my @finalIds = map { $_->id } @$members;
	my @penultimateIds = map { $_->id } @{$penultimateBracket->members};
        print "Joining Bracket $penultimateNumber, $number. ";
        print
"Bracket $number: @finalIds => Bracket $penultimateNumber: @penultimateIds\n";
	$self->_cleanC7;
        return C1,
	  # penultpPrime => $penultpPrime,
          number => @{ [ $number - 1 ] },
          matches  => $matches,
          brackets => $groups,
          %args;
    }
    if ( not defined $penultpPrime )
    {
	$penultpPrime = $penultimateBracket->pprime;
    }
    if ( defined $penultpPrime and $penultpPrime > 0 ) {
	my @penultids = map {$_->id} @{$penultimateBracket->members};
	my @finalids = map {$_->id} @{$group->members};
        print "Re-pairing Bracket $penultimateNumber. ";
        print "Bracket $penultimateNumber: @penultids & ";
	print "Bracket $number: @finalids\n";
	$self->_cleanC7;
	$self->thisBracket($penultimateIndex);
        return C7,
	    penultpPrime => $penultpPrime,
          matches  => $matches,
          brackets => $groups,
          %args;
    }
    else { return ERROR, msg => "Fell through C13", %args; }
}


=head2 bye

 $self->bye

The last, single, unpairable player is given a bye, but only if it hasn't had one before. B2

=cut 

sub bye {
    my $self = shift;
    my $index = $self->thisBracket;
    my $brackets = $self->brackets;
    my $bracket = $brackets->{$index};
    my $members = $bracket->members;
    my $byer = $bracket->s2->[-1];
    my $id = $byer->id;
    print "BYE, player $id ";
    my $byes = $self->byes;
    if ( my $round = $byes->{$id} ) {
	print "but, $byer had bye in round $round\n";
	$byer->unbyable(1);
	return C13;
    }
    else {
	my $round = $self->round;
	my $matches = $self->matches;
	my $byeindex = $index . 'Bye';
	$matches->{$byeindex} = [
	  Games::Tournament::Card->new(
	    round       => $round,
	    result      => { Bye => 'Bye' },
	    contestants => { Bye => $byer } )
	  ];
	print "OK.\n";
	$byes->{$id} = $round;
	return LAST;
    }
}


=head2 unfloat

 $self->unfloat(\@downFloaters)

Floaters may be substituted (C12,C13), requiring the pairing of the previous bracket to be undone. 'unfloat' returns a floater (or an anonymous array of them) to the bracket it came from, TODO if that bracket will accept it. That will depend on whether an opponent for it can be found in that bracket.

=cut 

sub unfloat {
    my $self     = shift;
    my $floaters = shift;
    my @floaters = ref $floaters eq 'ARRAY' ? @$floaters : ($floaters);
    print qq/Unfloating @{[map {$_->floating." ".$_->id.", "} @floaters]} /;
    my $groups     = $self->brackets;
    my $index      = $self->{index};
    my $bracketNow = $groups->[$index];
    my $membersNow = $bracketNow->members;
    my ( $prevIndex, $prevBracket );
    for my $floater (@floaters) {
        my $oldDirection = $floater->floating;
#if ($oldDirection =~ m/^Up/)
#{
#    die "Trying to return floater $floater->{id} to nonexistent @{[$index-1]}th bracket" unless $index+1 <= $#$groups;
#}
#else {
#    die "Trying to return floater $floater->{id} to nonexistent @{[$index+2]}th bracket in round $self->{round}" unless $index-1 >= 0;
#}
        $floater->floating('');
        $prevIndex = $oldDirection =~ m/^Up/i ? $index + 1 : $index - 1;
        $prevBracket = $groups->[$prevIndex];
        my @positions =
          grep { $membersNow->[$_] == $floater } 0 .. $#$membersNow;
        my $position = shift @positions;
        die "floater $floater->{id} was in ${index}th bracket more than once"
          if @positions;
        ( my $retrodirection = $floater->floating ) =~
          s/^$oldDirection,(.*)$/$1/;
        splice( @$membersNow, $position, 1 );
        $bracketNow->members($membersNow);
        splice( @$groups, $index, 1, $bracketNow );
        unshift @{ $prevBracket->members }, $floater;
    }
    splice( @$groups, $prevIndex, 1, $prevBracket );
    print
"[$index] @{[map {$_->id} @{$groups->[$index]->members}]} [$prevIndex] @{[map {$_->id} @{$groups->[$prevIndex]->members}]}\n";
    return;
}


=head2 c14

 Games::Tournament::Swiss::Procedure->c14

Decrease p (pprime) by 1 (and if the original value of x was greater than zero decrease x by 1 as well). As long as p is unequal to zero restart at C4. (At C13, if this is final bracket, because this means it is unpairable.) If p equals zero the entire score bracket is moved down to the next one. Restart with this score bracket at C1. (If it is the penultimate bracket, and the final bracket is unpairable, the final bracket is moved up, but I guess that's the same thing. C13 )

=cut 

sub c14 {
    my $self   = shift;
    my %args   = @_;
    my $groups = $self->brackets;
    print "C14,\t";
    my $thisGroup = $self->thisBracket;
    my $group   = $groups->{ $thisGroup };
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
    print "Bracket $number, now p=$pprime\n";
    if ( $pprime == 0 and $thisGroup eq $self->lastBracket and defined $args{penultpPrime})
    {
	delete $args{number};
	delete $args{penultpPrime};
	$self->previousBracket($group);
	return ( C13, number => $#$groups,
		penultpPrime => $pprime,
		brackets => $groups, %args );
    }
    elsif ( $pprime < $p and $thisGroup eq $self->lastBracket )
    {
        delete $args{penultpPrime};
        return ( C13,
        	penultpPrime => $pprime,
        	brackets => $groups, %args );
    }
    elsif ($pprime > 0) 
    {
	return ( C4, brackets => $groups, %args );
    }
    else {
	my @evacuees = @$members;
	my $next = $self->nextBracket;
	my $nextgroup = $groups->{$next};
        $group->exit($_)  for @evacuees;
        $_->floating('Down')           for @evacuees;
	$group->annexed(1);
        $nextgroup->entry($_)      for @evacuees;
	$nextgroup->naturalize($_) for @evacuees;
	# $groups->{ $thisGroup } = $group;
	# $groups->{ $next } = $nextgroup;
	my $nextNumber = $nextgroup->number;
	my @thisMemberIds = map { $_->id } @evacuees;
	my @nextMemberIds = map { $_->id } @{$nextgroup->members};
        print "Moving down all Bracket $number, to $nextNumber. ";
        print "@thisMemberIds => Bracket $nextNumber: @nextMemberIds\n";
	$self->thisBracket($next);
        return C1,
          brackets => $groups,
          %args;
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
    my $pairs = delete $args{paired};
    my @bracketMatches;
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
        print $rule . ' '
          . $contestants->{ (ROLES)[0] }->id . "&"
          . $contestants->{ (ROLES)[1] }->id . "  ";
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
    print "\n";
    # $self->previousBracket($group);
    # return C6OTHERS, matches => $allMatches, %args;
    return @bracketMatches;
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

Gets an array of homogeneous and heterogeneous brackets in order with remainder groups (iff they have been given bracket status and only until this status is withdrawn) coming after the heterogeneous groups from which they are formed. This ordered array is necessary, because remainder groups come into being and it is difficult to move back to them. Do we re-pair the remainder group, or the whole group from which it came? Remember to keep control of remainder groups' virtual bracket status. This method depends on each bracket having an index made up of the bracket score and a 'Remainder' suffix, if it is a remainder group.

=cut

sub _buildBracketOrder {
    my $self     = shift;
    my $brackets = $self->brackets;
    my @indexes = grep { not $brackets->{$_}->annexed } keys %$brackets;
    my @scoresAndremainders = map { m/^(\d*\.?\d+)(\D*)$/; [$1,$2] } @indexes;
    my %index;
    @index{@indexes} = map {{score => $_->[0], remainder => $_->[1] || '' }} 
				@scoresAndremainders;
    my @indexOrder = sort { $index{$b}->{score} <=> $index{$a}->{score} ||
			$index{$a}->{remainder} cmp $index{$b}->{remainder} }
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

Gets/sets the nextBracket to that which we are pairing now. This may or may not be a remainder group, depending on whether they have been given virtual bracket status.

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

Gets/sets the previousBracket to that which we are pairing now. This may or may not be a remainder group, depending on whether they have been given virtual bracket status.

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


=head2 myIndex

	$pairing->myIndex($bracket)

Gets the index of $bracket, possibly a changing label, because remainder groups coming into being and are given virtual bracket status.

=cut

sub myIndex {
    my $self     = shift;
    my $brackets = $self->brackets;
    my $bracket = shift;
    my %indexes = map { $_->thisBracket=> $_ } @$brackets;
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
${score}Remainder if it is a remainder group. You need to set this when moving from one bracket to another. And test the value returned. If no bracket is set, undef is returned.

=cut

sub thisBracket {
    my $self  = shift;
    my $thisBracket = shift;
    if ( defined $thisBracket ) { $self->{thisBracket} = $thisBracket; }
    elsif ( defined $self->{thisBracket} ) { return $self->{thisBracket}; }
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

Gets/sets a anonymous hash, keyed on pairing numbers of players, of a previous round in which these players had a bye. This has probably been calculated by Games::Tournament::Swiss::byes. B1

=cut

sub byes {
    my $self = shift;
    my $byes = shift;
    if ( defined $byes ) { $self->{byes} = $byes; }
    elsif ( $self->{byes} ) { return $self->{byes}; }
}


=head2 floatCriteria

	$group->floatCriteria( $group->floatCheckWaive )

Given the last criterion at which level checks have been waived, returns an anonymous array of the levels below this level for which checking is still in force. B5,6 C6,9,10

=cut

sub floatCriteria {
    my $self = shift;
    my $level = shift;
    my @levels = qw/None B6Down B5Down B6Up B5Up All/;
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
