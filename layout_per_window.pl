#!/usr/bin/perl

use strict;
use warnings;

use AnyEvent;
use AnyEvent::I3;

our %layout_per_win;
our @layouts = ("us", "ru -variant kom");
our $current_id = 0;

my $i3 = i3();
$i3->connect->recv or die "err1\n";
$i3->subscribe({
                window => \&h_window,
                binding => \&h_bind,
               })->recv->{success} or die "err2";
AE::cv->recv;

sub h_window {
    my ($data) = @_;
    if ($data->{change} eq q{focus}) {
        my $id = $data->{container}{id};
        my $l_num = $layout_per_win{$id} || 0;
        my $old_l_num = $layout_per_win{$current_id} || 0;
        if ($l_num != $old_l_num) {
            system("setxkbmap " . $layouts[$l_num]);
        }
        $current_id = $id;
    }
    elsif ($data->{change} eq q{close}) {
        my $id = $data->{container}{id};
        delete $layout_per_win{$id};
    }
}

sub h_bind {
    my ($data) = @_;
    if ($data->{change} eq "run" &&
        $data->{binding}{command} eq q{nop layout}) {
        my $l_num = $layout_per_win{$current_id} || 0;
        $l_num = ($l_num + 1) % @layouts;
        my $layout = $layouts[$l_num];
        system("setxkbmap $layout");
        $layout_per_win{$current_id} = $l_num;
    }
}
