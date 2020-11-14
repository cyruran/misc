#!/usr/bin/perl -l

use strict;
use warnings;

open my $fh, "<:raw", "logo.img";
my $r;

while (1)
{
    read $fh, $r, 16;
    my ($m1, $m2, $m3, $m4, $size) = unpack "c4x4i", $r;

    read $fh, $r,16;
    my ( $n ) = unpack "x4L", $r;

    read $fh, $r, 32;
    my $fn = unpack "A*", $r;
    print $fn;

    if ( $n ) {
        $size = $n - tell($fh);
    }

    read $fh, $r, $size;

    open my $fh1, ">:raw", "/tmp/aaa/$fn.bmp";
    print $fh1 $r;
    close $fh1;

    last unless $n;
}

close $fh;
