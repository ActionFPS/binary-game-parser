#!/usr/bin/perl
use strict;
use warnings;
use File::Map qw(map_file);

my $filename = $ARGV[0];
map_file my ($map), $filename;
my $size = -s $filename;
#print $size."\n";
# expected: 187236078
my $pos = $size - 20000;
sub test_position() {
    #    print chr(substr($map, $pos, 1));
    return ord(substr($map, $pos, 1)) eq 1 &&
        ord(substr($map, $pos + 2, 1)) eq 42 &&
        ord(substr($map, $pos + 3, 1)) eq 97 &&
        ord(substr($map, $pos + 4, 1)) eq 99 &&
        ord(substr($map, $pos + 5, 1)) eq 95;
};

my $found = 0;

while(!$found && $pos > 0) {
    if (test_position) {
        $found = 1;
    } else { $pos--; }
}
$pos -= 12;
$| = 1;
#$pos = -s $filename;
close $map;
while(1) {
    my $current_size = -s $filename;
    my $to_read = $current_size - $pos;
    if ( $to_read ) {
        map_file my ($map), $filename;
        print substr($map, $pos, $to_read);
        close $map;
    }
    $pos += $to_read;
#    sleep 1;
}
