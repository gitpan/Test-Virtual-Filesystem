use warnings;
use strict;
use File::Temp qw();
use Test::More;

use Test::Virtual::Filesystem;

my $tmpdir = File::Temp::tempdir('filesys_test_XXXX', CLEANUP => 1, TMPDIR => 1);
my $test = Test::Virtual::Filesystem->new({mountdir => $tmpdir})->runtests;

1;
