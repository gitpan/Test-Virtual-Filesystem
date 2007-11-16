use warnings;
use strict;
use File::Temp qw();
use Test::More;

use Test::Virtual::Filesystem;

my $tmpdir = File::Temp::tempdir('filesys_test_XXXX', CLEANUP => 1, TMPDIR => 1);
my $test = Test::Virtual::Filesystem->new({mountdir => $tmpdir});
if ($ENV{TEST_AUTHOR}) {
   $test->test_all(1);
}
$test->runtests;

__END__

# Local Variables:
#   mode: perl
#   perl-indent-level: 3
#   cperl-indent-level: 3
#   fill-column: 78
#   indent-tabs-mode: nil
#   c-indentation-style: bsd
# End:
# ex: set ts=8 sts=4 sw=4 tw=78 ft=perl expandtab :
