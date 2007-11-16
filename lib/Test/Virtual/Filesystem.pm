#######################################################################
#      $URL: svn+ssh://equilibrious@equilibrious.net/home/equilibrious/svnrepos/chrisdolan/Test-Virtual-Filesystem/lib/Test/Virtual/Filesystem.pm $
#     $Date: 2007-11-16 01:29:27 -0600 (Fri, 16 Nov 2007) $
#   $Author: equilibrious $
# $Revision: 703 $
########################################################################

package Test::Virtual::Filesystem;

use warnings;
use strict;
use English qw(-no_match_vars);
use Carp qw(croak);
use File::Temp qw();
use File::Path qw();
use File::Spec;
use List::MoreUtils qw(any);
use Attribute::Handlers;
use Test::More;
use base 'Test::Class';

our $VERSION = '0.02';

my $CAN_USE_XATTR;
eval {
   require File::ExtAttr;
   $CAN_USE_XATTR = 1;
};

=pod

=for stopwords TODO CPAN

=head1 NAME

Test::Virtual::Filesystem - Validate a filesystem

=head1 SYNOPSIS

    use Test::Virtual::Filesystem;
    Test::Virtual::Filesystem->new('/path/to/test')->runtests;

or with more customization:

    use Test::Virtual::Filesystem;
    my $test = Test::Virtual::Filesystem->new('/path/to/test');
    $test->test_xattr(1);
    $test->test_chown(1);
    $test->test_atime(1);
    $test->runtests;

=head1 LICENSE

Copyright 2007 Chris Dolan, I<cdolan@cpan.org>

This library is free software; you can redistribute it and/or modify it
under the same terms as Perl itself.

=head1 DESCRIPTION

If you are creating a filesystem, say via L<Fuse> or
L<Filesys::Virtual>, you need a fairly mundane set of tests to try out
lots of typical filesystem operations.  This package attempts to
accumulate a bunch of those tests to make it easier for you to test
your filesystem.

=head1 COMPATIBILITY POLICY

Every time we add a new test to this suite, we annotate it with a
version number.  If client code specifies an expected version number
(say, 1.10) and it's running against a newer version or this module
(say, 1.20) then any newer test will be marked as a TODO test.  That
way if the test fails, it won't regress published code that used to
work.

This policy will allow us to continue adding new filesystem tests
without worrying about breaking existing CPAN modules.

=head1 CAVEATS AND LIMITATIONS

This test class needs a more complete suite of test cases.  In
particular, tests are needed for the following filesystem features:

    extended attributes (xattr)
    multiple symlinks
    recursive symlinks
    hardlinks
    a/u/ctime
    nlink
    chown
    deep directories
    very full directories
    large files
    binary files
    files with awkward characters: EOF, NUL
    non-ASCII filenames (maybe constructor should specify the encoding?)
    permissions
    special file types (fifos, sockets, character and block devices, etc)
    file locking
    threading and re-entrancy
    truncate
    binmode
    eof
    fileno
    seek/rewinddir, tell/telldir
    read, sysread, syswrite
    async I/O?

Any help writing tests (or adapting tests from existing suites) will
be appreciated!

=head1 METHODS

This is a subclass of L<Test::Class>.  All methods from that class are
available, particularly C<runtests()>.

=over

=item $pkg->new({mountdir =E<gt> $mountdir, ...})

Create a new test which will operate on files contained within the
specified mount directory.  WARNING: any and all files and folders in
that mount directory will be deleted!

The supported options are:

=over

=item C<mountdir>

This required property indicates where tests should run.

=item C<compatible>

Specify a Test::Virtual::Filesystem version number that is known to
work.  If the actual Test::Virtual::Filesystem version number is
greater, then any test cases added after the specified compatible
version are considered C<TODO> tests.  See L<Test::More> for details
about C<TODO> tests.

=back

=item $self->init()

Invoked just before then end of C<new()>.  This exists solely for
subclassing convenience.  This implementation does nothing.

=back

=head1 PROPERTIES

The following accessor/mutator methods exist to turn on/off various
features.  they all behave in usual Perl fashion: with no argument,
they return the current value.  With one argument, they set the
current value and return void.

=over

=item $self->test_all()

Turns on all the tests.  Default false.

=item $self->test_xattr()

Default false.

=item $self->test_time()

Default true.  If set false, it also sets C<atime>, C<ctime> and C<utime> false.

=item $self->test_atime()

Default false.

=item $self->test_utime()

Default true.

=item $self->test_ctime()

Default true.

=item $self->test_permissions()

Default false.

=item $self->test_special()

Default true.  If set false, it also sets C<fifo> false.

=item $self->test_fifo()

Default false.  AKA named pipes.

=item $self->test_symlink()

Default true, except C<$^O eq 'MSWin32'> where it defaults to false.

=item $self->test_hardlink()

Default true.

=item $self->test_nlink()

Default true.

=item $self->test_chown()

Default false.

=back

=head1 TEST CASES

=over

=cut

sub new {
   my ($pkg, $opts) = @_;
   my $self = $pkg->SUPER::new();
   $opts ||= {};
   for my $key (qw(mountdir compatible)) {
      $self->{$key} = $opts->{$key};
   }
   $self->{fs_opts} = {
      'all' => 0,
      'xattr' => 0,
      'time' => {
         'atime' => 0,
         'utime' => 1,
         'ctime' => 1,
      },
      'permissions' => 0,
      'special' => {
         'fifo' => 0,
      },
      'symlink' => 1,
      'hardlink' => 1,
      'nlink' => 1,
      'chown' => 0,
   };
   $self->init;
   return $self;
}

sub init {
   # no-op, subclasses may override
   return;
}

{
   # Create a read-write accessor for each enabling property in fs_opts
   no strict 'refs';  ##no critic(NoStrict)
   my $opts = __PACKAGE__->new->{fs_opts};
   for my $field (keys %{$opts}) {
      *{'test_'.$field} = sub {
         return $_[0]->{fs_opts}->{$field} if @_ == 1;
         return $_[0]->{fs_opts}->{$field} = $_[1] if @_ == 2;
         croak 'wrong number of arguments to ' . $field;
      };
      my $val = $opts->{$field};
      if (ref $val) {
         for my $subfield (keys %{$val}) {
            *{'test_'.$subfield} = sub {
               return $_[0]->{fs_opts}->{$field} && $_[0]->{fs_opts}->{$field}->{$subfield} if @_ == 1;
               return ($_[0]->{fs_opts}->{$field} ||= {})->{$subfield} = $_[1] if @_ == 2;
               croak 'wrong number of arguments to ' . $subfield;
            };
         }
      }
   }
}

=item setup()

Runs before every test to prepare a directory for testing.

=cut

sub setup : Test(setup => 1) {
   my ($self) = @_;
   if (!defined $self->{mountdir}) {
      croak 'Programmer error: you did not specify a mountdir';
   }
   if (!-d $self->{mountdir}) {
      croak "Your mountdir '$self->{mountdir}' is not a valid directory";
   }
   if (!File::Spec->file_name_is_absolute($self->{mountdir})) {
      croak "Your mountdir '$self->{mountdir}' is not an absolute path";
   }
   if (File::Spec->splitdir($self->{mountdir}) <= 2) {
      croak "Your mountdir '$self->{mountdir}' is too close to the root of the filesystem." .
          '  I am too scared of deleting important files to use it';
   }
   $self->{tempdir} = File::Spec->catdir($self->{mountdir}, 'testdir');
   mkdir $self->{tempdir};
   ok(-d $self->{tempdir}, 'Created tempdir');
   return;
}

=item teardown()

Runs after every test to clean up the test directory so the next test
will have a clean workspace.

=cut

sub teardown : Test(teardown => 1) {
   my ($self) = @_;
   my $tmpdir = delete $self->{tempdir};
   if (defined $tmpdir && -e $tmpdir) {
      $self->_cleandir($tmpdir);
      if ($tmpdir ne $self->{mountdir}) {
         rmdir $tmpdir or die $OS_ERROR;
      }
   }
   ok(!defined $tmpdir || !-d $tmpdir, 'Removed tempdir');
   return;
}

sub _cleandir {
   my ($self, $dir) = @_;
   for my $file ($self->_read_dir($dir)) {
      next if q{.} eq $file;
      next if q{..} eq $file;
      my $path = File::Spec->catfile($dir, $file);
      die 'Internal error: escaped the temp space!' if length $path <= length $self->{mountdir};
      die 'nonsense missing file: ' . $path if !-e $path;
      if (-d $path) {
         $self->_cleandir($path);
         rmdir $path or die $OS_ERROR;
      } else {
         unlink $path or die $OS_ERROR;
      }
   }
   return;
}

=item Introduced($version)

A subroutine attribute used to flag the Test::Virtual::Filesystem
version number when that test was introduced.  It's used like this:

  sub open_nonexistent_file : Tests(1) : Introduced('0.02') {
     ok(!open(my $f, '<', '/tmp/no_such_file'));
  }

=cut

sub Introduced : ATTR(CODE) { ##no critic(MixedCase)
   my ($class, $symbol, $code_ref, $attr, $introduced_version) = @_;
   if ($symbol eq 'ANON') {
      warn 'cannot test anonymous subs - you probably loaded ' . __PACKAGE__ . ' too late.' .
          ' (after the CHECK block was run)';
   } else {
      # Wrap the sub in a version test
      no warnings 'redefine';  ##no critic(TestingAndDebugging::ProhibitNoWarnings)
      *{$symbol} = sub {
         local $TODO = $_[0]->_compatible($introduced_version); ## no critic(Local)
         $code_ref->(@_);
      };
      #print STDERR "record $class\::$name as $args\n";
   }
   return;
}
sub _compatible {
   my ($self, $introduced_version) = @_;
   return if !$self->{compatible};
   return if $introduced_version le $self->{compatible};
   return 'compatibility mode ' . $self->{compatible};
}

=item Features($featurelist)

This is a subroutine attribute to specify one or more features used in
the test.  The features should be listed as a comma-separated list:

  sub symlink_create : Tests(1) : Features('symlink') {
     ok(symlink($src, $dest));
  }
  sub symlink_permissions : Tests(2) : Features('symlink, permissions') {
     ok(symlink($src, $dest));
     ok(-w $dest);
  }

Subfeatures must be separated from their parent features by a C</>.  For example:

  sub atime_utime_set : Tests(1) : Features('time/atime, time/utime') {
     my $now = time;
     ok(utime($now, $now, $file));
  }

Look at the source code for C<new()> to see the supported features and
subfeatures.  The C<test_*> methods above describe the all the
features, but in those methods the subfeature names are flattened.

=cut

sub Features : ATTR(CODE) { ##no critic(MixedCase)
   my ($class, $symbol, $code_ref, $attr, $features) = @_;
   if ($symbol eq 'ANON') {
      warn 'cannot test anonymous subs - you probably loaded ' . __PACKAGE__ . ' too late.' .
          ' (after the CHECK block was run)';
   } else {
      my @features = split m/,\s*/xms, $features;
      my $name = *{$symbol}{NAME};
      # Wrap the sub in a feature test
      no warnings 'redefine';  ##no critic(TestingAndDebugging::ProhibitNoWarnings)
      *{$symbol} = sub {
       SKIP: {
          my $blocking_feature = $_[0]->_blocking_feature(@features);
          if ($blocking_feature) {
             my $numtests = __PACKAGE__->num_method_tests($name);
             skip 'feature unsupported: ' . $blocking_feature, $numtests;
          }
          $code_ref->(@_);
         }
      };
   }
   return;
}
sub _blocking_feature {
   my ($self, @features) = @_;
   return if $self->{fs_opts}->{all};
   for my $feature (@features) {
      my $opts = $self->{fs_opts};
      for my $part (split m{/}xms, $feature) {
         return $feature if !ref $opts;
         return $feature if !$opts->{$part};
         $opts = $opts->{$part};
      }
   }
   return;
}


sub _compatibility_test : Test(1) : Introduced('0.02') {
   # introduced in v0.02 I<solely> to test the compatibility feature.
   my ($self) = @_;
   pass('compatibility_test');
   return;
}

sub _features_test : Test(1) : Introduced('0.02') : Features('features_test') {
   # introduced in v0.02 I<solely> to test the 'features' feature.
   my ($self) = @_;
   pass('features_test');
   return;
}


=item stat_dir(), introduced in v0.01

=cut

sub stat_dir : Test(6) : Introduced('0.01') {
   my ($self) = @_;
   my $f = $self->_file(q{/});
   ok(-e $f, 'mount dir exists');
   ok(-d $f, 'mount dir is a dir');
   ok(!-f $f, 'mount dir is not a file');
   ok(!-l $f, 'mount dir is not a symlink');
   ok(-r $f, 'mount dir is readable');
   ok(-x $f, 'mount dir is searchable');
   return;
}

=item stat_dir_size(), introduced in v0.02

=cut

sub stat_dir_size : Test(1) : Introduced('0.02') {
   my ($self) = @_;
   my $f = $self->_file(q{/});
   ok(-s $f, 'mount dir has non-zero size');
   return;
}

=item read_dir(), introduced in v0.01

=cut

sub read_dir : Test(3) : Introduced('0.01') {
   my ($self) = @_;
   my $f = $self->_file(q{/});
   my @files = $self->_read_dir($f);
   cmp_ok(scalar @files, '>=', 2, 'dir contains at least two entries');
   ok((any { $_ eq q{.} }  @files), 'dir contains "."');
   ok((any { $_ eq q{..} } @files), 'dir contains ".."');
   return;
}

=item read_dir_fail(), introduced in v0.01

=cut

sub read_dir_fail : Test(2) : Introduced('0.01') {
   my ($self) = @_;
   my $f = $self->_file('/no_such');
   eval {
      $self->_read_dir($f);
   };
   like($EVAL_ERROR && qq{$EVAL_ERROR}, qr/No [ ] such/xms, 'read non-existent dir');
   ok(!-e $f, 'did not make dir');
   return;
}

=item read_file_fail(), introduced in v0.01

=cut

sub read_file_fail : Test(2) : Introduced('0.01') {
   my ($self) = @_;
   my $f = $self->_file('/read_file_fail');
   my $content = "content\n";
   eval {
      $self->_read_file($f);
   };
   like($EVAL_ERROR && qq{$EVAL_ERROR}, qr/No [ ] such/xms, 'read non-existent file');
   ok(!-e $f, 'did not make file');
   return;
}

=item write_empty_file(), introduced in v0.01

=cut

sub write_empty_file : Test(2) : Introduced('0.01') {
   my ($self) = @_;
   my $f = $self->_file('/create_file');
   $self->_write_file($f);
   ok(-f $f, 'created empty file');
   is(-s $f, 0, 'file got right size');
   return;
}

=item write_file(), introduced in v0.01

=cut

sub write_file : Test(2) : Introduced('0.01') {
   my ($self) = @_;
   my $f = $self->_file('/write_file');
   my $content = "content\n";
   $self->_write_file($f, $content);
   ok(-f $f, 'wrote file');
   is(-s $f, length $content, 'file got right size');
   return;
}

=item write_file_subdir_fail(), introduced in v0.01

=cut

sub write_file_subdir_fail : Test(2) : Introduced('0.01') {
   my ($self) = @_;
   my $f = $self->_file('/no_such/write_file');
   my $content = "content\n";
   eval {
      $self->_write_file($f, $content);
   };
   like($EVAL_ERROR && qq{$EVAL_ERROR}, qr/No [ ] such/xms, 'write to non-existent folder');
   ok(!-f $f, 'did not make file');
   return;
}

=item write_append_file(), introduced in v0.01

=cut

sub write_append_file : Test(2) : Introduced('0.01') {
   my ($self) = @_;
   my $f = $self->_file('/append_file');
   my $content = "content\n";
   $self->_write_file($f, $content);
   $self->_append_file($f, $content);
   ok(-f $f, 'wrote file');
   ok(-s $f == 2 * length $content, 'file got right size');
   return;
}

## Perl's '>>' creates the file if it doesn't exist
#
#=item write_append_file_fail(), introduced in v0.01
#
#=cut
#
# sub write_append_file_fail : Test(2) : Introduced('0.01') {
#    my ($self) = @_;
#    my $f = $self->_file('/append_file_fail');
#    my $content = "content\n";
#    eval {
#       $self->_append_file($f, $content);
#    };
#    like($EVAL_ERROR && qq{$EVAL_ERROR}, qr/No [ ] such/xms, 'append to non-existent file');
#    ok(!-f $f, 'did not make file');
#    return;
# }

=item write_read_file(), introduced in v0.01

=cut

sub write_read_file : Test(1) : Introduced('0.01') {
   my ($self) = @_;
   my $f = $self->_file('/read_file');
   my $content = "content\n";
   $self->_write_file($f, $content);
   is($self->_read_file($f), $content, 'read file');
   return;
}

=item write_unlink_file(), introduced in v0.01

=cut

sub write_unlink_file : Test(3) : Introduced('0.01') {
   my ($self) = @_;
   my $f = $self->_file('/read_file');
   my $content = "content\n";
   $self->_write_file($f, $content);
   ok(-e $f, 'file exists');
   ok(-f $f, 'file is a file');
   unlink $f or die $OS_ERROR;
   ok(!-f $f, 'file is deleted');
   return;
}

=item write_mkdir(), introduced in v0.01

=cut

sub write_mkdir : Test(1) : Introduced('0.01') {
   my ($self) = @_;
   my $f = $self->_file('/mk_dir');
   mkdir $f or die $OS_ERROR;
   ok(-d $f, 'made dir');
   return;
}

=item write_mkdir_fail(), introduced in v0.01

=cut

sub write_mkdir_fail : Test(2) : Introduced('0.01') {
   my ($self) = @_;
   my $f = $self->_file('/no_such/mk_dir');
   eval {
      mkdir $f or die $OS_ERROR;
   };
   like($EVAL_ERROR && qq{$EVAL_ERROR}, qr/No [ ] such/xms, 'mkdir at non-existent path');
   ok(!-d $f, 'did not make dir');
   return;
}

=item write_rmdir(), introduced in v0.01

=cut

sub write_rmdir : Test(2) : Introduced('0.01') {
   my ($self) = @_;
   my $f = $self->_file('/rm_dir');
   mkdir $f or die $OS_ERROR;
   ok(-d $f, 'made dir');
   rmdir $f or die $OS_ERROR;
   ok(!-d $f, 'made dir');
   return;
}

=item write_subdir(), introduced in v0.01

=cut

sub write_subdir : Test(3) : Introduced('0.01') {
   my ($self) = @_;
   my $d = $self->_file('/mk_dir');
   my $f = $self->_file('/mk_dir/file');
   my $content = "content\n";
   mkdir $d or die $OS_ERROR;
   ok(-d $d, 'made dir');
   $self->_write_file($f, $content);
   ok(-f $f, 'wrote file in subdir');
   is($self->_read_file($f), $content, 'right content');
   return;
}

=item symlink_create(), introduced in v0.02

=cut

sub symlink_create : Test(10) : Introduced('0.02') : Features('symlink') {
   my ($self) = @_;
   my $src = $self->_file('/symlink_target');
   mkdir $src or die $OS_ERROR;
   ok(-e $src, 'symlink source exists');
   my $s = $self->_file('/symlink_create');
   ok(symlink($src, $s), 'created symlink') or die $OS_ERROR;
   ok(-e $s, 'symlink exists');
   ok(-l $s, 'symlink is a symlink');
   ok(-d $s, 'symlink src is a dir');
   ok(!-f $s, 'symlink src is not a file');
   is(-s $s, -s $src, 'symlink size is size of source');
   is(readlink $s, $src, 'read newly created symlink');
   unlink $s or die $OS_ERROR;
   ok(!-e $s, 'symlink deleted');
   ok(-e $src, 'symlink source not deleted');
   return;
}

=item xattr_list(), introduced in v0.02

=cut

sub xattr_list : Test(1) : Introduced('0.02') : Features('xattr') {
   my ($self) = @_;
 SKIP: {
      if (!$CAN_USE_XATTR) {
         skip 'Optional File::ExtAttr not found', $self->num_method_tests('xattr_list');
      }
      my $f = $self->_file(q{/});
      my @attrs = File::ExtAttr::listfattr($f);
      ok(@attrs == 0 || defined $attrs[0], 'got xattr list');
   }
   return;
}

=item xattr_set(), introduced in v0.02

=cut

sub xattr_set : Test(9) : Introduced('0.02') : Features('xattr') {
   my ($self) = @_;
 SKIP: {
      if (!$CAN_USE_XATTR) {
         skip 'Optional File::ExtAttr not found', $self->num_method_tests('xattr_list');
      }
      my $f = $self->_file('/foo');
      $self->_write_file($f);

      my $xattr_key = 'org.cpan.cdolan';
      my $xattr_value = 'test';
      my $xattr_replace = 'test2';

      # just in case, clean up!
      if (defined File::ExtAttr::getfattr($f, $xattr_key)) {
         File::ExtAttr::delfattr($f, $xattr_key);
      }
      {
         # File::ExtAttr doesn't look at $^W or 'no warnings'.  Grr...
         local $SIG{__WARN__} = sub {};
         ok(!File::ExtAttr::setfattr($f, $xattr_key, $xattr_value, {replace => 1}), 'cannot replace missing xattr');
      }
      ok(File::ExtAttr::setfattr($f, $xattr_key, $xattr_value, {create => 1}), 'create xattr');
      is(File::ExtAttr::getfattr($f, $xattr_key), $xattr_value, 'get xattr');
      ok((any {$xattr_key eq $_} File::ExtAttr::listfattr($f)), 'list xattr');
      {
         local $SIG{__WARN__} = sub {};
         ok(!File::ExtAttr::setfattr($f, $xattr_key, $xattr_value, {create => 1}), 'cannot create existing xattr');
      }
      ok(File::ExtAttr::setfattr($f, $xattr_key, $xattr_replace, {replace => 1}), 'replace xattr');
      is(File::ExtAttr::getfattr($f, $xattr_key), $xattr_replace, 'get xattr');
      ok(File::ExtAttr::delfattr($f, $xattr_key), 'delete xattr');
      is(File::ExtAttr::getfattr($f, $xattr_key), undef, 'xattr deleted');

      unlink $f;
   }
   return;
}

#       xattr => 1,
#       time => {
#          utime => 1,
#          atime => 1,
#          ctime => 1,
#       },
#       perm => 1,
#       special => {
#          fifo => 1,
#       },
#       symlink => 1,
#       hardlink => 1,
#       nlink => 1,
#       chown => 1,

######### helpers ########

sub _file {
   my ($self, $path) = @_;
   $path =~ s{\A /}{}xms or croak 'test paths must be absolute';
   return File::Spec->catfile($self->{tempdir}, split m{/}xms, $path);
}

sub _write_file {
   my ($self, $f, @content) = @_;
   open my $fh, '>', $f or die $OS_ERROR;
   for my $content (@content) {
      print {$fh} $content or die $OS_ERROR;
   }
   close $fh or die $OS_ERROR;
   return;
}

sub _append_file {
   my ($self, $f, @content) = @_;
   open my $fh, '>>', $f or die $OS_ERROR;
   for my $content (@content) {
      print {$fh} $content or die $OS_ERROR;
   }
   close $fh or die $OS_ERROR;
   return;
}

sub _read_file {
   my ($self, $f) = @_;
   open my $fh, '<', $f or die $OS_ERROR;
   my $content = do { $/ = undef; <$fh> };   ##no critic(PunctuationVars)
   close $fh or die $OS_ERROR;
   return $content;
}

sub _read_dir {
   my ($self, $f) = @_;
   opendir my $fh, $f or die $OS_ERROR;
   my @content = readdir $fh;
   closedir $fh or die $OS_ERROR;
   return @content;
}

1;

__END__

=pod

=back

=head1 SEE ALSO

L<Test::Class>

L<Fuse::PDF>

=head1 AUTHOR

Chris Dolan, I<cdolan@cpan.org>

=cut

# Local Variables:
#   mode: perl
#   perl-indent-level: 3
#   cperl-indent-level: 3
#   fill-column: 78
#   indent-tabs-mode: nil
#   c-indentation-style: bsd
# End:
# ex: set ts=8 sts=4 sw=4 tw=78 ft=perl expandtab :
