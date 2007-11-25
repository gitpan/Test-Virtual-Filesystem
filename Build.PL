use Module::Build;

Module::Build->new(
   module_name => 'Test::Virtual::Filesystem',
   dist_author => 'Chris Dolan <cdolan@cpan.org>',
   license => 'perl',
   requires => {
      'perl' => '5.8.0',
      'List::MoreUtils' => 0,
      'Test::Class' => '0.24',
      'Test::More' => 0.62,
      'Attribute::Handlers' => 0, # implied by Test::Class, but just in case...
      'Readonly' => 1.00,
   },
   recommends => {
      # Optional for testing extended attributes
      'File::ExtAttr' => 1,

      # Optional in tests, skipped if not present
      'Test::Pod' => 0,
      'Test::Pod::Coverage' => 0,
   },
   create_readme => 1,
   create_makefile_pl => 'traditional',
)->create_build_script;
