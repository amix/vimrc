#!/usr/bin/env perl
# vimparse.pl - Reformats the error messages of the Perl interpreter for use
# with the quickfix mode of Vim
#
# Copyright (c) 2001 by Jörg Ziefle <joerg.ziefle@gmx.de>
# Copyright (c) 2012 Eric Harmon <http://eharmon.net>
# You may use and distribute this software under the same terms as Perl itself.
#
# Usage: put one of the two configurations below in your ~/.vimrc (without the
# description and '# ') and enjoy (be sure to adjust the paths to vimparse.pl
# before):
#
# Program is run interactively with 'perl -w':
#
# set makeprg=$HOME/bin/vimparse.pl\ %\ $*
# set errorformat=%t:%f:%l:%m
#
# Program is only compiled with 'perl -wc':
#
# set makeprg=$HOME/bin/vimparse.pl\ -c\ %\ $*
# set errorformat=%t:%f:%l:%m
#
# Usage:
#	vimparse.pl [-c] [-w] [-f <errorfile>] <programfile> [programargs]
#
#		-c	compile only, don't run (perl -wc)
#		-w	output warnings as warnings instead of errors (slightly slower)
#		-f	write errors to <errorfile>
#
# Example usages:
#	* From the command line:
#		vimparse.pl program.pl
#
#		vimparse.pl -c -w -f errorfile program.pl
#		Then run vim -q errorfile to edit the errors with Vim.
#		This uses the custom errorformat: %t:%f:%l:%m.
#
#	* From Vim:
#		Edit in Vim (and save, if you don't have autowrite on), then
#		type ':mak' or ':mak args' (args being the program arguments)
#		to error check.
#
# Version history:
#	0.3 (05/31/2012):
#		* Added support for the seperate display of warnings
#		* Switched output format to %t:%f:%l:%m to support error levels
#	0.2 (04/12/2001):
#		* First public version (sent to Bram)
#		* -c command line option for compiling only
#		* grammatical fix: 'There was 1 error.'
#		* bug fix for multiple arguments
#		* more error checks
#		* documentation (top of file, &usage)
#		* minor code clean ups
#	0.1 (02/02/2001):
#		* Initial version
#		* Basic functionality
#
# Todo:
#	* test on more systems
#	* use portable way to determine the location of perl ('use Config')
#	* include option that shows perldiag messages for each error
#	* allow to pass in program by STDIN
#	* more intuitive behaviour if no error is found (show message)
#
# Tested under SunOS 5.7 with Perl 5.6.0.  Let me know if it's not working for
# you.
use warnings;
use strict;
use Getopt::Std;
use File::Temp qw( tempfile );

use vars qw/$opt_I $opt_c $opt_w $opt_f $opt_h/; # needed for Getopt in combination with use strict 'vars'

use constant VERSION => 0.2;

getopts('cwf:hI:');

&usage if $opt_h; # not necessarily needed, but good for further extension

if (defined $opt_f) {

    open FILE, "> $opt_f" or do {
	warn "Couldn't open $opt_f: $!.  Using STDOUT instead.\n";
	undef $opt_f;
    };

};

my $handle = (defined $opt_f ? \*FILE : \*STDOUT);

(my $file = shift) or &usage; # display usage if no filename is supplied
my $args = (@ARGV ? ' ' . join ' ', @ARGV : '');

if ($file eq '-') { # make STDIN seek-able, so it can be read twice
    my $fh = tempfile();
    print {$fh} <STDIN>;
    open \*STDIN, '<&', $fh or die "open: $!";
    seek \*STDIN, 0, 0 or die "seek: $!";
}

my $libs = join ' ', map {"-I$_"} split ',', $opt_I || '';
my @error_lines = `$^X $libs @{[defined $opt_c ? '-c ' : '' ]} @{[defined $opt_w ? '-X ' : '-Mwarnings ']} "$file$args" 2>&1`;

my @lines = map { "E:$_" } @error_lines;

my @warn_lines;
if(defined($opt_w)) {
    if ($file eq '-') {
        seek \*STDIN, 0, 0 or die "seek: $!";
    }
    @warn_lines = `$^X $libs @{[defined $opt_c ? '-c ' : '' ]} -Mwarnings "$file$args" 2>&1`;
}

# Any new errors must be warnings
foreach my $line (@warn_lines) {
	if(!grep { $_ eq $line } @error_lines) {
		push(@lines, "W:$line");
	}
}

my $errors = 0;
foreach my $line (@lines) {

    chomp($line);
    my ($file, $lineno, $message, $rest, $severity);

    if ($line =~ /^([EW]):(.*)\sat\s(.*)\sline\s(\d+)(.*)$/) {
	($severity, $message, $file, $lineno, $rest) = ($1, $2, $3, $4, $5);
	$errors++;
	$message .= $rest if ($rest =~ s/^,//);
	print $handle "$severity:$file:$lineno:$message\n";

    } else { next };

}

if (defined $opt_f) {

    my $msg;
    if ($errors == 1) {

	$msg = "There was 1 error.\n";

    } else {

	$msg = "There were $errors errors.\n";

    };

    print STDOUT $msg;
    close FILE;
    unlink $opt_f unless $errors;

};

sub usage {

    (local $0 = $0) =~ s/^.*\/([^\/]+)$/$1/; # remove path from name of program
    print<<EOT;
Usage:
	$0 [-c] [-w] [-f <errorfile>] <programfile> [programargs]

		-c	compile only, don't run (executes 'perl -c')
		-w	output warnings as warnings instead of errors (slightly slower)
		-f	write errors to <errorfile>
		-I	specify \@INC/#include directory <perl_lib_path>

Examples:
	* At the command line:
		$0 program.pl
		Displays output on STDOUT.

		$0 -c -w -f errorfile program.pl
		Then run 'vim -q errorfile' to edit the errors with Vim.
		This uses the custom errorformat: %t:%f:%l:%m.

	* In Vim:
		Edit in Vim (and save, if you don't have autowrite on), then
		type ':mak' or ':mak args' (args being the program arguments)
		to error check.
EOT

    exit 0;

};
