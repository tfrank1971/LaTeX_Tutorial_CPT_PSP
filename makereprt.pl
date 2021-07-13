#!/usr/bin/perl

use warnings;

use Getopt::Long;

$userName =  $ENV{'LOGNAME'}; 

$LATEXTMPLDIR = "/home/user/documents/ltxtmpl"; #this is the source directory of the template


sub replaceString {
  my ($regexp, $in, $out) = @_;

  open(FH,$in) or die("Can't open $in");
  open(FO,">$out") or die("Can't open $out");
  while ($line = <FH>) {
    map { $line =~ s/$_/$regexp->{$_}/g; } keys %$regexp;
    print FO $line;
  }
  close(FH);
  close(FO);
}

$force = 0;
GetOptions("force!" => \$force);

umask 0002;

$PWD = $ENV{PWD};
$NDIR = $ARGV[0];

die "No project name specified!\n" if !$NDIR;

die "Project $NDIR already exists!\n" if -d $NDIR && !$force;

$NPWD = $PWD . "/" . $NDIR;

if (! -d $NDIR) {
	print "Creating Directory: $NDIR\n";
	mkdir($NDIR, 0777) or die "Can't mkdir $NDIR: $!"; 
}
chdir($NDIR) or die "Can't chdir to $NDIR: $!"; 

$DDIR = "report";
$DDAT = "data";
$DANA = "analysis";
$DANAC = "analysis/comparison";
$DANQS1 = "analysis/nmnqs";
$DANQS2 = "analysis/comparison/nmnqs";
$DANT1 = "analysis/tables";
$DANP1 = "analysis/plots";

map {if (! mkdir($_, 0777) && !$force) { die "Can't mkdir $_: $!" } } $DDIR, $DDAT, $DANA,$DANAC, $DANQS1, $DANQS2, $DANT1,$DANP1;

chdir "$NPWD/$DDIR" or die "Can't chdir to $NPWD/$DDIR: $!";


# get list of tex files used
my @lfiles = ("myacronyms.tex",".latexmkrc");
opendir LDIR, "$LATEXTMPLDIR";
map { -f "$LATEXTMPLDIR/$_" && /^#report#.*$/ && push @lfiles, $_; } readdir(LDIR);
closedir LDIR;

 
my %re = ("#report#" => "$NDIR", "#InsertDirectoryHere#" => "$PWD\/$NDIR\/$DDIR");

print "Processing file Latex:";
for $in (@lfiles) {
  # process the filename
  $out = $in;
  map { $out =~ s/\Q$_\E/\Q$re{$_}\E/; } keys %re;
  print " $out";
  # replace the strings within the files
  replaceString(\%re, "$LATEXTMPLDIR/$in", $out);
}
print " Done\n";

# file copying 
my $fcp=`rsync -avx $LATEXTMPLDIR/textblocks/ $PWD/$NDIR/report/textblocks/`;
$fcp=`rsync -avx $LATEXTMPLDIR/logo/ $PWD/$NDIR/report/logo/`;
$fcp=`rsync -avx $LATEXTMPLDIR/license/ $PWD/$NDIR/report/license/`;
$fcp=`rsync -avx $LATEXTMPLDIR/nonmem/ $PWD/$NDIR/analysis/nmnqs/`;
$fcp=`rsync -avx $LATEXTMPLDIR/Rscripts/ $PWD/$NDIR/analysis/`;
$fcp=`rsync -avx $LATEXTMPLDIR/Rfunctions/ $PWD/$NDIR/analysis/.R/`;
$fcp=`rsync -avx $LATEXTMPLDIR/Rresults/comparison/ $PWD/$NDIR/analysis/comparison/`;
$fcp=`rsync -avx $LATEXTMPLDIR/Rresults/plots/ $PWD/$NDIR/analysis/plots/`;
$fcp=`rsync -avx $LATEXTMPLDIR/Rresults/tables/ $PWD/$NDIR/analysis/tables/`;
$fcp=`rsync -avx $LATEXTMPLDIR/data/ $PWD/$NDIR/data/`;

my $smbln=`ln -s $PWD/$NDIR/data/data_set_504.dat $PWD/$NDIR/analysis/`;

print "\n\nCongratulation, $userName, your NEW PROJECT: $NDIR\n has been created successfully!\n";


exit 0;
