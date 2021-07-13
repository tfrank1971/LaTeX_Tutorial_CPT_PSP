#!/usr/bin/perl -w

use Getopt::Long;
&Getopt::Long::config("no_ignore_case");

$file=0;
my $result=&GetOptions(
                       "file:s",   \$file
                      );

if (!$file) {
        die ("no file given");
}
open (FILE, $file);

my (@FIT, $LINE, @ETABARF, @SE, @P, @SHRINKETA, @SHRINKEPS, @SHRINKEBV);
@FIT=();
$an=0;
while (<FILE>) { 
 
  if ($_ =~ / \#TERE:/) {
    $an=0;
  }
  if ($_ =~ / \#TERM:/) {
    $an=1;
    @FIT=();
  }
  if ( $an==1 ){ 
    push(@FIT,$_);
    #print $_;
    }
}

close (FILE);
#$an=0;

#print @FIT;

push(@FIT," #TERE:");
my @FITBACK=@FIT;

@ETABARF=&getfit("ETABAR:","SE:",@FIT);
## sub for extracting parts of @FIT
@SE=&getfit("SE:",'P VAL.:|N:',@FIT);

@P=&getfit("P VAL.:",'ETAshrink|SHRINK:|ETASHRINKSD\(%\)',@FIT);
@SHRINKETA=&getfit('SHRINK:|ETAshrink\(%\):|ETASHRINKSD\(%\)','Sigshrink:|EPSshrink\(%\):|EBVshrink\(%\):|ETASHRINKVR\(%\)',@FIT);
@SHRINKEBV=&getfit('EBVshrink\(%\):|EBVSHRINKSD\(%\)','EPSshrink\(%\):|EBVSHRINKVR\(%\)',@FIT);
@SHRINKEPS=&getfit('Sigshrink:|EPSshrink\(%\):|EPSSHRINKSD\(%\)','#TERE:|EPSSHRINKVR\(%\)',@FIT);

sub getfit {
my ($STOP,$START,$an,@FITRETURN,@FITT);
@FITRETURN=();
$an=0;
$START=$_[0];
$STOP=$_[1];
@FITT=$_[2];
foreach $LINE (@FIT) {
  chomp($LINE);
  if ( $LINE =~ /$STOP/ ) {
    $an=0;
  }
  if ( $LINE =~ /$START/ ) {
    $an=1;
  }
  if ( $an==1 ){
    $LINE =~ s/$START//;
    $LINE =~ s/^\s//;
    $LINE =~ s/\s+/ /;
    
    push(@FITRETURN,$LINE);
  }
}
  @FITRETURN;
}
  open(ETABARF,"> ETABAR.txt");
  open(SHRINKETAF,"> ETASHRINK.txt");
  open(SHRINKSIGF,"> SIGSHRINK.txt");


  print ETABARF "@ETABARF \n";
  print ETABARF "@SE \n";
  print ETABARF "@P \n";
  print SHRINKETAF "@SHRINKETA \n";
  print SHRINKSIGF "@SHRINKEPS \n";

  close(ETABARF);
  close(SHRINKETAF);
  close (SHRINKSIGF);
