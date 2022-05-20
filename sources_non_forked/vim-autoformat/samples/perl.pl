# Examples from perltidy webpage.....

#Some code with side comments
my $lines     = 0;    # checksum: #lines
my $bytes     = 0;    # checksum: #bytes
my $sum       = 0;    # checksum: system V sum
my $patchdata = 0;    # saw patch data
my $pos       = 0;    # start of patch data
my $endkit    = 0;    # saw end of kit
my $fail      = 0;    # failed

#A loop
$_ = <<'EOL';
   $url = new URI::URL "http://www/";   die if $url eq "xXx";
EOL
   LOOP:{print(" digits"),redo LOOP if/\G\d+\b[,.;]?\s*/gc;print(" lowercase"),
   redo LOOP if/\G[a-z]+\b[,.;]?\s*/gc;print(" UPPERCASE"),redo LOOP 
   if/\G[A-Z]+\b[,.;]?\s*/gc;print(" Capitalized"),
   redo LOOP if/\G[A-Z][a-z]+\b[,.;]?\s*/gc;
   print(" MiXeD"),redo LOOP if/\G[A-Za-z]+\b[,.;]?\s*/gc;print(
   " alphanumeric"),redo LOOP if/\G[A-Za-z0-9]+\b[,.;]?\s*/gc;print(" line-noise"
   ),redo LOOP if/\G[^A-Za-z0-9]+/gc;print". That's all!\n";}

#A hash definition list
%unitscale=("in",72,"pt",72.27/72,"pc",12,"mm",72/25.4,"cm",72/2.54,
"\\hsize",100,"\\vsize",100,"\\textwidth",100,"\\textheight",100,
"\\pagewidth",100,"\\linewidth",100);

#A matrix
my $a_box = [ [ $a11, $a12, $a13, $a14, $a15, $a16 ], 
[ $a21, $a22, $a23, $a24, $a25, $a26 ], [ $a31, $a32, $a33, $a34, $a35, $a36 ],
[ $a41, $a42, $a43, $a44, $a45, $a46 ], [ $a51, $a52, $a53, $a54, $a55, $a56 ],
    [ $a61, $a62, $a63, $a64, $a65, $a66 ], ];

#A complex data structure
%TV=(flintstones=>{series=>"flintstones",nights=>[qw(monday thursday friday)],
members=>[{name=>"fred",role=>"lead",age=>36,},{name=>"wilma",role=>"wife",
age=>31,},{name=>"pebbles",role=>"kid",age=>4,},],},jetsons=>{series=>"jetsons",
nights=>[qw(wednesday saturday)],members=>[{name=>"george",role=>"lead",age=>41,
},{name=>"jane",role=>"wife",age=>39,},{name=>"elroy",role=>"kid",age=>9,},],},
simpsons=>{series=>"simpsons",nights=>[qw(monday)],members=>[{name=>"homer",
role=>"lead",age=>34,},{name=>"marge",role=>"wife",age=>37,},{name=>"bart",
role=>"kid",age=>11,},],},);

#Cleaning up code from a code generator
#A good application of perltidy is to clean up code which has been produced by a code generator. Here is a snippet advent.t which was generated from Fortran source by a translation tool.
{
  L9140:
 if ($msccom::obj==$msccom::food) {
 goto L8142;
 }
 if ($msccom::obj==$msccom::bird||$msccom::obj==$msccom::snake||$msccom::obj==$msccom::clam||$msccom::obj==$msccom::oyster||$msccom::obj==$msccom::dwarf||$msccom::obj==$msccom::dragon||$msccom::obj==$msccom::troll||$msccom::obj==$msccom::bear) {
 $msccom::spk=71;
 }
 goto L2011;
 # 
  #  DRINK.  IF NO OBJECT, ASSUME WATER AND LOOK FOR IT HERE.  IF WATER IS 
  #  THE BOTTLE, DRINK THAT, ELSE MUST BE AT A WATER LOC, SO DRINK STREAM. 
  # 
  L9150:
 if ($msccom::obj==0&&$liqloc->($placom::loc)!=$msccom::water&&($liq->(0)!=$msccom::water||!$here->($msccom::bottle))) {
 goto L8000;
 }
 if ($msccom::obj!=0&&$msccom::obj!=$msccom::water) {
 $msccom::spk=110;
 }
 if ($msccom::spk==110||$liq->(0)!=$msccom::water||!$here->($msccom::bottle)) {
 goto L2011;
 }
 $placom::prop->($msccom::bottle)=1;
 $placom::place->($msccom::water)=0;
 $msccom::spk=74;
 goto L2011;
 # 
  #  RUB.  YIELDS VARIOUS SNIDE REMARKS. 
  # 
  L9160:
 if ($msccom::obj!=$placom::lamp) {
 $msccom::spk=76;
 }
 goto L2011;
 # 
  #  THROW.  SAME AS DISCARD UNLESS AXE.  THEN SAME AS ATTACK EXCEPT IGNOR 
  #  AND IF DWARF IS PRESENT THEN ONE MIGHT BE KILLED.  (ONLY WAY TO DO SO 
  #  AXE ALSO SPECIAL FOR DRAGON, BEAR, AND TROLL.  TREASURES SPECIAL FOR 
  # 
  L9170:
 if ($toting->($msccom::rod2)&&$msccom::obj==$msccom::rod&&!$toting->($msccom::rod)) {
 $msccom::obj=$msccom::rod2;
 }
 }

