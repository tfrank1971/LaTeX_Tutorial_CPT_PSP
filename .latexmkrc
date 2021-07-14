####################################################
# configuration file for latexmk. DO NOT DELETE
####################################################

add_cus_dep('glo', 'gls', 0, 'makeglo2gls');
add_cus_dep('acn', 'acr', 0, 'makeglo2gls');
sub makeglo2gls {
        system("makeglossaries $_[0]");
}

$pdf_mode = 5;

$xdvipdfmx ='xdvipdfmx -V 4 %O %S';


@generated_exts = qw(aux idx ind lof lot out toc acn acr alg glg glo gls ist fls run.xml *bbl 'xdv');
