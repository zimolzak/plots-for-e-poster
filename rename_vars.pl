#!/usr/bin/perl -w
use strict;
while(<>){
    s/\+PCR/pos_pcr_test/g;
    s/\-PCR/neg_pcr_test/g;
    s/\+antibodies/pos_ab_test/g;
    s/\-antibodies/neg_ab_test/g;
    print;
}
