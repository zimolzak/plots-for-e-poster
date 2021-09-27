#!/usr/bin/perl -w
use strict;
while(<>){
    s/\+PCR/pos_pcr/g;
    s/\-PCR/neg_pcr/g;
    s/\+antibodies/pos_ab/g;
    s/\-antibodies/neg_ab/g;
    print;
}
