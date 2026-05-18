#!/usr/bin/perl -w
# cidr.pl
#
use strict;
use Socket;

# cidr_to_netmask()
# given a cidr return a netmask
sub cidr_to_netmask($) {
    my $cidr = shift;
    my $bits = "1" x $cidr . "0" x (32 - $cidr);

    return join ".", (unpack 'CCCC', pack("B*", $bits));
}

# netmask_to_cidr()
# take netmask and return cidr
sub netmask_to_cidr($) {
    my $nm = shift;
    my @bytes = split /\./, $nm;
    my $cidr = 0;

    for (@bytes) {
        my $bits = unpack("B*", pack ("C", $_));
        $cidr += $bits =~ tr /1/1/;
    }

    return $cidr;
}

# network()
# given ip and netmask, return network
sub network($$) {
    my ($ip,$nm) = @_;
    my $cidr = netmask_to_cidr($nm);
    my $u32  = unpack "N", pack "CCCC", split /\./, $ip;
    my $bits = 1 x $cidr . "0" x (32 - $cidr);
    my $msk  = unpack "N", pack "B*", $bits;
    my $net  = join ".", unpack "CCCC", pack "N", $u32 & $msk;

    return $net;
}

# broadcast()
# given ip and cidr return broadcast
sub broadcast($$) {
    my ($ip,$cidr) = @_;
    my $u32 = unpack "N", pack "CCCC", split /\./, $ip;
    my $bits = 1 x $cidr . "0" x (32 - $cidr);
    my $msk = unpack "N", pack "B*", $bits;
    my $bcast = join ".", unpack "CCCC", pack "N", $u32 | ~$msk;

    return $bcast;
}
# report results
sub report($) {
    my ($ip,$cidr) = split /\//, shift;
    my $msk = cidr_to_netmask($cidr);
    my $net = network($ip,$msk);
    my $bcast = broadcast($ip,$cidr);
    my $host = gethostbyaddr(inet_aton($ip), AF_INET);

    printf "%s\n", ($host) ? $host : $ip;
    printf "    ip        %s\n", $ip;
    printf "    netmask   %s\n", $msk;
    printf "    broadcast %s\n", $bcast;
    printf "    network   %s\n", $net;
    printf "    hosts     /%d = %d\n", $cidr, (2 ** (32-$cidr)) - 2;
}

#
# int main(int argc, char *argv[])
#
my %db = ();
my $ipaddr = '\d{1,3}\.\d{1,3}\.\d{1,3}\.\d{1,3}';

for (@ARGV) { 
    if ($_ =~ m!$ipaddr/\d+!) {
        $db{$_}++;
    } 
    elsif ($_ =~ m!$ipaddr!) {
        $db{"$_/24"}++;
        $db{"$_/22"}++;
    }
    else {
        my ($n,$a,$addr,$len,@ip) = gethostbyname $_;
        my $ip_addr =  ($ip[0]) ? join '.', unpack('C4',$ip[0]) : 0 ;

        if ($ip_addr =~ m!$ipaddr!) { 
            $db{"$ip_addr/24"}++;
            $db{"$ip_addr/22"}++;
        }
    }
}

for (sort keys %db) { report($_); }
