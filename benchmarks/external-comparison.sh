#!/bin/sh
set -eu

cd "$(dirname "$0")/.."

iterations="${ITERATIONS:-5000}"
case "$iterations" in
    *[!0-9]*|'')
        printf '%s\n' "ITERATIONS must be a positive integer." >&2
        exit 2
        ;;
    0)
        printf '%s\n' "ITERATIONS must be a positive integer." >&2
        exit 2
        ;;
esac

run_engine() {
    engine="$1"
    version="$2"
    shift 2

    perl -MIPC::Open3 -MSymbol=gensym \
        -MTime::HiRes=clock_gettime,CLOCK_MONOTONIC -e '
        use strict;
        use warnings;

        my ($engine, $version, $iterations, @command) = @ARGV;
        my $error = gensym;
        my $pid;

        local $SIG{ALRM} = sub { die "$engine exceeded the 60 second limit\n" };
        alarm 60;
        $pid = open3(my $input, my $output, $error, @command);

        my $ready = 0;
        while (my $line = <$output>) {
            if ($line =~ /\AREADY\r?\n\z/) {
                $ready = 1;
                last;
            }
            print STDERR "[$engine load] $line";
        }
        die "$engine exited before READY\n" unless $ready;

        my $start = clock_gettime(CLOCK_MONOTONIC);
        print {$input} "$iterations.\n";
        close $input;

        my $result;
        while (my $line = <$output>) {
            if ($line =~ /\ARESULT ([0-9]+) ([0-9]+) ([0-9]+) ([0-9]+)\r?\n\z/) {
                $result = [$1, $2, $3, $4];
                last;
            }
            print STDERR "[$engine timed] $line";
        }
        my $end = clock_gettime(CLOCK_MONOTONIC);

        my $stderr = do {
            local $/;
            <$error> // "";
        };
        waitpid($pid, 0);
        my $status = $?;
        alarm 0;

        print STDERR "[$engine stderr] $stderr" if length $stderr;
        die "$engine exited with status $status\n" if $status != 0;
        die "$engine exited before RESULT\n" unless defined $result;

        my ($count, $checksum, $fingerprint, $aggregate) = @$result;
        my $expected = $iterations * 465;
        die "$engine returned invalid solution count $count\n" unless $count == 30;
        die "$engine returned invalid checksum $checksum\n" unless $checksum == 465;
        die "$engine returned invalid fingerprint $fingerprint\n"
            unless $fingerprint == 1589920743;
        die "$engine returned invalid aggregate $aggregate\n"
            unless $aggregate == $expected;

        $version =~ s/[\t\r\n]+/ /g;
        printf "engine=%s\tversion=%s\titerations=%d\tmethod=parent-clock_gettime-monotonic\telapsed_ms=%.3f\tsolutions_per_iteration=%d\tchecksum_per_iteration=%d\tfingerprint_per_iteration=%d\taggregate=%d\n",
            $engine, $version, $iterations, 1000 * ($end - $start),
            $count, $checksum, $fingerprint, $aggregate;
    ' "$engine" "$version" "$iterations" "$@"
}

swi_version="$(nix shell nixpkgs#swi-prolog -c swipl --version)"
trealla_version="$(nix shell nixpkgs#trealla -c tpl --version)"
scryer_version="$(nix shell nixpkgs#scryer-prolog -c scryer-prolog --version)"
cl_prolog_version="$(sbcl --version)"

run_engine swi "$swi_version" \
    nix shell nixpkgs#swi-prolog -c \
    swipl -q -f none -s benchmarks/external-workload.pl \
    -g benchmark_server
run_engine trealla "$trealla_version" \
    nix shell nixpkgs#trealla -c \
    tpl -f -q benchmarks/external-workload.pl \
    -g benchmark_server
run_engine scryer "$scryer_version" \
    nix shell nixpkgs#scryer-prolog -c \
    scryer-prolog -f benchmarks/external-workload.pl \
    -g benchmark_server
run_engine cl-prolog "$cl_prolog_version" \
    sbcl --noinform --disable-debugger \
    --script benchmarks/external-cl-prolog.lisp
