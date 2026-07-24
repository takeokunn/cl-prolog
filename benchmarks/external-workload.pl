edge(0, 1).
edge(0, 2).
edge(1, 3).
edge(1, 4).
edge(2, 5).
edge(2, 6).
edge(3, 7).
edge(3, 8).
edge(4, 9).
edge(4, 10).
edge(5, 11).
edge(5, 12).
edge(6, 13).
edge(6, 14).
edge(7, 15).
edge(7, 16).
edge(8, 17).
edge(8, 18).
edge(9, 19).
edge(9, 20).
edge(10, 21).
edge(10, 22).
edge(11, 23).
edge(11, 24).
edge(12, 25).
edge(12, 26).
edge(13, 27).
edge(13, 28).
edge(14, 29).
edge(14, 30).

path(Source, Destination) :-
    edge(Source, Destination).
path(Source, Destination) :-
    edge(Source, Middle),
    path(Middle, Destination).

solution_stats([], Count, Checksum, Fingerprint, Count, Checksum, Fingerprint).
solution_stats([[binding(destination, Value)]|Values],
               Count0, Checksum0, Fingerprint0,
               Count, Checksum, Fingerprint) :-
    Count1 is Count0 + 1,
    Checksum1 is Checksum0 + Value,
    Fingerprint1 is (Fingerprint0 * 131 + Value) mod 2147483647,
    solution_stats(Values, Count1, Checksum1, Fingerprint1,
                   Count, Checksum, Fingerprint).

one_iteration(Count, Checksum, Fingerprint) :-
    findall([binding(destination, Destination)],
            path(0, Destination),
            Solutions),
    solution_stats(Solutions, 0, 0, 0, Count, Checksum, Fingerprint),
    Count =:= 30,
    Checksum =:= 465,
    Fingerprint =:= 1589920743.

run_iterations(0, Aggregate, Aggregate).
run_iterations(Iterations, Aggregate0, Aggregate) :-
    Iterations > 0,
    one_iteration(_, Checksum, _),
    Remaining is Iterations - 1,
    Aggregate1 is Aggregate0 + Checksum,
    run_iterations(Remaining, Aggregate1, Aggregate).

run_workload(Iterations, Aggregate) :-
    Iterations > 0,
    run_iterations(Iterations, 0, Aggregate),
    Expected is Iterations * 465,
    Aggregate =:= Expected.

benchmark_server :-
    run_workload(100, _),
    write('READY'),
    nl,
    flush_output,
    read(Iterations),
    run_workload(Iterations, Aggregate),
    write('RESULT '),
    write(30),
    write(' '),
    write(465),
    write(' '),
    write(1589920743),
    write(' '),
    write(Aggregate),
    nl,
    flush_output,
    halt.
