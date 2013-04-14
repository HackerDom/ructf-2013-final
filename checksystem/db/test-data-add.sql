-- Test data for RuCTF-2013

INSERT INTO teams VALUES ( 101, 'Team 101', '172.16.16.101/32', '172.16.16.101', true );
INSERT INTO teams VALUES ( 102, 'Team 102', '172.16.16.102/32', '172.16.16.102', true );
INSERT INTO teams VALUES ( 103, 'Team 103', '172.16.16.103/32', '172.16.16.103', true );
INSERT INTO teams VALUES ( 104, 'Team 104', '172.16.16.104/32', '172.16.16.104', true );

INSERT INTO services VALUES ( 1, 'UP',     './stub/up.sh' );
INSERT INTO services VALUES ( 2, 'Down',   './stub/down.sh' );
INSERT INTO services VALUES ( 3, 'Random', './stub/random.pl' );

--INSERT INTO services VALUES ( 1, 'buster',        './Buster/buster.checker.sh'                );
--INSERT INTO services VALUES ( 2, 'booking',       './booking/booking.checker.pl'              );
--INSERT INTO services VALUES ( 3, 'flightprocess', './flightprocess/flightprocess.checker.pl'  );
--INSERT INTO services VALUES ( 4, 'flybook',       './FlyBook/FlyBook.check.py'                );
--INSERT INTO services VALUES ( 5, 'gds',           './gds/checker.py'                          );
--INSERT INTO services VALUES ( 6, 'geotracker',    './geotracker/geotracker.checker.pl'        );
--INSERT INTO services VALUES ( 7, 'mch',           './mch/mch.checker.pl'                      );
--INSERT INTO services VALUES ( 8, 'lust',          './lust/lust.checker.sh'                    );
--
