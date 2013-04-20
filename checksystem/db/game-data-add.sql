-- Test data for RuCTF-2013

INSERT INTO teams VALUES ( 101, '[censored]',       '10.23.1.0/24',  'team1.ructf',  true );
INSERT INTO teams VALUES ( 102, 'Koibasta',         '10.23.2.0/24',  'team2.ructf',  true );
INSERT INTO teams VALUES ( 103, 'rm -rf',           '10.23.3.0/24',  'team3.ructf',  true );
INSERT INTO teams VALUES ( 104, 'xPuzzle',          '10.23.4.0/24',  'team4.ructf',  true );
INSERT INTO teams VALUES ( 105, 'LeetMore',         '10.23.5.0/24',  'team5.ructf',  true );
INSERT INTO teams VALUES ( 106, 'ufologists',       '10.23.6.0/24',  'team6.ructf',  true );
INSERT INTO teams VALUES ( 107, 'ReallyNonamesFor', '10.23.7.0/24',  'team7.ructf',  true );
INSERT INTO teams VALUES ( 108, 'RDot.Org',         '10.23.8.0/24',  'team8.ructf',  true );
INSERT INTO teams VALUES ( 109, 'h34dump',	    '10.23.9.0/24',  'team9.ructf',  true );
INSERT INTO teams VALUES ( 110, 'PeterPen',	    '10.23.10.0/24', 'team10.ructf', true );
INSERT INTO teams VALUES ( 111, 'SiBears',	    '10.23.11.0/24', 'team11.ructf', true );
INSERT INTO teams VALUES ( 112, 'Honeypot',	    '10.23.12.0/24', 'team12.ructf', true );
INSERT INTO teams VALUES ( 113, 'WildRide',	    '10.23.13.0/24', 'team13.ructf', true );
INSERT INTO teams VALUES ( 114, 'Majonnäs!',	    '10.23.14.0/24', 'team14.ructf', true );
INSERT INTO teams VALUES ( 115, 'Guest team',	    '10.23.15.0/24', 'team15.ructf', true );

INSERT INTO services VALUES ( 1, 'SES',       './ses.checker.pl' );
INSERT INTO services VALUES ( 2, 'MapReduce', './mr.checker.py'    );
INSERT INTO services VALUES ( 3, 'Queue',     './queue.checker.py' );
INSERT INTO services VALUES ( 4, 'DNS',       './stub/notexist.sh' );
INSERT INTO services VALUES ( 5, 'IPS',       './stub/notexist.sh' );
INSERT INTO services VALUES ( 6, 'DB',        './database.checker.py' );
INSERT INTO services VALUES ( 7, 'Scripts',   './stub/notexist.sh' );

