-- Test data for RuCTF-2013

INSERT INTO teams VALUES ( 101, 'Team 101', '172.16.16.101/32', 'team1.ructf', true );
INSERT INTO teams VALUES ( 102, 'Team 102', '172.16.16.102/32', 'team2.ructf', true );
INSERT INTO teams VALUES ( 103, 'Team 103', '172.16.16.103/32', 'team3.ructf', true );
INSERT INTO teams VALUES ( 104, 'Team 104', '172.16.16.104/32', 'team4.ructf', false );
INSERT INTO teams VALUES ( 105, 'Team 105', '172.16.16.105/32', 'team5.ructf', true );
INSERT INTO teams VALUES ( 106, 'Team 106', '172.16.16.106/32', 'team6.ructf', true );

INSERT INTO services VALUES ( 1, 'SES',       './ses.checker.pl' );
INSERT INTO services VALUES ( 2, 'MapReduce', './mr.checker.py'    );
INSERT INTO services VALUES ( 3, 'Queue',     './stub/notexist.sh' );
INSERT INTO services VALUES ( 4, 'DNS',       './stub/notexist.sh' );
INSERT INTO services VALUES ( 5, 'Balancer',  './stub/notexist.sh' );
INSERT INTO services VALUES ( 6, 'DB',        './database.checker.py' );
INSERT INTO services VALUES ( 7, 'Scripts',   './stub/notexist.sh' );

