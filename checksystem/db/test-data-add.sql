-- Test data for RuCTF-2013

INSERT INTO teams VALUES ( 101, 'Team 101', '172.16.16.101/32', 'team1.ructf', true );
INSERT INTO teams VALUES ( 102, 'Team 102', '172.16.16.102/32', 'team2.ructf', true );
INSERT INTO teams VALUES ( 103, 'Team 103', '172.16.16.103/32', 'team3.ructf', true );
INSERT INTO teams VALUES ( 104, 'Team 104', '172.16.16.104/32', 'team4.ructf', false );
INSERT INTO teams VALUES ( 105, 'Team 105', '172.16.16.105/32', 'team5.ructf', true );
INSERT INTO teams VALUES ( 106, 'Team 106', '172.16.16.106/32', 'team6.ructf', true );
INSERT INTO teams VALUES ( 107, 'Team 107', '172.16.16.107/32', 'team7.ructf', true );
INSERT INTO teams VALUES ( 108, 'Team 108', '172.16.16.108/32', 'team8.ructf', true );
INSERT INTO teams VALUES ( 109, 'Team 109', '172.16.16.109/32', 'team9.ructf', true );
INSERT INTO teams VALUES ( 110, 'Team 110', '172.16.16.110/32', 'team10.ructf', true );
INSERT INTO teams VALUES ( 111, 'Team 111', '172.16.16.111/32', 'team11.ructf', true );
INSERT INTO teams VALUES ( 112, 'Team 112', '172.16.16.112/32', 'team12.ructf', true );
INSERT INTO teams VALUES ( 113, 'Team 113', '172.16.16.113/32', 'team13.ructf', true );
INSERT INTO teams VALUES ( 114, 'Team 114', '172.16.16.114/32', 'team14.ructf', true );
INSERT INTO teams VALUES ( 115, 'Team 115', '172.16.16.115/32', 'team15.ructf', true );

INSERT INTO services VALUES ( 1, 'SES',       './ses.checker.pl' );
INSERT INTO services VALUES ( 2, 'MapReduce', './mr.checker.py'    );
INSERT INTO services VALUES ( 3, 'Queue',     './queue.checker.py' );
INSERT INTO services VALUES ( 4, 'DNS',       './stub/notexist.sh' );
INSERT INTO services VALUES ( 5, 'IPS',       './stub/notexist.sh' );
INSERT INTO services VALUES ( 6, 'DB',        './database.checker.py' );
INSERT INTO services VALUES ( 7, 'Scripts',   './stub/notexist.sh' );

