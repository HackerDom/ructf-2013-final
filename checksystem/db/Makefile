help:
	echo Targets: create-test drop-test create-game drop-game

create-game:
	psql -af game-databases-create.sql postgres
	psql -f tables-create.sql ructf
	./change-owner.sh ructf ructf
	psql -f game-data-add.sql ructf

drop-game:
	psql -af game-databases-drop.sql postgres

create-test:
	psql -af test-databases-create.sql postgres
	psql -f tables-create.sql ructfetest
	./change-owner.sh ructfetest testuser
	psql -f test-data-add.sql ructfetest

drop-test:
	psql -af test-databases-drop.sql postgres

