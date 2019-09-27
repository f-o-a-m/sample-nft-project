This directory contains migrations used to set up the SQL schema for the indexer.
They are ran by `flyway`, as part of the `make migrate` step.

They follow Flyway's naming schema, and try to have a logical structure as to the order of operations.

New migrations should always have an incremented version, to enable continuous delivery of your
application. If your versions don't monotonically increase, you will have to wipe your SQL
database to run the migrations again.

You can read more [Flyway's Migration Conventions](https://flywaydb.org/documentation/migrations), if interested.