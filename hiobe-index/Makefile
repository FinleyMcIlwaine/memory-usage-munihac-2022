BACKUP_DB=database/hiobe_backup.db
PROD_DB=database/hiobe.db

# Install the hiobe binaries in this directory
all:
	cabal install hiobe-index --installdir . --overwrite-policy=always

# Remove binaries, profiles, and snapshots
clean:
	rm hiobe-server hiobe-debugger hiobe-traffic *.eventlog *.eventlog.html *.hp *.snapshot

# Copy backup database to hiobe.db
reset:
	cp ${BACKUP_DB} ${PROD_DB}

