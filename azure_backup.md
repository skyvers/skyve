# Classes
- ExternalBackup interface
    - additional to the normal local backup
        - local, remote, both backup type
    - getInstance()
        - externalBackupClass from json
            - if not null then extra processing for external backup
    - listBackups()
    - copyBackup()
    - moveBackup()
    - downloadBackup()
    - uploadBackup()
    - AzureBlobStorageBackup implementation
        - uses AzureBlobStorage APi
        - 2 additional json properties
            - connectionString - how to connect
            - containerName - folder/namespace
# Maven Dependency
- Add azure dependency as compile scope with <optional>true</optional>
    - means the app pom needs to include azure dependency to use it.
# Setup
- Easy to setup these things, just google.
# Scheduled backup behaviour (modules.admin.DataMaintenance.BackupJob)
- The core job zips the backup and, when external backups are enabled, uploads it and deletes the local zip.
- The scheduled job then renames it to DAILY_<name>.zip (local rename, or external move).
- WEEKLY_yyyyMMWW / MONTHLY_yyyyMM / YEARLY_yyyy copies are made **at most once per period**.
    - If a copy for the current period already exists (locally or externally) it is skipped and logged.
    - So the periodic copy is the *first* good daily of the period, not the last.
    - A daily with problems is copied with the _PROBLEMS suffix (e.g. WEEKLY_202609 04_PROBLEMS.zip) and does not block a later good daily from producing the period's real copy.
- Culling is by name (descending) per prefix, for both plain and _PROBLEMS names, including YEARLY_.
# Azure copy/move
- copyBackup uses Azure's asynchronous server-side copy (beginCopy with a short-lived read SAS on the source).
    - The blob never passes through the app server: no egress charge, no local bandwidth, no 256 MB limit (that limit only applies to the synchronous copyFromUrl).
    - moveBackup is copyBackup then deleteBackup.
- Net transfer per scheduled run is one upload (the initial zip); everything else is in-account.
# Notes
- There's a card to treat local/remote backups better - https://trello.com/c/sWuwu4ca/1290-add-backup-type-location-enum-to-the-backups-based-on-the-backup-names
