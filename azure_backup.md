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
- The scheduled job then renames it to DAILY_<name>.zip (local rename, or external move). If the external move fails, the weekly/monthly/yearly copies are skipped for that run.
- WEEKLY_yyyyMMWW / MONTHLY_yyyyMM / YEARLY_yyyy copies are made **at most once per period**.
    - If a copy for the current period already exists in the store in use (the local directory, or the external backup when enabled) it is skipped and logged.
    - So the periodic copy is the first daily of the period whose copy succeeded, not the last. A failed copy leaves nothing behind and is retried on the next run.
    - A daily with problems is copied with the _PROBLEMS suffix (e.g. WEEKLY_20260904_PROBLEMS.zip) and does not block a later good daily from producing the period's real copy.
    - WW is week-in-month, so a calendar week that spans a month boundary gets two weekly copies (pre-existing naming).
- Culling keeps the newest N per prefix, for both plain and _PROBLEMS names, including YEARLY_. Local order is by name; external order is whatever listBackups returns (Azure: last-modified, descending).
    - Retention 0 (or null) for a tier culls every backup of that tier, including yearly (yearly was never culled before Skyve 10).
- Skyve applications carry their own copy of the admin module, so the scheduled job changes only reach an app after it is re-assembled (skyve:assemble); the Azure class changes arrive with the skyve-ext dependency.
# Azure copy/move
- copyBackup uses Azure's asynchronous server-side copy (beginCopy on the source blob URL).
    - Account-key connection string: the request's Shared Key authorisation is applied to the same-account source, so nothing is added to the source URL.
    - SAS connection string: Azure does not extend the request's SAS to the copy source (CannotVerifyCopySource / NoAuthenticationInformation), so the connection string's own SAS is appended to the source URL.
        - The SAS needs resource types Service, Container and Object (Container alone fails every blob operation with AuthorizationResourceTypeMismatch) with Read, Write, Delete, List, Add and Create.
        - The SAS must not carry an IP restriction: Azure reads the copy source from its own network, so an IP-restricted SAS fails the copy with AuthorizationFailure while uploads from the allowed address still work.
        - Startup's connection string field accepts up to 500 characters; a portal-generated SAS connection string is ~450.
    - The blob never passes through the app server: no egress charge, no local bandwidth, no 256 MB limit (that limit only applies to the synchronous copyFromUrl).
    - The copy is waited on (4 hour cap). On failure or timeout the copy is aborted and any partial destination blob is deleted, so exists() never reports a bad copy.
    - moveBackup is copyBackup then deleteBackup.
- Net transfer per scheduled run is one upload (the initial zip); everything else is in-account.
# Notes
- There's a card to treat local/remote backups better - https://trello.com/c/sWuwu4ca/1290-add-backup-type-location-enum-to-the-backups-based-on-the-backup-names
