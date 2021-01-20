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
# Notes
- Ben claims the backups aren't culled externally
- There's a card to treat local/remote backups better - https://trello.com/c/sWuwu4ca/1290-add-backup-type-location-enum-to-the-backups-based-on-the-backup-names
