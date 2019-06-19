package org.skyve.impl.generate;

public enum DialectOptions {
	// @formatter:off
	H2("H2", true, false, 0, 1024), 
	H2_NO_INDEXES("H2 without indexes", false, false, 0, 1024), 
	MYSQL_5("MySQL 5", true, false, 0,1024), 
	MSSQL_2014("SQL Server up to 2014", true, true, 0, 900), // SQL Server 2014 and below limits indexes to 900
	MSSQL_2016("SQL Server 2016+", true, true, 0, 1024),
	POSTGRESQL("PostgreSQL", true, true, 63, 1024);
	// @formatter:on

	private final String description;
	private final boolean dataStoreIndexForeignKeys;
	private final boolean dataStoreIndexNamesInGlobalNamespace;
	private final int dataStoreIdentifierCharacterLimit;
	private final int dataStoreBizKeyLength;

	DialectOptions(final String description,
			final boolean dataStoreIndexForeignKeys,
			final boolean dataStoreIndexNamesInGlobalNamespace,
			final int dataStoreIdentifierCharacterLimit,
			final int dataStoreBizKeyLength) {
		this.description = description;
		this.dataStoreIndexForeignKeys = dataStoreIndexForeignKeys;
		this.dataStoreIndexNamesInGlobalNamespace = dataStoreIndexNamesInGlobalNamespace;
		this.dataStoreIdentifierCharacterLimit = dataStoreIdentifierCharacterLimit;
		this.dataStoreBizKeyLength = dataStoreBizKeyLength;
	}

	public int getDataStoreBizKeyLength() {
		return dataStoreBizKeyLength;
	}

	public int getDataStoreIdentifierCharacterLimit() {
		return dataStoreIdentifierCharacterLimit;
	}

	public String getDescription() {
		return description;
	}

	public boolean isDataStoreIndexForeignKeys() {
		return dataStoreIndexForeignKeys;
	}

	public boolean isDataStoreIndexNamesInGlobalNamespace() {
		return dataStoreIndexNamesInGlobalNamespace;
	}
}
