package org.skyve.impl.generate;

public enum DialectOptions {
	H2(true, false, 0, 1024),
	H2_NO_INDEXES(false, false, 0, 1024),
	MYSQL(true, false, 0, 1024),
	MSSQL_2014(true, true, 0, 900), // SQL Server 2014 and below limits indexes to 900
	MSSQL_2016(true, true, 0, 1024);

	private final boolean dataStoreIndexForeignKeys;
	private final boolean dataStoreIndexNamesInGlobalNamespace;
	private final int dataStoreIdentifierCharacterLimit;
	private final int dataStoreBizKeyLength;

	DialectOptions(boolean dataStoreIndexForeignKeys,
						   boolean dataStoreIndexNamesInGlobalNamespace,
						   int dataStoreIdentifierCharacterLimit,
						   int dataStoreBizKeyLength) {
		this.dataStoreIndexForeignKeys = dataStoreIndexForeignKeys;
		this.dataStoreIndexNamesInGlobalNamespace = dataStoreIndexNamesInGlobalNamespace;
		this.dataStoreIdentifierCharacterLimit = dataStoreIdentifierCharacterLimit;
		this.dataStoreBizKeyLength = dataStoreBizKeyLength;
	}

	public boolean isDataStoreIndexForeignKeys() {
		return dataStoreIndexForeignKeys;
	}

	public boolean isDataStoreIndexNamesInGlobalNamespace() {
		return dataStoreIndexNamesInGlobalNamespace;
	}

	public int getDataStoreIdentifierCharacterLimit() {
		return dataStoreIdentifierCharacterLimit;
	}

	public int getDataStoreBizKeyLength() {
		return dataStoreBizKeyLength;
	}
}
