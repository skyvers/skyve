package org.skyve.persistence;


public class DataStore {
	private String dialectClassName;
	private String jndiDataSourceName;
	private String jdbcDriverClassName;
	private String jdbcUrl;
	private String userName;
	private String password;
	// Timeout for data store connections employed in general UI/forms processing 
	private int oltpConnectionTimeoutInSeconds = 0;
	// Timeout for data store connections employed when running jobs and background tasks 
	private int asyncConnectionTimeoutInSeconds = 0;
	
	public DataStore(String jndiDataSourceName,
						String dialectClassName) {
		this(jndiDataSourceName, dialectClassName, 0, 0);
	}

	public DataStore(String jndiDataSourceName,
						String dialectClassName,
						int oltpConnectionTimeoutInSeconds,
						int asyncConnectionTimeoutInSeconds) {
		this.jndiDataSourceName = jndiDataSourceName;
		this.dialectClassName = dialectClassName;
		this.oltpConnectionTimeoutInSeconds = oltpConnectionTimeoutInSeconds;
		this.asyncConnectionTimeoutInSeconds = asyncConnectionTimeoutInSeconds;
	}

	public DataStore(String jdbcDriverClassName, String jdbcUrl, String dialectClassName) {
		this(jdbcDriverClassName, jdbcUrl, dialectClassName, 0, 0);
	}

	public DataStore(String jdbcDriverClassName,
						String jdbcUrl,
						String dialectClassName,
						int oltpConnectionTimeoutInSeconds,
						int asyncConnectionTimeoutInSeconds) {
		this(jdbcDriverClassName, jdbcUrl, null, null, dialectClassName, oltpConnectionTimeoutInSeconds, asyncConnectionTimeoutInSeconds);
	}

	public DataStore(String jdbcDriverClassName, 
						String jdbcUrl,
						String userName,
						String password,
						String dialectClassName) {
		this(jdbcDriverClassName, jdbcUrl, userName, password, dialectClassName, 0, 0);
	}

	public DataStore(String jdbcDriverClassName, 
						String jdbcUrl,
						String userName,
						String password,
						String dialectClassName,
						int oltpConnectionTimeoutInSeconds,
						int asyncConnectionTimeoutInSeconds) {
		this.jdbcDriverClassName = jdbcDriverClassName;
		this.jdbcUrl = jdbcUrl;
		this.userName = userName;
		this.password = password;
		this.dialectClassName = dialectClassName;
		this.oltpConnectionTimeoutInSeconds = oltpConnectionTimeoutInSeconds;
		this.asyncConnectionTimeoutInSeconds = asyncConnectionTimeoutInSeconds;
	}

	public String getDialectClassName() {
		return dialectClassName;
	}

	public String getJndiDataSourceName() {
		return jndiDataSourceName;
	}

	public String getJdbcDriverClassName() {
		return jdbcDriverClassName;
	}

	public String getJdbcUrl() {
		return jdbcUrl;
	}

	public String getUserName() {
		return userName;
	}

	public String getPassword() {
		return password;
	}

	public int getOltpConnectionTimeoutInSeconds() {
		return oltpConnectionTimeoutInSeconds;
	}

	public int getAsyncConnectionTimeoutInSeconds() {
		return asyncConnectionTimeoutInSeconds;
	}
}
