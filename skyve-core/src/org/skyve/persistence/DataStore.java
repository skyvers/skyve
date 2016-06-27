package org.skyve.persistence;


public class DataStore {
	private String dialectClassName;
	private String jndiDataSourceName;
	private String jdbcDriverClassName;
	private String jdbcUrl;
	private String userName;
	private String password;
	
	public DataStore(String jndiDataSourceName, String dialectClassName) {
		this.jndiDataSourceName = jndiDataSourceName;
		this.dialectClassName = dialectClassName;
	}

	public DataStore(String jdbcDriverClassName, String jdbcUrl, String dialectClassName) {
		this(jdbcDriverClassName, jdbcUrl, null, null, dialectClassName);
	}
	
	public DataStore(String jdbcDriverClassName, 
						String jdbcUrl,
						String userName,
						String password,
						String dialectClassName) {
		this.jdbcDriverClassName = jdbcDriverClassName;
		this.jdbcUrl = jdbcUrl;
		this.userName = userName;
		this.password = password;
		this.dialectClassName = dialectClassName;
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
}
