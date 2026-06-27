package org.skyve.persistence;


/**
 * Connection configuration for a Skyve data store (a relational database).
 *
 * <p>A data store is configured either via a container-managed JNDI data source or a
 * directly specified JDBC driver/URL. Exactly one of {@link #getJndiDataSourceName()} or
 * {@link #getJdbcDriverClassName()} will be non-null for a given instance.
 *
 * <h2>OLTP vs async timeouts</h2>
 * <p>Skyve distinguishes between two connection-pool timeout budgets:
 * <ul>
 *   <li><b>OLTP timeout</b> ({@link #getOltpConnectionTimeoutInSeconds()}) — applied to
 *       connections used in interactive UI and form-processing requests where a
 *       short, bounded timeout protects end-user responsiveness.
 *   <li><b>Async timeout</b> ({@link #getAsyncConnectionTimeoutInSeconds()}) — applied to
 *       connections used in background jobs, batch processes, and scheduled tasks where
 *       longer-running operations must complete without interruption.
 * </ul>
 * <p>A value of {@code 0} for either timeout means no explicit timeout is applied.
 *
 * <h2>Dialect</h2>
 * <p>The dialect class name selects the Hibernate {@code Dialect} subclass appropriate
 * for the target database engine (e.g. a MariaDB, PostgreSQL, or H2 dialect).
 */
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
		/**
	 * Constructs a JNDI data-source-backed data store with no connection timeout.
	 *
	 * @param jndiDataSourceName the JNDI name of the container-managed data source;
	 *                           must not be {@code null}
	 * @param dialectClassName   the fully qualified Hibernate Dialect class name;
	 *                           must not be {@code null}
	 */	public DataStore(String jndiDataSourceName,
						String dialectClassName) {
		this(jndiDataSourceName, dialectClassName, 0, 0);
	}
	/**
	 * Constructs a JNDI data-source-backed data store with explicit connection timeouts.
	 *
	 * @param jndiDataSourceName                the JNDI name of the container-managed data source;
	 *                                          must not be {@code null}
	 * @param dialectClassName                  the fully qualified Hibernate Dialect class name;
	 *                                          must not be {@code null}
	 * @param oltpConnectionTimeoutInSeconds    timeout for UI/forms connections in seconds;
	 *                                          {@code 0} means no timeout
	 * @param asyncConnectionTimeoutInSeconds   timeout for background-job connections in seconds;
	 *                                          {@code 0} means no timeout
	 */	public DataStore(String jndiDataSourceName,
						String dialectClassName,
						int oltpConnectionTimeoutInSeconds,
						int asyncConnectionTimeoutInSeconds) {
		this.jndiDataSourceName = jndiDataSourceName;
		this.dialectClassName = dialectClassName;
		this.oltpConnectionTimeoutInSeconds = oltpConnectionTimeoutInSeconds;
		this.asyncConnectionTimeoutInSeconds = asyncConnectionTimeoutInSeconds;
	}

	/**
	 * Constructs a JDBC-driver-backed data store with no authentication and no timeout.
	 *
	 * @param jdbcDriverClassName the fully qualified JDBC driver class name; must not be {@code null}
	 * @param jdbcUrl             the JDBC connection URL; must not be {@code null}
	 * @param dialectClassName    the fully qualified Hibernate Dialect class name; must not be {@code null}
	 */
	public DataStore(String jdbcDriverClassName, String jdbcUrl, String dialectClassName) {
		this(jdbcDriverClassName, jdbcUrl, dialectClassName, 0, 0);
	}
	/**
	 * Constructs a JDBC-driver-backed data store with no authentication but with explicit timeouts.
	 *
	 * @param jdbcDriverClassName               the fully qualified JDBC driver class name; must not be {@code null}
	 * @param jdbcUrl                           the JDBC connection URL; must not be {@code null}
	 * @param dialectClassName                  the fully qualified Hibernate Dialect class name; must not be {@code null}
	 * @param oltpConnectionTimeoutInSeconds    timeout for UI/forms connections in seconds; {@code 0} means no timeout
	 * @param asyncConnectionTimeoutInSeconds   timeout for background-job connections in seconds; {@code 0} means no timeout
	 */	public DataStore(String jdbcDriverClassName,
						String jdbcUrl,
						String dialectClassName,
						int oltpConnectionTimeoutInSeconds,
						int asyncConnectionTimeoutInSeconds) {
		this(jdbcDriverClassName, jdbcUrl, null, null, dialectClassName, oltpConnectionTimeoutInSeconds, asyncConnectionTimeoutInSeconds);
	}
	/**
	 * Constructs a JDBC-driver-backed data store with explicit credentials and no timeout.
	 *
	 * @param jdbcDriverClassName the fully qualified JDBC driver class name; must not be {@code null}
	 * @param jdbcUrl             the JDBC connection URL; must not be {@code null}
	 * @param userName            the database username; may be {@code null} for unauthenticated connections
	 * @param password            the database password; may be {@code null}
	 * @param dialectClassName    the fully qualified Hibernate Dialect class name; must not be {@code null}
	 */	public DataStore(String jdbcDriverClassName, 
						String jdbcUrl,
						String userName,
						String password,
						String dialectClassName) {
		this(jdbcDriverClassName, jdbcUrl, userName, password, dialectClassName, 0, 0);
	}
	/**
	 * Constructs a JDBC-driver-backed data store with explicit credentials and timeouts.
	 *
	 * @param jdbcDriverClassName               the fully qualified JDBC driver class name; must not be {@code null}
	 * @param jdbcUrl                           the JDBC connection URL; must not be {@code null}
	 * @param userName                          the database username; may be {@code null}
	 * @param password                          the database password; may be {@code null}
	 * @param dialectClassName                  the fully qualified Hibernate Dialect class name; must not be {@code null}
	 * @param oltpConnectionTimeoutInSeconds    timeout for UI/forms connections in seconds; {@code 0} means no timeout
	 * @param asyncConnectionTimeoutInSeconds   timeout for background-job connections in seconds; {@code 0} means no timeout
	 */	public DataStore(String jdbcDriverClassName, 
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

	/**
	 * Returns the fully qualified Hibernate Dialect class name.
	 *
	 * @return the dialect class name; never {@code null} for a properly configured store
	 */
	public String getDialectClassName() {
		return dialectClassName;
	}

	/**
	 * Returns the JNDI name of the container-managed data source, or {@code null} if this
	 * store uses a JDBC driver directly.
	 *
	 * @return the JNDI data source name, or {@code null}
	 */
	public String getJndiDataSourceName() {
		return jndiDataSourceName;
	}

	/**
	 * Returns the fully qualified JDBC driver class name, or {@code null} if this store
	 * uses a JNDI data source.
	 *
	 * @return the JDBC driver class name, or {@code null}
	 */
	public String getJdbcDriverClassName() {
		return jdbcDriverClassName;
	}

	/**
	 * Returns the JDBC connection URL, or {@code null} if this store uses a JNDI data source.
	 *
	 * @return the JDBC URL, or {@code null}
	 */
	public String getJdbcUrl() {
		return jdbcUrl;
	}

	/**
	 * Returns the database username, or {@code null} if the connection requires no
	 * authentication or a JNDI data source is used.
	 *
	 * @return the username, or {@code null}
	 */
	public String getUserName() {
		return userName;
	}

	/**
	 * Returns the database password, or {@code null}.
	 *
	 * @return the password, or {@code null}
	 */
	public String getPassword() {
		return password;
	}

	/**
	 * Returns the timeout in seconds for connections used in interactive UI/forms processing.
	 * A value of {@code 0} means no explicit timeout is applied.
	 *
	 * @return the OLTP connection timeout in seconds
	 */
	public int getOltpConnectionTimeoutInSeconds() {
		return oltpConnectionTimeoutInSeconds;
	}

	/**
	 * Returns the timeout in seconds for connections used in background jobs and batch tasks.
	 * A value of {@code 0} means no explicit timeout is applied.
	 *
	 * @return the async connection timeout in seconds
	 */
	public int getAsyncConnectionTimeoutInSeconds() {
		return asyncConnectionTimeoutInSeconds;
	}
}
