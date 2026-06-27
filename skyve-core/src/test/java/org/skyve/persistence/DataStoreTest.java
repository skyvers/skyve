package org.skyve.persistence;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;

class DataStoreTest {

	@Test
	@SuppressWarnings("static-method")
	void jndiConstructorSetsFields() {
		DataStore ds = new DataStore("java:comp/env/jdbc/skyve", "org.hibernate.dialect.H2Dialect");
		assertThat(ds.getJndiDataSourceName(), is("java:comp/env/jdbc/skyve"));
		assertThat(ds.getDialectClassName(), is("org.hibernate.dialect.H2Dialect"));
		assertEquals(0, ds.getOltpConnectionTimeoutInSeconds());
		assertEquals(0, ds.getAsyncConnectionTimeoutInSeconds());
		assertNull(ds.getJdbcDriverClassName());
		assertNull(ds.getJdbcUrl());
	}

	@Test
	@SuppressWarnings("static-method")
	void jndiConstructorWithTimeoutsSetsTimeouts() {
		DataStore ds = new DataStore("java:comp/env/jdbc/skyve", "org.hibernate.dialect.H2Dialect", 30, 60);
		assertEquals(30, ds.getOltpConnectionTimeoutInSeconds());
		assertEquals(60, ds.getAsyncConnectionTimeoutInSeconds());
	}

	@Test
	@SuppressWarnings("static-method")
	void jdbcConstructorSetsFields() {
		DataStore ds = new DataStore("org.h2.Driver", "jdbc:h2:mem:test", "org.hibernate.dialect.H2Dialect");
		assertThat(ds.getJdbcDriverClassName(), is("org.h2.Driver"));
		assertThat(ds.getJdbcUrl(), is("jdbc:h2:mem:test"));
		assertThat(ds.getDialectClassName(), is("org.hibernate.dialect.H2Dialect"));
		assertEquals(0, ds.getOltpConnectionTimeoutInSeconds());
		assertEquals(0, ds.getAsyncConnectionTimeoutInSeconds());
	}

	@Test
	@SuppressWarnings("static-method")
	void jdbcConstructorWithTimeoutsSetsTimeouts() {
		DataStore ds = new DataStore("org.h2.Driver", "jdbc:h2:mem:test", "org.hibernate.dialect.H2Dialect", 15, 45);
		assertEquals(15, ds.getOltpConnectionTimeoutInSeconds());
		assertEquals(45, ds.getAsyncConnectionTimeoutInSeconds());
	}

	@Test
	@SuppressWarnings("static-method")
	void jdbcConstructorWithCredentialsSetsCredentials() {
		DataStore ds = new DataStore("org.h2.Driver", "jdbc:h2:mem:test", "admin", "secret", "org.hibernate.dialect.H2Dialect");
		assertThat(ds.getUserName(), is("admin"));
		assertThat(ds.getPassword(), is("secret"));
	}

	@Test
	@SuppressWarnings("static-method")
	void jdbcConstructorWithCredentialsAndTimeouts() {
		DataStore ds = new DataStore("org.h2.Driver", "jdbc:h2:mem:test", "admin", "secret", "org.hibernate.dialect.H2Dialect", 20, 40);
		assertThat(ds.getUserName(), is("admin"));
		assertThat(ds.getPassword(), is("secret"));
		assertEquals(20, ds.getOltpConnectionTimeoutInSeconds());
		assertEquals(40, ds.getAsyncConnectionTimeoutInSeconds());
	}
}
