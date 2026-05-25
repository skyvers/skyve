package org.skyve.impl.security;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import org.junit.Test;
import org.skyve.persistence.Persistence;
import org.skyve.persistence.SQL;

@SuppressWarnings("static-method")
public class SkyveRememberMeTokenRepositoryTest {

	@Test
	public void testGetGetTokenForSeriesSqlReturnsDefault() {
		SkyveRememberMeTokenRepository repo = new SkyveRememberMeTokenRepository();
		assertNotNull(repo.getGetTokenForSeriesSql());
	}

	@Test
	public void testSetGetTokenForSeriesSqlRoundTrips() {
		SkyveRememberMeTokenRepository repo = new SkyveRememberMeTokenRepository();
		repo.setGetTokenForSeriesSql("select * from custom_token where series = ?");
		assertEquals("select * from custom_token where series = ?", repo.getGetTokenForSeriesSql());
	}

	@Test
	public void testGetCreateNewTokenSqlReturnsDefault() {
		SkyveRememberMeTokenRepository repo = new SkyveRememberMeTokenRepository();
		assertNotNull(repo.getCreateNewTokenSql());
	}

	@Test
	public void testSetCreateNewTokenSqlRoundTrips() {
		SkyveRememberMeTokenRepository repo = new SkyveRememberMeTokenRepository();
		repo.setCreateNewTokenSql("insert into custom_token values(?, ?)");
		assertEquals("insert into custom_token values(?, ?)", repo.getCreateNewTokenSql());
	}

	@Test
	public void testGetUpdateTokenSqlReturnsDefault() {
		SkyveRememberMeTokenRepository repo = new SkyveRememberMeTokenRepository();
		assertNotNull(repo.getUpdateTokenSql());
	}

	@Test
	public void testSetUpdateTokenSqlRoundTrips() {
		SkyveRememberMeTokenRepository repo = new SkyveRememberMeTokenRepository();
		repo.setUpdateTokenSql("update custom_token set token = ? where series = ?");
		assertEquals("update custom_token set token = ? where series = ?", repo.getUpdateTokenSql());
	}

	@Test
	public void testGetRemoveUserTokensSqlReturnsDefault() {
		SkyveRememberMeTokenRepository repo = new SkyveRememberMeTokenRepository();
		assertNotNull(repo.getRemoveUserTokensSql());
	}

	@Test
	public void testSetRemoveUserTokensSqlRoundTrips() {
		SkyveRememberMeTokenRepository repo = new SkyveRememberMeTokenRepository();
		repo.setRemoveUserTokensSql("delete from custom_token where user = ?");
		assertEquals("delete from custom_token where user = ?", repo.getRemoveUserTokensSql());
	}

	@Test
	public void testGetTokenForSeriesWithNullJdbcTemplateThrowsOrReturnsNull() {
		// With no DataSource injected, getJdbcTemplate() returns null.
		// The implementation throws IllegalStateException for null JdbcTemplate.
		SkyveRememberMeTokenRepository repo = new SkyveRememberMeTokenRepository();
		try {
			Object result = repo.getTokenForSeries("nonexistent-series");
			assertNull(result);
		}
		catch (@SuppressWarnings("unused") IllegalStateException e) {
			// expected when JdbcTemplate is null
		}
	}

	@Test
	public void testRemoveUserTokensViaPersistenceCallsExecute() {
		SkyveRememberMeTokenRepository repo = new SkyveRememberMeTokenRepository();
		Persistence p = mock(Persistence.class);
		SQL sql = mock(SQL.class);
		when(p.newSQL(anyString())).thenReturn(sql);
		when(sql.putParameter(anyString(), (String) any(), anyBoolean())).thenReturn(sql);
		when(sql.execute()).thenReturn(1);

		repo.removeUserTokens(p, "testuser");

		verify(p).newSQL(anyString());
		verify(sql).execute();
	}
}
