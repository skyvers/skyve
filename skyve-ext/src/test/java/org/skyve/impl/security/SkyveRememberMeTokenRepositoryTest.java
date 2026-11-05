package org.skyve.impl.security;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.ArgumentMatchers.anyBoolean;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;
import static org.mockito.Mockito.when;

import java.util.Date;

import org.junit.jupiter.api.Test;
import org.skyve.persistence.Persistence;
import org.skyve.persistence.SQL;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.dao.IncorrectResultSizeDataAccessException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.security.web.authentication.rememberme.PersistentRememberMeToken;

@SuppressWarnings({"static-method", "boxing", "java:S8692"}) // system clock OK
class SkyveRememberMeTokenRepositoryTest {

	@Test
	void testGetGetTokenForSeriesSqlReturnsDefault() {
		SkyveRememberMeTokenRepository repo = new SkyveRememberMeTokenRepository();
		assertNotNull(repo.getGetTokenForSeriesSql());
	}

	@Test
	void testSetGetTokenForSeriesSqlRoundTrips() {
		SkyveRememberMeTokenRepository repo = new SkyveRememberMeTokenRepository();
		repo.setGetTokenForSeriesSql("select * from custom_token where series = ?");
		assertEquals("select * from custom_token where series = ?", repo.getGetTokenForSeriesSql());
	}

	@Test
	void testGetCreateNewTokenSqlReturnsDefault() {
		SkyveRememberMeTokenRepository repo = new SkyveRememberMeTokenRepository();
		assertNotNull(repo.getCreateNewTokenSql());
	}

	@Test
	void testSetCreateNewTokenSqlRoundTrips() {
		SkyveRememberMeTokenRepository repo = new SkyveRememberMeTokenRepository();
		repo.setCreateNewTokenSql("insert into custom_token values(?, ?)");
		assertEquals("insert into custom_token values(?, ?)", repo.getCreateNewTokenSql());
	}

	@Test
	void testGetUpdateTokenSqlReturnsDefault() {
		SkyveRememberMeTokenRepository repo = new SkyveRememberMeTokenRepository();
		assertNotNull(repo.getUpdateTokenSql());
	}

	@Test
	void testSetUpdateTokenSqlRoundTrips() {
		SkyveRememberMeTokenRepository repo = new SkyveRememberMeTokenRepository();
		repo.setUpdateTokenSql("update custom_token set token = ? where series = ?");
		assertEquals("update custom_token set token = ? where series = ?", repo.getUpdateTokenSql());
	}

	@Test
	void testGetRemoveUserTokensSqlReturnsDefault() {
		SkyveRememberMeTokenRepository repo = new SkyveRememberMeTokenRepository();
		assertNotNull(repo.getRemoveUserTokensSql());
	}

	@Test
	void testSetRemoveUserTokensSqlRoundTrips() {
		SkyveRememberMeTokenRepository repo = new SkyveRememberMeTokenRepository();
		repo.setRemoveUserTokensSql("delete from custom_token where user = ?");
		assertEquals("delete from custom_token where user = ?", repo.getRemoveUserTokensSql());
	}

	@Test
	void testGetTokenForSeriesWithNullJdbcTemplateThrowsOrReturnsNull() {
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
	void testRemoveUserTokensViaPersistenceCallsExecute() {
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

	@Test
	void testCreateNewTokenUsesJdbcTemplate() {
		SkyveRememberMeTokenRepository repo = new SkyveRememberMeTokenRepository();
		JdbcTemplate jdbcTemplate = mock(JdbcTemplate.class);
		when(jdbcTemplate.update(anyString(), (Object[]) any())).thenReturn(1);
		repo.setJdbcTemplate(jdbcTemplate);

		PersistentRememberMeToken token = new PersistentRememberMeToken("customer/user", "series1", "tokenval", new Date());
		repo.createNewToken(token);

		verify(jdbcTemplate).update(anyString(), (Object[]) any());
	}

	@Test
	void testCreateNewTokenWithNoSlashInUsername() {
		SkyveRememberMeTokenRepository repo = new SkyveRememberMeTokenRepository();
		JdbcTemplate jdbcTemplate = mock(JdbcTemplate.class);
		when(jdbcTemplate.update(anyString(), (Object[]) any())).thenReturn(1);
		repo.setJdbcTemplate(jdbcTemplate);

		// Username without slash — uses UNKNOWN customer
		PersistentRememberMeToken token = new PersistentRememberMeToken("justusernoSlash", "series2", "tokenval2", new Date());
		repo.createNewToken(token);

		verify(jdbcTemplate).update(anyString(), (Object[]) any());
	}

	@Test
	void testUpdateTokenUsesJdbcTemplate() {
		SkyveRememberMeTokenRepository repo = new SkyveRememberMeTokenRepository();
		JdbcTemplate jdbcTemplate = mock(JdbcTemplate.class);
		when(jdbcTemplate.update(anyString(), (Object[]) any())).thenReturn(1);
		repo.setJdbcTemplate(jdbcTemplate);

		repo.updateToken("series1", "newtoken", new Date());

		verify(jdbcTemplate).update(anyString(), (Object[]) any());
	}

	@Test
	void testRemoveUserTokensStringUsesJdbcTemplate() {
		SkyveRememberMeTokenRepository repo = new SkyveRememberMeTokenRepository();
		JdbcTemplate jdbcTemplate = mock(JdbcTemplate.class);
		when(jdbcTemplate.update(anyString(), (Object[]) any())).thenReturn(1);
		repo.setJdbcTemplate(jdbcTemplate);

		repo.removeUserTokens("username");

		verify(jdbcTemplate).update(anyString(), (Object[]) any());
	}

	@SuppressWarnings("unchecked")
	@Test
	void testGetTokenForSeriesReturnsTokenWhenFound() {
		SkyveRememberMeTokenRepository repo = new SkyveRememberMeTokenRepository();
		JdbcTemplate jdbcTemplate = mock(JdbcTemplate.class);
		PersistentRememberMeToken expected = new PersistentRememberMeToken("user", "series1", "token1", new Date());
		when(jdbcTemplate.queryForObject(anyString(), any(RowMapper.class), (Object[]) any())).thenReturn(expected);
		repo.setJdbcTemplate(jdbcTemplate);

		PersistentRememberMeToken result = repo.getTokenForSeries("series1");
		assertNotNull(result);
		assertEquals("user", result.getUsername());
	}

	@SuppressWarnings("unchecked")
	@Test
	void testGetTokenForSeriesReturnsNullOnEmptyResult() {
		SkyveRememberMeTokenRepository repo = new SkyveRememberMeTokenRepository();
		JdbcTemplate jdbcTemplate = mock(JdbcTemplate.class);
		when(jdbcTemplate.queryForObject(anyString(), any(RowMapper.class), (Object[]) any()))
			.thenThrow(new EmptyResultDataAccessException(1));
		repo.setJdbcTemplate(jdbcTemplate);

		PersistentRememberMeToken result = repo.getTokenForSeries("series1");
		assertNull(result);
	}

	@SuppressWarnings("unchecked")
	@Test
	void testGetTokenForSeriesReturnsNullOnIncorrectResultSize() {
		SkyveRememberMeTokenRepository repo = new SkyveRememberMeTokenRepository();
		JdbcTemplate jdbcTemplate = mock(JdbcTemplate.class);
		when(jdbcTemplate.queryForObject(anyString(), any(RowMapper.class), (Object[]) any()))
			.thenThrow(new IncorrectResultSizeDataAccessException(1, 2));
		repo.setJdbcTemplate(jdbcTemplate);

		PersistentRememberMeToken result = repo.getTokenForSeries("series1");
		assertNull(result);
	}
}
