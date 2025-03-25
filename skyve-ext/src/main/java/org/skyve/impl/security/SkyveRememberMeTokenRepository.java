package org.skyve.impl.security;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Date;

import org.skyve.domain.types.OptimisticLock;
import org.skyve.impl.util.UUIDv7;
import org.skyve.impl.util.UtilImpl;
import org.skyve.persistence.Persistence;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.dao.DataAccessException;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.dao.IncorrectResultSizeDataAccessException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.jdbc.core.support.JdbcDaoSupport;
import org.springframework.security.web.authentication.rememberme.PersistentRememberMeToken;
import org.springframework.security.web.authentication.rememberme.PersistentTokenRepository;

import jakarta.annotation.Nonnull;

public class SkyveRememberMeTokenRepository extends JdbcDaoSupport implements PersistentTokenRepository {

    private static final Logger LOGGER = LoggerFactory.getLogger(SkyveRememberMeTokenRepository.class);

	private String getTokenForSeriesSql = "select userName, series, token, lastUsed from ADM_UserToken where series = ?";
	private String createNewTokenSql = "insert into ADM_UserToken (bizId, bizVersion, bizLock, bizKey, bizCustomer, bizUserId, userName, series, token, lastUsed) values(?, ?, ?, ?, ?, ?, ?, ?, ?, ?)";
	private String updateTokenSql = "update ADM_UserToken set token = ?, lastUsed = ? where series = ?";
	private String removeUserTokensSql = "delete from ADM_UserToken where userName = ?";
	
	@Override
	public void createNewToken(PersistentRememberMeToken token) {
		String series = token.getSeries();
		String customerAndUserName = token.getUsername();
		String userName = customerAndUserName;
		String customerName = UtilImpl.CUSTOMER;
		Date lastUsed = token.getDate();
		if (customerName == null) {
			customerName = "UNKNOWN";
		}
		int slashIndex = customerAndUserName.indexOf('/');
		if (slashIndex > 0) {
			customerName = customerAndUserName.substring(0, slashIndex);
			userName = customerAndUserName.substring(slashIndex);
		}
		JdbcTemplate t = getJdbcTemplate();
		if (t == null) {
			throw new IllegalStateException("getJdbcTemplate() is null");
		}
		t.update(createNewTokenSql,
					UUIDv7.create().toString(),
					Integer.valueOf(0),
					new OptimisticLock(userName, new Date()).toString(),
					customerAndUserName,
					customerName,
					"SYSTEM",
					customerAndUserName,
					series,
					token.getTokenValue(),
					lastUsed);
	}

	@Override
	public void updateToken(String series, String tokenValue, Date lastUsed) {
		JdbcTemplate t = getJdbcTemplate();
		if (t == null) {
			throw new IllegalStateException("getJdbcTemplate() is null");
		}
		t.update(updateTokenSql, tokenValue, lastUsed, series);
	}

	/**
	 * Loads the token data for the supplied series identifier.
	 * If an error occurs, it will be reported and null will be returned (since the result
	 * should just be a failed persistent login).
	 */
	@Override
	public PersistentRememberMeToken getTokenForSeries(String seriesId) {
		try {
			JdbcTemplate t = getJdbcTemplate();
			if (t == null) {
				throw new IllegalStateException("getJdbcTemplate() is null");
			}
			return t.queryForObject(
						getTokenForSeriesSql,
						new RowMapper<PersistentRememberMeToken>() {
							@Override
							public PersistentRememberMeToken mapRow(ResultSet rs, int rowNum)
							throws SQLException {
								return new PersistentRememberMeToken(rs.getString(1),
																		rs.getString(2),
																		rs.getString(3),
																		rs.getTimestamp(4));
							}
						},
						seriesId);
		}
		catch (@SuppressWarnings("unused") EmptyResultDataAccessException zeroResults) {
			LOGGER.warn("Querying token for series " + seriesId + " returned no result");
		}
		catch (@SuppressWarnings("unused") IncorrectResultSizeDataAccessException moreThanOne) {
			LOGGER.warn("Querying token for series " + seriesId + " returned many results");
		}
		catch (DataAccessException e) {
			LOGGER.error("Failed to find token for series {}", seriesId, e);
		}

		return null;
	}

	@Override
	public void removeUserTokens(String username) {
		JdbcTemplate t = getJdbcTemplate();
		if (t == null) {
			throw new IllegalStateException("getJdbcTemplate() is null");
		}
		t.update(removeUserTokensSql, username);
	}

	/**
	 * Use the given Persistence to remove all the remember-me tokens for a user principal name.
	 * This occurs within the Persistence transaction.
	 * @param p	The persistence to use.
	 * @param username	The user principal name - usually in the form <customer-name>/<user-name> 
	 */
	public void removeUserTokens(@Nonnull Persistence p, @Nonnull String username) {
		p.newSQL(removeUserTokensSql.replace("?", ":userName"))
			.putParameter("userName", username, false)
			.execute();
	}
	
	public String getGetTokenForSeriesSql() {
		return getTokenForSeriesSql;
	}

	public void setGetTokenForSeriesSql(String getTokenForSeriesSql) {
		this.getTokenForSeriesSql = getTokenForSeriesSql;
	}

	public String getCreateNewTokenSql() {
		return createNewTokenSql;
	}

	public void setCreateNewTokenSql(String createNewTokenSql) {
		this.createNewTokenSql = createNewTokenSql;
	}

	public String getUpdateTokenSql() {
		return updateTokenSql;
	}

	public void setUpdateTokenSql(String updateTokenSql) {
		this.updateTokenSql = updateTokenSql;
	}

	public String getRemoveUserTokensSql() {
		return removeUserTokensSql;
	}

	public void setRemoveUserTokensSql(String removeUserTokensSql) {
		this.removeUserTokensSql = removeUserTokensSql;
	}
}
