package org.skyve.impl.security;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Date;
import java.util.UUID;

import org.skyve.domain.types.OptimisticLock;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.Util;
import org.springframework.dao.DataAccessException;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.dao.IncorrectResultSizeDataAccessException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.jdbc.core.support.JdbcDaoSupport;
import org.springframework.security.web.authentication.rememberme.PersistentRememberMeToken;
import org.springframework.security.web.authentication.rememberme.PersistentTokenRepository;

public class SkyveRememberMeTokenRepository extends JdbcDaoSupport implements PersistentTokenRepository {
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
					UUID.randomUUID().toString(),
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
			Util.LOGGER.warning("Querying token for series " + seriesId + " returned no result");
		}
		catch (@SuppressWarnings("unused") IncorrectResultSizeDataAccessException moreThanOne) {
			Util.LOGGER.warning("Querying token for series " + seriesId + " returned many results");
		}
		catch (DataAccessException e) {
			Util.LOGGER.severe("Failed to find token for series " + seriesId);
			e.printStackTrace();
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
