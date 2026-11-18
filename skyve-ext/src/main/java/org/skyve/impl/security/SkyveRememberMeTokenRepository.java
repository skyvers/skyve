package org.skyve.impl.security;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Date;

import org.skyve.domain.types.OptimisticLock;
import org.skyve.impl.util.UUIDv7;
import org.skyve.impl.util.UtilImpl;
import org.skyve.persistence.Persistence;
import org.slf4j.Logger;
import org.skyve.util.logging.SkyveLoggerFactory;
import org.springframework.dao.DataAccessException;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.dao.IncorrectResultSizeDataAccessException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.RowMapper;
import org.springframework.jdbc.core.support.JdbcDaoSupport;
import org.springframework.security.web.authentication.rememberme.PersistentRememberMeToken;
import org.springframework.security.web.authentication.rememberme.PersistentTokenRepository;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Persists Spring remember-me tokens in Skyve admin token storage tables.
 */
public class SkyveRememberMeTokenRepository extends JdbcDaoSupport implements PersistentTokenRepository {
    private static final Logger LOGGER = SkyveLoggerFactory.getLogger(SkyveRememberMeTokenRepository.class);

    private static final String JDBC_TEMPLATE_REQUIRED = "getJdbcTemplate() is null";

	/**
	 * SQL used to fetch a remember-me token row by series identifier.
	 */
	private String getTokenForSeriesSql = "select userName, series, token, lastUsed from ADM_UserToken where series = ?";

	/**
	 * SQL used to insert a new remember-me token row.
	 */
	private String createNewTokenSql = "insert into ADM_UserToken (bizId, bizVersion, bizLock, bizKey, bizCustomer, bizUserId, userName, series, token, lastUsed) values(?, ?, ?, ?, ?, ?, ?, ?, ?, ?)";
	
	/**
	 * SQL used to update an existing remember-me token value and timestamp.
	 */
	private String updateTokenSql = "update ADM_UserToken set token = ?, lastUsed = ? where series = ?";
	
	/**
	 * SQL used to remove all remember-me tokens for a user principal name.
	 */
	private String removeUserTokensSql = "delete from ADM_UserToken where userName = ?";

	/**
	 * SQL used to remove one remember-me token series of a user principal name.
	 * The user name is part of the match, so a series can only be removed by the user it was issued to.
	 */
	private String removeUserTokenSql = "delete from ADM_UserToken where userName = ? and series = ?";

	/**
	 * SQL used to remove all of a user's remember-me tokens but one series (this browser's).
	 * Takes the user name then the series to keep.
	 */
	private String removeOtherUserTokensSql = "delete from ADM_UserToken where userName = ? and series <> ?";
	
	/**
	 * Inserts a newly issued remember-me token.
	 *
	 * @param token The token to persist.
	 */
	@Override
	public void createNewToken(@SuppressWarnings("null") @Nonnull PersistentRememberMeToken token) {
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
			throw new IllegalStateException(JDBC_TEMPLATE_REQUIRED);
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

	/**
	 * Updates the token value and last-used time for an existing token series.
	 *
	 * @param series The token series identifier.
	 * @param tokenValue The new token value.
	 * @param lastUsed The timestamp of token usage.
	 */
	@Override
	public void updateToken(@SuppressWarnings("null") @Nonnull String series,
							@SuppressWarnings("null") @Nonnull String tokenValue,
							@SuppressWarnings("null") @Nonnull Date lastUsed) {
		JdbcTemplate t = getJdbcTemplate();
		if (t == null) {
			throw new IllegalStateException(JDBC_TEMPLATE_REQUIRED);
		}
		t.update(updateTokenSql, tokenValue, lastUsed, series);
	}

	/**
	 * Loads the token data for the supplied series identifier.
	 * If an error occurs, it will be reported and null will be returned (since the result
	 * should just be a failed persistent login).
	 */
	@Override
	public @Nullable PersistentRememberMeToken getTokenForSeries(@SuppressWarnings("null") @Nonnull String seriesId) {
		try {
			JdbcTemplate t = getJdbcTemplate();
			if (t == null) {
				throw new IllegalStateException(JDBC_TEMPLATE_REQUIRED);
			}
			return t.queryForObject(
						getTokenForSeriesSql,
						new RowMapper<PersistentRememberMeToken>() {
							/**
							 * Maps a result-set row to a remember-me token instance.
							 *
							 * @param rs The JDBC result set.
							 * @param rowNum The row index being mapped.
							 * @return A mapped persistent remember-me token.
							 * @throws SQLException If reading the row fails.
							 */
							@Override
							public @Nonnull PersistentRememberMeToken mapRow(@SuppressWarnings("null") @Nonnull ResultSet rs, int rowNum)
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
			LOGGER.warn("Querying token for series {} returned no result", seriesId);
		}
		catch (@SuppressWarnings("unused") IncorrectResultSizeDataAccessException moreThanOne) {
			LOGGER.warn("Querying token for series {} returned many results", seriesId);
		}
		catch (DataAccessException e) {
			LOGGER.error("Failed to find token for series {}", seriesId, e);
		}

		return null;
	}

	/**
	 * Removes all remember-me tokens for the supplied user principal name.
	 *
	 * @param username The principal name to purge token rows for.
	 */
	@Override
	public void removeUserTokens(@SuppressWarnings("null") @Nonnull String username) {
		JdbcTemplate t = getJdbcTemplate();
		if (t == null) {
			throw new IllegalStateException(JDBC_TEMPLATE_REQUIRED);
		}
		t.update(removeUserTokensSql, username);
	}

	/**
	 * Removes one remember-me token series, leaving the user's other series (other browsers and devices) in place.
	 * Nothing is removed if the series belongs to another user.
	 *
	 * @param username The principal name the series was issued to, usually in the form customer-name/user-name.
	 * @param series The series identifier, from the remember-me cookie of the browser that is signing out.
	 */
	public void removeUserToken(@Nonnull String username, @Nonnull String series) {
		JdbcTemplate t = getJdbcTemplate();
		if (t == null) {
			throw new IllegalStateException(JDBC_TEMPLATE_REQUIRED);
		}
		t.update(removeUserTokenSql, username, series);
	}

	/**
	 * Use the given Persistence to remove all the remember-me tokens for a user principal name.
	 * This occurs within the Persistence transaction.
	 *
	 * @param p The persistence to use.
	 * @param username The user principal name, usually in the form customer-name/user-name.
	 */
	public void removeUserTokens(@Nonnull Persistence p, @Nonnull String username) {
		p.newSQL(removeUserTokensSql.replace("?", ":userName"))
			.putParameter("userName", username, false)
			.execute();
	}
	
	/**
	 * Use the given Persistence to remove all of a user's remember-me tokens except one series,
	 * leaving that browser's Remember me in place ("sign out of all other sessions").
	 * This occurs within the Persistence transaction.
	 *
	 * @param p The persistence to use.
	 * @param username The user principal name, usually in the form customer-name/user-name.
	 * @param keepSeries The series to keep, from this browser's remember-me cookie; {@code null} if the browser
	 * 						has none, when all of the user's tokens are removed.
	 * @return The number of token series removed.
	 */
	public int removeOtherUserTokens(@Nonnull Persistence p, @Nonnull String username, @Nullable String keepSeries) {
		if (keepSeries == null) {
			return p.newSQL(removeUserTokensSql.replace("?", ":userName"))
						.putParameter("userName", username, false)
						.execute();
		}
		return p.newSQL(removeOtherUserTokensSql.replaceFirst("\\?", ":userName").replaceFirst("\\?", ":series"))
					.putParameter("userName", username, false)
					.putParameter("series", keepSeries, false)
					.execute();
	}

	/**
	 * Returns the SQL used to load a token by series.
	 *
	 * @return The token lookup SQL.
	 */
	public @Nonnull String getGetTokenForSeriesSql() {
		return getTokenForSeriesSql;
	}

	/**
	 * Sets the SQL used to load a token by series.
	 *
	 * @param getTokenForSeriesSql The token lookup SQL.
	 */
	public void setGetTokenForSeriesSql(@Nonnull String getTokenForSeriesSql) {
		this.getTokenForSeriesSql = getTokenForSeriesSql;
	}

	/**
	 * Returns the SQL used to insert a new token.
	 *
	 * @return The token insert SQL.
	 */
	public @Nonnull String getCreateNewTokenSql() {
		return createNewTokenSql;
	}

	/**
	 * Sets the SQL used to insert a new token.
	 *
	 * @param createNewTokenSql The token insert SQL.
	 */
	public void setCreateNewTokenSql(@Nonnull String createNewTokenSql) {
		this.createNewTokenSql = createNewTokenSql;
	}

	/**
	 * Returns the SQL used to update token values.
	 *
	 * @return The token update SQL.
	 */
	public @Nonnull String getUpdateTokenSql() {
		return updateTokenSql;
	}

	/**
	 * Sets the SQL used to update token values.
	 *
	 * @param updateTokenSql The token update SQL.
	 */
	public void setUpdateTokenSql(@Nonnull String updateTokenSql) {
		this.updateTokenSql = updateTokenSql;
	}

	/**
	 * Returns the SQL used to remove one series of a user's token rows.
	 *
	 * @return The single series removal SQL, taking the user name and then the series.
	 */
	public @Nonnull String getRemoveUserTokenSql() {
		return removeUserTokenSql;
	}

	/**
	 * Sets the SQL used to remove one series of a user's token rows.
	 *
	 * @param removeUserTokenSql The single series removal SQL, taking the user name and then the series.
	 */
	public void setRemoveUserTokenSql(@Nonnull String removeUserTokenSql) {
		this.removeUserTokenSql = removeUserTokenSql;
	}

	/**
	 * Returns the SQL used to remove all of a user's token rows but one series.
	 *
	 * @return The removal SQL, taking the user name and then the series to keep.
	 */
	public @Nonnull String getRemoveOtherUserTokensSql() {
		return removeOtherUserTokensSql;
	}

	/**
	 * Sets the SQL used to remove all of a user's token rows but one series.
	 *
	 * @param removeOtherUserTokensSql The removal SQL, taking the user name and then the series to keep.
	 */
	public void setRemoveOtherUserTokensSql(@Nonnull String removeOtherUserTokensSql) {
		this.removeOtherUserTokensSql = removeOtherUserTokensSql;
	}

	/**
	 * Returns the SQL used to remove user token rows.
	 *
	 * @return The token removal SQL.
	 */
	public @Nonnull String getRemoveUserTokensSql() {
		return removeUserTokensSql;
	}

	/**
	 * Sets the SQL used to remove user token rows.
	 *
	 * @param removeUserTokensSql The token removal SQL.
	 */
	public void setRemoveUserTokensSql(@Nonnull String removeUserTokensSql) {
		this.removeUserTokensSql = removeUserTokensSql;
	}
}
