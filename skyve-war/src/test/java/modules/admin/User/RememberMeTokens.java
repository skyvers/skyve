package modules.admin.User;

import java.util.List;
import java.util.UUID;

import org.skyve.domain.types.Timestamp;
import org.skyve.persistence.Persistence;

/**
 * Test helper: remember-me token rows in {@code ADM_UserToken}, as {@code SkyveRememberMeTokenRepository} writes them.
 */
public final class RememberMeTokens {
	private RememberMeTokens() {
		// Disallow instantiation.
	}

	/**
	 * Inserts a token row.
	 *
	 * @param principal the principal name, {@code customer/userName}
	 * @param series the token series
	 */
	public static void add(Persistence p, String customer, String principal, String series) {
		p.newSQL("insert into ADM_UserToken (bizId, bizVersion, bizLock, bizKey, bizCustomer, bizUserId, userName, series, token, lastUsed) "
					+ "values (:bizId, 0, '20260101000000000admin', :userName, :customer, 'SYSTEM', :userName, :series, 'token', :lastUsed)")
			.putParameter("bizId", UUID.randomUUID().toString(), false)
			.putParameter("userName", principal, false)
			.putParameter("customer", customer, false)
			.putParameter("series", series, false)
			.putParameter("lastUsed", new Timestamp())
			.execute();
	}

	/**
	 * Returns the series of a principal's rows, in order.
	 */
	public static List<String> series(Persistence p, String principal) {
		return p.newSQL("select series from ADM_UserToken where userName = :userName order by series")
				.putParameter("userName", principal, false)
				.scalarResults(String.class);
	}
}
