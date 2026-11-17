package org.skyve.impl.web.spring;

import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.verify;

import java.sql.PreparedStatement;
import java.sql.Timestamp;
import java.util.List;

import org.junit.jupiter.api.Test;
import org.mockito.ArgumentCaptor;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.PreparedStatementSetter;
import org.springframework.security.provisioning.JdbcUserDetailsManager;

import util.AbstractH2Test;

/**
 * Tests the JDBC user details manager's two-factor update, which needs the configured data store's dialect.
 */
@SuppressWarnings({"static-method", "resource"}) // the prepared statements are mocks
class SkyveSpringSecurityH2Test extends AbstractH2Test {
	@Test
	void updateUserWritesTheTwoFactorCodeGeneratedTimestamp() throws Exception {
		long generated = 1_700_000_000_000L;

		PreparedStatement ps = updateUser(new org.skyve.domain.types.Timestamp(generated));

		verify(ps).setString(1, "123456");
		verify(ps).setString(2, "token");
		verify(ps).setTimestamp(3, new Timestamp(generated));
	}

	@Test
	void updateUserWritesNullWhenNoTwoFactorCodeWasGenerated() throws Exception {
		PreparedStatement ps = updateUser(null);

		verify(ps).setTimestamp(3, null);
	}

	/**
	 * Runs the JDBC user details manager's updateUser for a 2FA user and applies its statement setter.
	 *
	 * @return the prepared statement the update's parameters were set on
	 */
	private static PreparedStatement updateUser(org.skyve.domain.types.Timestamp tfaCodeGenerated) throws Exception {
		JdbcUserDetailsManager manager = (JdbcUserDetailsManager) new SkyveSpringSecurity().jdbcUserDetailsManager();
		JdbcTemplate template = mock(JdbcTemplate.class);
		manager.setJdbcTemplate(template);

		manager.updateUser(new TwoFactorAuthUser("acme/mike",
													"password",
													true,
													true,
													true,
													true,
													List.of(),
													"acme",
													"mike",
													"123456",
													"token",
													tfaCodeGenerated,
													"mike@acme.com",
													"password"));

		ArgumentCaptor<PreparedStatementSetter> setter = ArgumentCaptor.forClass(PreparedStatementSetter.class);
		verify(template).update(anyString(), setter.capture());
		PreparedStatement ps = mock(PreparedStatement.class);
		setter.getValue().setValues(ps);
		return ps;
	}
}
