package org.skyve.impl.web.spring;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.mockito.Mockito.mock;

import java.lang.reflect.Method;
import java.util.Collections;

import org.junit.jupiter.api.Test;
import org.springframework.security.provisioning.UserDetailsManager;

@SuppressWarnings("static-method")
class TwoFactorAuthPushEmailFilterTest {
	@Test
	void supportsPushConfigurationOnlyForEmailMode() throws Exception {
		TwoFactorAuthPushEmailFilter filter = new TwoFactorAuthPushEmailFilter(mock(UserDetailsManager.class));
		assertFalse(filter.supportsPushConfiguration(null));

		Object emailConfig = createTfaConfig("EMAIL");
		Object offConfig = createTfaConfig("OFF");
		assertTrue(invokeSupports(filter, emailConfig));
		assertFalse(invokeSupports(filter, offConfig));
	}

	@Test
	void pushNotificationReturnsWhenUserHasNoEmail() {
		TwoFactorAuthPushEmailFilter filter = new TwoFactorAuthPushEmailFilter(mock(UserDetailsManager.class));
		TwoFactorAuthUser user = new TwoFactorAuthUser("acme/alice",
				"pwd",
				true,
				true,
				true,
				true,
				Collections.emptyList(),
				"acme",
				"alice",
				null,
				null,
				null,
				null,
				"pwd");

		assertDoesNotThrow(() -> filter.pushNotification(user, "123456"));
	}

	private static Object createTfaConfig(String type) throws Exception {
		Class<?> configClass = Class.forName("org.skyve.impl.util.TwoFactorAuthCustomerConfiguration");
		return configClass.getConstructor(String.class, int.class, String.class, String.class)
				.newInstance(type, Integer.valueOf(60), "subject", "body");
	}

	private static boolean invokeSupports(TwoFactorAuthPushEmailFilter filter, Object config) throws Exception {
		Class<?> configClass = Class.forName("org.skyve.impl.util.TwoFactorAuthCustomerConfiguration");
		Method method = TwoFactorAuthPushEmailFilter.class.getDeclaredMethod("supportsPushConfiguration", configClass);
		method.setAccessible(true);
		return ((Boolean) method.invoke(filter, config)).booleanValue();
	}
}
