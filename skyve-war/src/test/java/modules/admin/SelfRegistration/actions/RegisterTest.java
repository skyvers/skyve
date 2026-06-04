package modules.admin.SelfRegistration.actions;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.controller.ServerSideActionResult;

import modules.admin.SelfRegistration.SelfRegistrationExtension;

/**
 * Tests simple self-registration action branches.
 */
@SuppressWarnings("static-method")
class RegisterTest {
	@Test
	void executeWithNoUserReturnsBean() throws Exception {
		SelfRegistrationExtension bean = new SelfRegistrationExtension();

		ServerSideActionResult<SelfRegistrationExtension> result = new Register().execute(bean, null);

		assertNotNull(result);
		assertSame(bean, result.getBean());
	}
}
