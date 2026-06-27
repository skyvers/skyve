package modules.admin.SelfRegistration.actions;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.controller.ServerSideActionResult;

import modules.admin.SelfRegistration.SelfRegistrationExtension;

@SuppressWarnings("static-method")
class SelfRegistrationResendActivationTest {

	@Test
	void executeWithNullBeanReturnsNullBean() throws Exception {
		ResendActivation action = new ResendActivation();
		// bean == null → skips the if block, returns ServerSideActionResult with null bean
		ServerSideActionResult<SelfRegistrationExtension> result = action.execute(null, null);
		assertNotNull(result);
		assertNull(result.getBean());
	}
}
