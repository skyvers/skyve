package modules.admin.SecurityLog;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.controller.ImplicitActionName;

@SuppressWarnings("static-method")
class SecurityLogBizletTest {

	private static final SecurityLogBizlet bizlet = new SecurityLogBizlet();

	@Test
	void preExecuteNewReturnsBean() throws Exception {
		SecurityLogExtension bean = new SecurityLogExtension();
		SecurityLogExtension result = bizlet.preExecute(ImplicitActionName.New, bean, null, null);
		assertNotNull(result);
	}
}
