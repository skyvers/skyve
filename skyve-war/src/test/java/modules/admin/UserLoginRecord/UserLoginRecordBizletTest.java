package modules.admin.UserLoginRecord;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.controller.ImplicitActionName;

@SuppressWarnings("static-method")
class UserLoginRecordBizletTest {

	private static final UserLoginRecordBizlet bizlet = new UserLoginRecordBizlet();

	@Test
	void preExecuteNewReturnsBean() throws Exception {
		UserLoginRecordExtension bean = new UserLoginRecordExtension();
		@SuppressWarnings("null") // passes null on purpose
		UserLoginRecordExtension result = bizlet.preExecute(ImplicitActionName.New, bean, null, null);
		assertNotNull(result);
	}
}
