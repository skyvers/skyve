package modules.admin.UserLoginRecord;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertSame;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.controller.ImplicitActionName;

import util.AbstractH2Test;

@SuppressWarnings("static-method")
class UserLoginRecordBizletH2Test extends AbstractH2Test {
	private static final UserLoginRecordBizlet BIZLET = new UserLoginRecordBizlet();

	@Test
	void preExecuteEditWithNoIpLeavesLocationFieldsNull() throws Exception {
		UserLoginRecordExtension bean = new UserLoginRecordExtension();

		UserLoginRecordExtension result = BIZLET.preExecute(ImplicitActionName.Edit, bean, null, null);

		assertSame(bean, result);
		assertNull(bean.getCity());
		assertNull(bean.getRegion());
	}

	@Test
	void preSaveSkipsNewLoginChecksWhenBeanIsAlreadyPersisted() {
		UserLoginRecordExtension bean = new UserLoginRecordExtension();
		bean.setBizId("existing-login-record");

		assertDoesNotThrow(() -> BIZLET.preSave(bean));
	}
}
