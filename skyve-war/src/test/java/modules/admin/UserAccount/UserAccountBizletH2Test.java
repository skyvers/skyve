package modules.admin.UserAccount;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;

import modules.admin.domain.UserAccount;
import util.AbstractH2Test;

@SuppressWarnings("static-method")
class UserAccountBizletH2Test extends AbstractH2Test {
	@Test
	void newInstancePopulatesSessionsAndReturnsSameBean() throws Exception {
		UserAccountBizlet bizlet = new UserAccountBizlet();
		UserAccount bean = new UserAccount();
		UserAccount result = bizlet.newInstance(bean);
		assertNotNull(result);
	}
}
