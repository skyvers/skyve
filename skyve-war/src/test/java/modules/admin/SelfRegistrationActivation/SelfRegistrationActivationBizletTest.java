package modules.admin.SelfRegistrationActivation;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;
import org.skyve.metadata.controller.ImplicitActionName;

@SuppressWarnings("static-method")
public class SelfRegistrationActivationBizletTest {

	private static final SelfRegistrationActivationBizlet bizlet = new SelfRegistrationActivationBizlet();

	@Test
	void preExecuteEditReturnsBean() throws Exception {
		SelfRegistrationActivationExtension bean = new SelfRegistrationActivationExtension();
		SelfRegistrationActivationExtension result = bizlet.preExecute(ImplicitActionName.Edit, bean, null, null);
		assertNotNull(result);
	}

	@Test
	void preExecuteSaveReturnsBean() throws Exception {
		SelfRegistrationActivationExtension bean = new SelfRegistrationActivationExtension();
		SelfRegistrationActivationExtension result = bizlet.preExecute(ImplicitActionName.Save, bean, null, null);
		assertNotNull(result);
	}
}
