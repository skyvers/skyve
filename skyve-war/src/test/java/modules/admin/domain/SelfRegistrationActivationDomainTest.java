package modules.admin.domain;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.Test;

import modules.test.AbstractSkyveTest;

public class SelfRegistrationActivationDomainTest extends AbstractSkyveTest {

	@Test
	@SuppressWarnings("static-method")
	void newInstanceCreatesSelfRegistrationActivation() throws Exception {
		SelfRegistrationActivation bean = SelfRegistrationActivation.newInstance();
		assertNotNull(bean);
		assertEquals("admin", bean.getBizModule());
		assertEquals("SelfRegistrationActivation", bean.getBizDocument());
	}

	@Test
	@SuppressWarnings("static-method")
	void resultSuccessSetAndGet() throws Exception {
		SelfRegistrationActivation bean = SelfRegistrationActivation.newInstance();
		bean.setResult(SelfRegistrationActivation.Result.SUCCESS);
		assertEquals(SelfRegistrationActivation.Result.SUCCESS, bean.getResult());
		assertEquals("SUCCESS", bean.getResult().toCode());
	}

	@Test
	@SuppressWarnings("static-method")
	void resultAlreadyActivatedSetAndGet() throws Exception {
		SelfRegistrationActivation bean = SelfRegistrationActivation.newInstance();
		bean.setResult(SelfRegistrationActivation.Result.ALREADYACTIVATED);
		assertEquals(SelfRegistrationActivation.Result.ALREADYACTIVATED, bean.getResult());
	}

	@Test
	@SuppressWarnings("static-method")
	void resultExpiredSetAndGet() throws Exception {
		SelfRegistrationActivation bean = SelfRegistrationActivation.newInstance();
		bean.setResult(SelfRegistrationActivation.Result.EXPIRED);
		assertEquals(SelfRegistrationActivation.Result.EXPIRED, bean.getResult());
	}

	@Test
	@SuppressWarnings("static-method")
	void resultFailureSetAndGet() throws Exception {
		SelfRegistrationActivation bean = SelfRegistrationActivation.newInstance();
		bean.setResult(SelfRegistrationActivation.Result.FAILURE);
		assertEquals(SelfRegistrationActivation.Result.FAILURE, bean.getResult());
	}
}
