package modules.admin.Configuration;

import static org.junit.jupiter.api.Assertions.assertNotNull;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.domain.Configuration;
import util.AbstractH2Test;

/**
 * H2-backed tests for {@link ConfigurationBizlet} covering preExecute and postSave branches.
 */
class ConfigurationBizletH2Test extends AbstractH2Test {

	private static final ConfigurationBizlet bizlet = new ConfigurationBizlet();
	private ConfigurationExtension bean;

	@BeforeEach
	void setUpBean() {
		bean = new DataBuilder().fixture(FixtureType.crud).build(Configuration.MODULE_NAME, Configuration.DOCUMENT_NAME);
	}

	@Test
	void preExecuteNewInitializesStartup() throws Exception {
		ConfigurationExtension result = bizlet.preExecute(ImplicitActionName.New, bean, null, null);
		assertNotNull(result);
		assertNotNull(result.getStartup());
	}

	@Test
	void preExecuteEditDoesNotThrow() throws Exception {
		ConfigurationExtension result = bizlet.preExecute(ImplicitActionName.Edit, bean, null, null);
		assertNotNull(result);
	}

	@Test
	void postSaveWithNullTwoFactorFieldsDoesNotThrow() throws Exception {
		Assertions.assertDoesNotThrow(() -> bean.setTwoFactorType(null));
		bean.setTwofactorPushCodeTimeOutSeconds(null);
		bean.setTwoFactorEmailSubject(null);
		bean.setTwoFactorEmailBody(null);
		// Should complete without exception
		bizlet.postSave(bean);
	}

	@Test
	void postSaveWithAllTwoFactorFieldsSetDoesNotThrow() throws Exception {
		Assertions.assertDoesNotThrow(() -> bean.setBizVersion(Integer.valueOf(0)));
		bean.setTwoFactorType(modules.admin.domain.Configuration.TwoFactorType.email);
		bean.setTwofactorPushCodeTimeOutSeconds(Integer.valueOf(300));
		bean.setTwoFactorEmailSubject("Verification Code");
		bean.setTwoFactorEmailBody("Your code is: {tfaCode}");
		// Should complete without exception (new bean with version 0)
		bizlet.postSave(bean);
	}
}
