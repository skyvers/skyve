package modules.admin.Configuration;

import static org.hamcrest.CoreMatchers.*;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.Before;
import org.junit.Test;
import org.skyve.impl.util.UtilImpl;

import modules.admin.ModulesUtil;
import modules.admin.domain.Configuration;
import util.AbstractH2TestForJUnit4;

public class ConfigurationBizletTest extends AbstractH2TestForJUnit4 {

	private ConfigurationBizlet bizlet;
	private ConfigurationExtension configuration;

	@Before
	public void setup() throws Exception {
		UtilImpl.SMTP_SENDER = "test@test.com";
		bizlet = new ConfigurationBizlet();
		configuration = Configuration.newInstance();
	}

	@Test
	public void testNewInstance() throws Exception {
		// validate the test data
		assertThat(ModulesUtil.currentAdminUserProxy(), is(notNullValue()));

		// call the method under test
		ConfigurationExtension result = bizlet.newInstance(configuration);

		// verify the result
		assertThat(result, is(notNullValue()));
		assertThat(result.getEmailFrom(), is(notNullValue()));
		assertThat(result.getStartup(), is(notNullValue()));
	}

}
