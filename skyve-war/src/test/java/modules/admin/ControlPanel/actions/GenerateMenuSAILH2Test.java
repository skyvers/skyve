package modules.admin.ControlPanel.actions;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;

import java.util.List;

import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.metadata.sail.language.Automation;
import org.skyve.metadata.sail.language.Automation.TestStrategy;
import org.skyve.metadata.user.User;
import org.skyve.web.UserAgentType;

import jakarta.inject.Inject;
import util.AbstractH2Test;

/**
 * Tests for the GenerateMenuSAIL action delegation methods.
 */
public class GenerateMenuSAILH2Test extends AbstractH2Test {

	@Inject
	private GenerateMenuSAIL action;

	@Test
	public void testSingleGeneratesAutomation() throws Exception {
		User currentUser = CORE.getPersistence().getUser();
		String moduleName = currentUser.getCustomer().getModules().get(0).getName();

		Automation result = action.single(currentUser,
											null,
											null,
											moduleName,
											"sc",
											UserAgentType.desktop,
											TestStrategy.Assert);

		assertThat(result, is(notNullValue()));
		assertThat(result.getUxui(), is("sc"));
		assertThat(result.getUserAgentType(), is(UserAgentType.desktop));
		assertThat(result.getTestStrategy(), is(TestStrategy.Assert));
		assertThat(result.getInteractions(), is(notNullValue()));
	}

	@Test
	public void testMultipleGeneratesAutomationList() throws Exception {
		User currentUser = CORE.getPersistence().getUser();

		List<Automation> result = action.multiple(currentUser,
													null,
													null,
													"sc",
													UserAgentType.desktop,
													TestStrategy.Assert);

		assertThat(result, is(notNullValue()));
	}
}
