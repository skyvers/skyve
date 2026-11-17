package modules.admin.ControlPanel.actions;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertAll;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.List;
import java.util.Set;

import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.impl.util.UtilImpl;
import org.skyve.impl.util.XMLMetaData;
import org.skyve.metadata.sail.language.Automation;
import org.skyve.metadata.sail.language.Automation.TestStrategy;
import org.skyve.metadata.user.User;
import org.skyve.web.UserAgentType;

import jakarta.inject.Inject;
import util.AbstractH2Test;

/**
 * Tests for the GenerateMenuSAIL action delegation methods.
 */
class GenerateMenuSAILH2Test extends AbstractH2Test {

	@Inject
	private GenerateMenuSAIL action;

	@Test
	void testSingleGeneratesAutomation() throws Exception {
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

	/**
	 * The generator's rule for a non-constant condition (M17), on the real admin and kitchen sink views, generating as
	 * the test's user (DevOps): a condition the two test instances agree shows the step leaves it in (the Control
	 * Panel's devOps tab and its GenerateTestData); one they agree hides it is a comment saying so (SendMail, as no SMTP
	 * host is configured); one they disagree on is a comment saying the step depends on what the script enters
	 * (ConditionsFixture's actions follow Show, true when new but false in test data); and a constant condition leaves
	 * the step out without a word (ActionsFixture's RunWithoutTrigger is invisible="true").
	 */
	@Test
	void conditionsDecideAsTheRuleSays() throws Exception {
		User currentUser = CORE.getPersistence().getUser();
		Set<String> twoFactorCustomers = UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS;
		// admin.Configuration's view reads it, and only a server's start-up sets it
		UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS = Set.of();
		try {
			String admin = XMLMetaData.marshalSAIL(action.single(currentUser, null, null, "admin", "external", UserAgentType.desktop, TestStrategy.Assert));
			String kitchenSink = XMLMetaData.marshalSAIL(action.single(currentUser, null, null, "kitchensink", "external", UserAgentType.desktop, TestStrategy.Assert));

			assertAll(() -> assertTrue(admin.contains("<ns2:tabSelect path=\"Design\"/>"), admin),
						() -> assertTrue(admin.contains("<ns2:action name=\"GenerateTestData\"/>"), admin),
						() -> assertTrue(admin.contains("<ns2:comment>not generated: action SendMail depends on visible=\"emailConfigured\", " +
															"which hides or disables it on both test instances</ns2:comment>"), admin),
						() -> assertTrue(kitchenSink.contains("<ns2:comment>not generated: action ConditionsAction1 depends on " +
																"visible=\"shownByExpression\", which depends on what the script enters</ns2:comment>"),
											kitchenSink),
						() -> assertFalse(kitchenSink.contains("RunWithoutTrigger"), kitchenSink),
						() -> assertTrue(kitchenSink.contains("<ns2:action name=\"RunFromToolbar\"/>"), kitchenSink));
		}
		finally {
			UtilImpl.TWO_FACTOR_AUTH_CUSTOMERS = twoFactorCustomers;
		}
	}

	@Test
	void testMultipleGeneratesAutomationList() throws Exception {
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
