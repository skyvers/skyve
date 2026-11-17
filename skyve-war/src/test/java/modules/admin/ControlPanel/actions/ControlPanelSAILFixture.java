package modules.admin.ControlPanel.actions;

import java.util.List;

import org.skyve.CORE;
import org.skyve.persistence.Persistence;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.ControlPanel.ControlPanelExtension;
import modules.admin.User.UserExtension;
import modules.admin.UserProxy.UserProxyExtension;
import modules.admin.domain.ControlPanel;
import modules.admin.domain.ControlPanel.SailExecutor;
import modules.admin.domain.User;
import modules.admin.domain.UserProxy;
import modules.admin.domain.UserRole;

/**
 * A Control Panel ready to execute SAIL, for the H2 tests of its SAIL actions. Its SAIL user is saved and committed (the
 * actions retrieve the user through the repository's own connection), so the tests that use it truncate afterwards.
 */
final class ControlPanelSAILFixture {
	static final String BASE_URL = "http://localhost:8080/skyve/";

	/**
	 * Uses most of what the PrimeFaces generator emits: sign in and out, a comment and messages with quotes, list and
	 * edit navigation, a list grid's New (which a list view's Vue grid cannot have generated), tabs, text, check box
	 * and combo entry, Save and a view action, success and failure tests (with and without a message), a pause, a
	 * lookup's row and a (PrimeFaces data table) list grid's and a data grid's row.
	 */
	static final String AUTOMATION = """
			<automation xmlns="http://www.skyve.org/xml/sail" uxui="tablet" userAgentType="tablet" testStrategy="Assert">
				<before>
					<login user="TestUser" password="pass&quot;word" />
					<testSuccess />
				</before>
				<interaction name="Contacts and users">
					<method>
						<comment>A "quoted" comment with a \\ backslash</comment>
						<navigateList module="admin" document="Contact" />
						<listGridNew module="admin" document="Contact" />
						<navigateEdit module="admin" document="Contact" />
						<dataEnter binding="name" value="SAIL &quot;Contact&quot; {run}" />
						<dataEnter binding="contactType" value="1" />
						<save />
						<testSuccess />
						<testFailure />
						<testFailure message="A &quot;required&quot; value" />
						<pause millis="1" />
						<navigateEdit module="admin" document="ControlPanel" />
						<tabSelect path="Instrumentation" />
						<dataEnter binding="queryTrace" value="true" />
						<tabSelect path="Design" />
						<dataEnter binding="query" value="select bean from {admin.Contact} as bean" />
						<action name="ExecuteQuery" />
						<tabSelect path="SAIL" />
						<lookupDescriptionPick binding="sailUser" row="0" />
						<navigateEdit module="kitchensink" document="ListGridFixture" />
						<listGridSelect module="kitchensink" query="qListGridFixtureFaces" row="0" />
						<navigateEdit module="admin" document="User" />
						<tabSelect path="Roles" />
						<dataGridSelect binding="roles" row="0" />
					</method>
				</interaction>
				<after>
					<logout />
				</after>
			</automation>
			""";

	private ControlPanelSAILFixture() {
		// static only
	}

	/**
	 * Returns a Control Panel with a SAIL user, base URL, builders, executor and {@link #AUTOMATION}.
	 *
	 * <p>Side effects: saves and commits a user with the roles the automation's views need.
	 */
	static ControlPanelExtension controlPanel() {
		ControlPanelExtension result = new DataBuilder().fixture(FixtureType.crud).build(ControlPanel.MODULE_NAME, ControlPanel.DOCUMENT_NAME);
		result.setSailUser(sailUser());
		result.setSailBaseUrl(BASE_URL);
		result.setSailComponentBuilder("org.skyve.impl.web.faces.pipeline.component.SkyveComponentBuilderChain");
		result.setSailLayoutBuilder("org.skyve.impl.web.faces.pipeline.layout.ResponsiveLayoutBuilder");
		result.setSailExecutor(SailExecutor.primeFacesInlineWebDriver);
		result.setSail(AUTOMATION);
		return result;
	}

	/**
	 * Saves and commits a user with the roles the automation's views and the kitchen sink menu need, since the actions
	 * generate as the Control Panel's SAIL user, which they retrieve from the repository (through its own connection).
	 *
	 * @return	the user's proxy
	 */
	private static UserProxyExtension sailUser() {
		UserExtension user = new DataBuilder().fixture(FixtureType.crud).build(User.MODULE_NAME, User.DOCUMENT_NAME);
		user.setUserName("sail.generated." + System.nanoTime());
		user.setInactive(Boolean.FALSE);
		user.getGroups().clear();
		user.getRoles().clear();
		for (String roleName : List.of("admin.BasicUser", "admin.DevOps", "admin.SecurityAdministrator", "kitchensink.dev")) {
			UserRole role = UserRole.newInstance();
			role.setRoleName(roleName);
			user.addRolesElement(role);
		}
		Persistence persistence = CORE.getPersistence();
		user = persistence.save(user);
		persistence.commit(false);
		persistence.begin();
		return persistence.retrieve(UserProxy.MODULE_NAME, UserProxy.DOCUMENT_NAME, user.getBizId());
	}
}
