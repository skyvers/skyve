package modules.admin.User;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;

import java.util.List;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.skyve.impl.sail.mock.MockWebContext;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.domain.Group;
import modules.admin.domain.User;
import modules.admin.domain.User.GroupSelection;
import util.AbstractH2Test;

/**
 * H2-backed tests for UserBizlet covering newInstance, preRerender,
 * preExecute, and getVariantDomainValues.
 */
public class UserBizletH2Test extends AbstractH2Test {

	private static final UserBizlet bizlet = new UserBizlet();

	private DataBuilder db;
	private UserExtension bean;
	private MockWebContext webContext;

	@BeforeEach
	void setup() {
		db = new DataBuilder().fixture(FixtureType.crud);
		bean = db.build(User.MODULE_NAME, User.DOCUMENT_NAME);
		webContext = new MockWebContext();
	}

	// ---- newInstance ----

	@Test
	void newInstanceSetsCreatedDateTime() throws Exception {
		UserExtension result = bizlet.newInstance(bean);
		assertNotNull(result);
		assertNotNull(result.getCreatedDateTime());
	}

	@Test
	void newInstanceSetsWizardState() throws Exception {
		UserExtension result = bizlet.newInstance(bean);
		assertNotNull(result);
		assertNotNull(result.getWizardState());
	}

	@Test
	void newInstanceSetsGroupSelection() throws Exception {
		UserExtension result = bizlet.newInstance(bean);
		assertNotNull(result);
		assertNotNull(result.getGroupSelection());
	}

	// ---- preRerender: groupSelection ----

	@Test
	void preRerenderGroupSelectionNewGroupCreatesNewGroup() throws Exception {
		bean.setGroupSelection(GroupSelection.newGroup);

		bizlet.preRerender(User.groupSelectionPropertyName, bean, webContext);

		assertNotNull(bean.getNewGroup());
	}

	@Test
	void preRerenderGroupSelectionExistingGroupsClearsNewGroup() throws Exception {
		bean.setGroupSelection(GroupSelection.existingGroups);
		bean.setNewGroup(Group.newInstance());

		bizlet.preRerender(User.groupSelectionPropertyName, bean, webContext);

		assertThat(bean.getNewGroup(), is(nullValue()));
	}

	@Test
	void preRerenderNewPasswordWithNullPasswordDoesNotThrow() throws Exception {
		bean.setNewPassword(null);

		bizlet.preRerender(User.newPasswordPropertyName, bean, webContext);
		// just should not throw
	}

	@Test
	void preRerenderNewPasswordWithValueDoesNotThrow() throws Exception {
		bean.setNewPassword("testPassword123");

		bizlet.preRerender(User.newPasswordPropertyName, bean, webContext);
		// just should not throw
	}

	@Test
	void preRerenderUnknownSourceDoesNotThrow() throws Exception {
		bizlet.preRerender("unknownSource", bean, webContext);
		// just should not throw
	}

	// ---- preExecute: Save ----

	@Test
	void preExecuteSaveWithNullNewGroupDoesNotAddGroup() throws Exception {
		bean.setNewGroup(null);
		int initialSize = bean.getGroups().size();

		UserExtension result = bizlet.preExecute(ImplicitActionName.Save, bean, null, webContext);
		assertNotNull(result);
		assertThat(result.getGroups().size(), is(initialSize));
	}

	@Test
	void preExecuteSaveWithNewGroupAddsGroupToList() throws Exception {
		modules.admin.Group.GroupExtension newGroup = Group.newInstance();
		newGroup.setName("TestGroup");
		bean.setNewGroup(newGroup);
		int initialSize = bean.getGroups().size();

		UserExtension result = bizlet.preExecute(ImplicitActionName.Save, bean, null, webContext);
		assertNotNull(result);
		assertThat(result.getGroups().size(), is(initialSize + 1));
	}

	// ---- getVariantDomainValues ----

	@Test
	void getVariantDomainValuesForGroupsReturnsList() throws Exception {
		List<DomainValue> result = bizlet.getVariantDomainValues(User.groupsPropertyName);
		assertThat(result, is(notNullValue()));
		// May be empty in test DB but should not throw
	}

	@Test
	void getVariantDomainValuesForDataGroupReturnsList() throws Exception {
		List<DomainValue> result = bizlet.getVariantDomainValues(User.dataGroupPropertyName);
		assertThat(result, is(notNullValue()));
	}

	@Test
	void getVariantDomainValuesForHomeModuleReturnsList() throws Exception {
		List<DomainValue> result = bizlet.getVariantDomainValues(User.homeModulePropertyName);
		assertThat(result, is(notNullValue()));
		assertFalse(result.isEmpty(), "Expected at least one accessible module");
	}

	@Test
	void getVariantDomainValuesForUnknownAttributeCallsSuper() throws Exception {
		List<DomainValue> result = bizlet.getVariantDomainValues("unknownAttribute");
		// super returns null or empty - just should not throw
	}
}
