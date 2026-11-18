package modules.admin.User;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.CoreMatchers.nullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.mockito.Mockito.mock;

import java.util.List;
import java.util.Objects;
import java.util.UUID;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;
import org.skyve.CORE;
import org.skyve.domain.types.Timestamp;
import org.skyve.impl.sail.mock.MockWebContext;
import org.skyve.metadata.controller.ImplicitActionName;
import org.skyve.metadata.model.document.Bizlet.DomainValue;
import org.skyve.persistence.Persistence;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.Contact.ContactExtension;
import modules.admin.domain.DataGroup;
import modules.admin.domain.Group;
import modules.admin.domain.User;
import modules.admin.domain.User.GroupSelection;
import util.AbstractH2Test;

/**
 * H2-backed tests for UserBizlet covering newInstance, preRerender,
 * preExecute, and getVariantDomainValues.
 */
@SuppressWarnings("static-method")
class UserBizletH2Test extends AbstractH2Test {

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
		Assertions.assertDoesNotThrow(() -> bean.setNewPassword(null));
		bizlet.preRerender(User.newPasswordPropertyName, bean, webContext);
		// just should not throw
	}

	@Test
	void preRerenderNewPasswordWithValueDoesNotThrow() throws Exception {
		Assertions.assertDoesNotThrow(() -> bean.setNewPassword("testPassword123"));
		bizlet.preRerender(User.newPasswordPropertyName, bean, webContext);
		// just should not throw
	}

	@Test
	void preRerenderUnknownSourceDoesNotThrow() {
		Assertions.assertDoesNotThrow(() -> bizlet.preRerender("unknownSource", bean, webContext));
		// just should not throw
	}

	// ---- preExecute: Save ----

	@Test
	void preExecuteSaveWithNullNewGroupDoesNotAddGroup() throws Exception {
		bean.setNewGroup(null);
		int initialSize = bean.getGroups().size();

		UserExtension result = bizlet.preExecute(ImplicitActionName.Save, bean, null, webContext);
		assertNotNull(result);
		assertEquals(initialSize, result.getGroups().size());
	}

	@Test
	void preExecuteSaveWithNewGroupAddsGroupToList() throws Exception {
		modules.admin.Group.GroupExtension newGroup = Group.newInstance();
		newGroup.setName("TestGroup");
		bean.setNewGroup(newGroup);
		int initialSize = bean.getGroups().size();

		UserExtension result = bizlet.preExecute(ImplicitActionName.Save, bean, null, webContext);
		assertNotNull(result);
		assertEquals(initialSize + 1, result.getGroups().size());
	}

	// ---- preSave ----

	@Test
	void preSaveSetsUserAndContactBizUserIdToUserBizId() throws Exception {
		bean.setBizId("user-owned-id");
		ContactExtension contact = bean.getContact();
		assertNotNull(contact);
		bean.setBizUserId("previous-owner");
		contact.setBizUserId("previous-contact-owner");

		bizlet.preSave(bean);

		assertEquals("user-owned-id", bean.getBizUserId());
		assertEquals("user-owned-id", contact.getBizUserId());
	}

	/**
	 * A password change keeps removing every remember-me series of the user, on all devices, and no other user's.
	 * (Signing out removes only one series: see {@code SkyveRememberMeServicesTest}.)
	 */
	@Test
	void postSaveAfterAPasswordChangeRemovesAllTheUsersRememberMeTokens() throws Exception {
		Persistence p = CORE.getPersistence();
		String userName = bean.getBizCustomer() + '/' + bean.getUserName();
		String otherUserName = bean.getBizCustomer() + "/someoneElse";
		for (String[] row : new String[][] { { userName, "phone" }, { userName, "tablet" }, { userName, "pc" },
				{ otherUserName, "pc2" } }) {
			p.newSQL(
					"insert into ADM_UserToken (bizId, bizVersion, bizLock, bizKey, bizCustomer, bizUserId, userName, series, token, lastUsed) "
							+ "values (:bizId, 0, '20260101000000000admin', :userName, :customer, 'SYSTEM', :userName, :series, 'token', :lastUsed)")
					.putParameter("bizId", UUID.randomUUID().toString(), false)
					.putParameter("userName", row[0], false)
					.putParameter("customer", bean.getBizCustomer(), false)
					.putParameter("series", row[1], false)
					.putParameter("lastUsed", new Timestamp())
					.execute();
		}
		CORE.getStash().put("passwordChanged", Boolean.TRUE);
		UserBizlet injected = new UserBizlet();
		java.lang.reflect.Field userService = UserBizlet.class.getDeclaredField("userService");
		userService.setAccessible(true);
		userService.set(injected, mock(UserService.class));

		injected.postSave(bean);

		assertEquals(0, tokenCount(p, userName));
		assertEquals(1, tokenCount(p, otherUserName));
	}

	private static int tokenCount(Persistence p, String userName) {
		Number count = p.newSQL("select count(*) from ADM_UserToken where userName = :userName")
				.putParameter("userName", userName, false)
				.scalarResult(Number.class);
		return Objects.requireNonNull(count).intValue();
	}

	@Test
	void preSaveSetsContactBizDataGroupIdFromUsersDataGroup() throws Exception {
		ContactExtension contact = bean.getContact();
		assertNotNull(contact);
		DataGroup dataGroup = DataGroup.newInstance();
		dataGroup.setBizId("data-group-id");
		bean.setDataGroup(dataGroup);
		contact.setBizDataGroupId("previous-data-group");

		bizlet.preSave(bean);

		assertEquals("data-group-id", contact.getBizDataGroupId());
	}

	@Test
	void preSaveClearsContactBizDataGroupIdWhenUserHasNoDataGroup() throws Exception {
		ContactExtension contact = bean.getContact();
		assertNotNull(contact);
		bean.setDataGroup(null);
		contact.setBizDataGroupId("previous-data-group");

		bizlet.preSave(bean);

		assertThat(contact.getBizDataGroupId(), is(nullValue()));
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
		assertNotNull(result);
		assertFalse(result.isEmpty(), "Expected at least one accessible module");
	}

	@Test
	void getVariantDomainValuesForUnknownAttributeCallsSuper() {
		Assertions.assertDoesNotThrow(() -> bizlet.getVariantDomainValues("unknownAttribute"));
		// super returns null or empty - just should not throw
	}
}
