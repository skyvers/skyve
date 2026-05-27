package modules.admin.User;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.function.Executable;
import org.skyve.CORE;
import org.skyve.domain.messages.DomainException;
import org.skyve.metadata.MetaDataException;
import org.skyve.persistence.DocumentQuery;
import org.skyve.util.DataBuilder;
import org.skyve.util.Util;
import org.skyve.util.test.SkyveFixture;

import jakarta.annotation.Nonnull;
import jakarta.inject.Inject;
import modules.admin.Contact.ContactExtension;
import modules.admin.Group.GroupExtension;
import modules.admin.domain.Contact;
import modules.admin.domain.Group;
import modules.admin.domain.User;
import modules.admin.domain.UserProxy;
import util.AbstractH2Test;
import org.skyve.domain.messages.ValidationException;

class UserServiceH2Test extends AbstractH2Test {
	private DataBuilder db;
	private ContactExtension contact = null;
	@Inject
	private transient UserService userService;

	@BeforeEach
	void setUpTests() {
		db = new DataBuilder().fixture(SkyveFixture.FixtureType.crud);
	}

	@Test
	void createAdminUserFromContactWithGroupShouldFailIfNoContact() {
		// given
		contact = null;

		// when
		Executable executable = () -> userService.createAdminUserFromContactWithGroup(contact, "groupName", "homeModuleName",
				false);

		// then
		DomainException e = assertThrows(DomainException.class, executable);
		assertThat(e.getMessage(),
				is(Util.nullSafeI18n("admin.modulesUtils.createAdminUserFromContactWithGroup.exception.contact")));
	}

	@Test
	void createAdminUserFromContactWithGroupShouldFailIfNoGroupName() {
		// given
		contact = Contact.newInstance();

		// when
		Executable executable = () -> userService.createAdminUserFromContactWithGroup(contact, null, "homeModuleName", false);

		// then
		DomainException e = assertThrows(DomainException.class, executable);
		assertThat(e.getMessage(),
				is(Util.nullSafeI18n("admin.modulesUtils.createAdminUserFromContactWithGroup.exception.groupName")));
	}

	@Test
	void createAdminUserFromContactWithGroupShouldFailIfNoValidGroupName() {
		// given
		contact = Contact.newInstance();
		GroupExtension group = Group.newInstance();
		group.setName("Admin");

		// when
		Executable executable = () -> userService.createAdminUserFromContactWithGroup(contact, "Admin", "homeModuleName", false);

		// then
		DomainException e = assertThrows(DomainException.class, executable);
		assertThat(e.getMessage(),
				is(Util.nullSafeI18n("admin.modulesUtils.createAdminUserFromContactWithGroup.exception.invalidGroup")));
	}

	@Test
	@SuppressWarnings("java:S1854") // Persistence.save() may return a different instance; assignment captured intentionally
	void createAdminUserFromContactWithGroupShouldFailIfNoValidHomeModuleName() {
		// given
		contact = db.build(Contact.MODULE_NAME, Contact.DOCUMENT_NAME);

		String homeModuleName = "homeModuleName";

		GroupExtension group = db.build(Group.MODULE_NAME, Group.DOCUMENT_NAME);
		group.setName("Admin");
		group = CORE.getPersistence().save(group);

		// when
		Executable executable = () -> userService.createAdminUserFromContactWithGroup(contact, "Admin", homeModuleName, false);

		// then
		MetaDataException e = assertThrows(MetaDataException.class, executable);
		assertThat(e.getMessage(), containsString("Module " + homeModuleName + " does not exist"));
	}

	@Test
	@SuppressWarnings("java:S1854") // Persistence.save() may return a different instance; assignment captured intentionally
	void createAdminUserFromContactWithGroupShould() {
		// given
		contact = db.build(Contact.MODULE_NAME, Contact.DOCUMENT_NAME);

		GroupExtension group = db.build(Group.MODULE_NAME, Group.DOCUMENT_NAME);
		group.setName("TestGroup");
		group = CORE.getPersistence().save(group);

		// when
		userService.createAdminUserFromContactWithGroup(contact, "TestGroup", Contact.MODULE_NAME, false);

		// then
		DocumentQuery q = CORE.getPersistence().newDocumentQuery(User.MODULE_NAME, User.DOCUMENT_NAME);
		q.getFilter().addEquals(UserProxy.userNamePropertyName, contact.getEmail1());
		@Nonnull UserExtension result = q.beanResult();

		assertThat(result, is(notNullValue()));
		assertThat(result.getContact(), is(contact));
	}

	@Test
	void validateGroupsAddsErrorWhenActiveUserHasNoRolesOrGroups() {
		UserExtension user = db.build(User.MODULE_NAME, User.DOCUMENT_NAME);
		user.setInactive(Boolean.FALSE);
		user.getRoles().clear();
		user.getGroups().clear();

		ValidationException e = new ValidationException();
		userService.validateGroups(user, e);
		assertFalse(e.getMessages().isEmpty());
	}

	@Test
	void validateGroupsNoErrorWhenUserIsInactive() {
		UserExtension user = db.build(User.MODULE_NAME, User.DOCUMENT_NAME);
		user.setInactive(Boolean.TRUE);
		user.getRoles().clear();
		user.getGroups().clear();

		ValidationException e = new ValidationException();
		userService.validateGroups(user, e);
		assertTrue(e.getMessages().isEmpty());
	}

	@Test
	void getCustomerRoleValuesReturnsNonNullList() {
		assertNotNull(userService.getCustomerRoleValues(CORE.getUser()));
	}

	@Test
	void evictUserProxyDoesNotThrowForNewBean() {
		UserExtension user = Assertions.assertDoesNotThrow(() -> db.build(User.MODULE_NAME, User.DOCUMENT_NAME));
		// Just verify the method doesn't throw
		userService.evictUserProxy(user);
	}

	@Test
	void currentAdminUserIsInDataGroupReturnsBooleanWithoutException() {
		// The test H2 user is typically not in a data group
		assertFalse(userService.currentAdminUserIsInDataGroup());
	}

	@Test
	void nextFromConfirmContactThrowsValidationException() {
		UserExtension user = db.build(User.MODULE_NAME, User.DOCUMENT_NAME);
		user.setWizardState(modules.admin.domain.User.WizardState.confirmContact);

		assertThrows(ValidationException.class, () -> userService.next(user));
	}
}
