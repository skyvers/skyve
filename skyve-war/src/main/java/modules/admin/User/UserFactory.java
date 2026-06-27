package modules.admin.User;

import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFactory;
import org.skyve.util.test.SkyveFixture;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.User.actions.Check;
import modules.admin.User.actions.GeneratePassword;
import modules.admin.User.actions.New;
import modules.admin.User.actions.Next;
import modules.admin.User.actions.ResendActivation;
import modules.admin.domain.Contact;
import modules.admin.domain.Group;
import modules.admin.domain.User;

/**
 * Creates test fixtures for admin {@link User} documents.
 */
@SkyveFactory(excludedActions = { Check.class, GeneratePassword.class, Next.class, ResendActivation.class,
		New.class }, excludedUpdateAttributes = { User.passwordLastChangedCountryCodePropertyName,
				User.passwordLastChangedCountryNamePropertyName })
public class UserFactory {
	/**
	 * Builds a CRUD-oriented user fixture including contact and at least one group.
	 *
	 * @return A test-ready user extension.
	 * @throws Exception If fixture construction fails.
	 */
	@SkyveFixture(types = FixtureType.crud)
	public static UserExtension crudInstance() {
		UserExtension user = new DataBuilder().factoryBuild(User.MODULE_NAME, User.DOCUMENT_NAME);
		user.setConfirmPassword(null);
		user.setGeneratedPassword(null);
		user.setNewPassword(null);

		user.setContact(new DataBuilder().build(Contact.MODULE_NAME, Contact.DOCUMENT_NAME));

		user.getGroups()
				.add(new DataBuilder()
						.build(Group.MODULE_NAME, Group.DOCUMENT_NAME));

		return user;
	}
}
