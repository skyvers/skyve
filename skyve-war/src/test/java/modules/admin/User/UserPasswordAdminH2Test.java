package modules.admin.User;

import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertNull;
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Objects;

import org.junit.jupiter.api.Test;
import org.skyve.CORE;
import org.skyve.EXT;
import org.skyve.domain.messages.ValidationException;
import org.skyve.impl.metadata.repository.LocalDataStoreRepository;
import org.skyve.persistence.Persistence;
import org.skyve.util.DataBuilder;
import org.skyve.util.test.SkyveFixture.FixtureType;

import modules.admin.User.actions.GeneratePassword;
import modules.admin.domain.User;
import modules.admin.domain.UserRole;
import util.AbstractH2Test;

/**
 * An administrator setting a user's password: it is stored as a hash, must be complex enough and the user
 * name must be new; a generated password is hashed and forces the user to change it at their next sign in.
 */
@SuppressWarnings("static-method")
class UserPasswordAdminH2Test extends AbstractH2Test {
	private static final String GOOD = "Admin0Set0Password!";

	@Test
	void aNewPasswordIsStoredAsAHashAndNeverAsThePlainText() {
		UserExtension user = newUser("hash");
		user.setNewPassword(GOOD);
		user.setConfirmPassword(GOOD);

		UserExtension saved = CORE.getPersistence().save(user);

		assertNotEquals(GOOD, saved.getPassword());
		assertTrue(EXT.checkPassword(GOOD, saved.getPassword()));
		assertNull(saved.getNewPassword(), "the typed password is not kept on the bean");
		assertNull(saved.getConfirmPassword());
	}

	@Test
	void aPasswordThatIsNotComplexEnoughIsRefusedAndTheUserIsNotCreated() {
		UserExtension user = newUser("weak");
		user.setNewPassword("abc");
		user.setConfirmPassword("abc");
		Persistence p = CORE.getPersistence();

		assertThrows(ValidationException.class, () -> p.save(user));
		assertFalse(user.isPersisted());
	}

	@Test
	void aMismatchedConfirmationIsRefused() {
		UserExtension user = newUser("mismatch");
		user.setNewPassword(GOOD);
		user.setConfirmPassword(GOOD + "x");
		Persistence p = CORE.getPersistence();

		assertThrows(ValidationException.class, () -> p.save(user));
	}

	@Test
	void aUserNameAlreadyInUseIsRefused() {
		UserExtension first = newUser("dupe");
		first.setNewPassword(GOOD);
		first.setConfirmPassword(GOOD);
		Persistence p = CORE.getPersistence();
		p.save(first);
		UserExtension second = newUser("dupe-other");
		second.setUserName(first.getUserName());
		second.setNewPassword(GOOD);
		second.setConfirmPassword(GOOD);

		assertThrows(ValidationException.class, () -> p.save(second));
	}

	@Test
	void aGeneratedPasswordIsStoredHashedAndForcesAChangeAtTheNextSignIn() {
		UserExtension user = newUser("generated");
		Persistence p = CORE.getPersistence();
		try {
			new GeneratePassword().execute(user, null);
		} catch (Exception e) {
			throw new IllegalStateException(e);
		}
		String generated = user.getGeneratedPassword();
		assertNotNull(generated);

		UserExtension saved = p.save(user);
		p.commit(false);
		p.begin();

		assertNotEquals(generated, saved.getPassword());
		assertTrue(EXT.checkPassword(generated, saved.getPassword()));
		assertTrue(Objects.requireNonNull(new LocalDataStoreRepository().retrieveUser(CUSTOMER + '/' + user.getUserName()))
				.isPasswordChangeRequired());
	}

	@Test
	void anAdministratorWhoTypesADifferentPasswordInsteadOfTheGeneratedOneDoesNotForceAChange() {
		UserExtension user = newUser("typed");
		Persistence p = CORE.getPersistence();
		try {
			new GeneratePassword().execute(user, null);
		} catch (Exception e) {
			throw new IllegalStateException(e);
		}
		user.setNewPassword(GOOD);
		user.setConfirmPassword(GOOD);

		UserExtension saved = p.save(user);
		p.commit(false);
		p.begin();

		assertTrue(EXT.checkPassword(GOOD, saved.getPassword()));
		assertFalse(Objects.requireNonNull(new LocalDataStoreRepository().retrieveUser(CUSTOMER + '/' + user.getUserName()))
				.isPasswordChangeRequired());
	}

	private static UserExtension newUser(String prefix) {
		UserExtension result = new DataBuilder().fixture(FixtureType.crud).build(User.MODULE_NAME, User.DOCUMENT_NAME);
		result.setUserName("admin.pw." + prefix + "." + System.nanoTime());
		result.setInactive(Boolean.FALSE);
		result.setPasswordExpired(Boolean.FALSE);
		result.setPassword(null);
		result.setNewPassword(null);
		result.setConfirmPassword(null);
		result.setGeneratedPassword(null);
		result.getGroups().clear();
		result.getRoles().clear();
		UserRole role = UserRole.newInstance();
		role.setRoleName("admin.BasicUser");
		result.addRolesElement(role);
		return result;
	}
}
