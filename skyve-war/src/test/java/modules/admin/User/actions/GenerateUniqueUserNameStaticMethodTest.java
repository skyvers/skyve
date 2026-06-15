package modules.admin.User.actions;

import static org.junit.jupiter.api.Assertions.assertThrows;

import org.junit.jupiter.api.Test;
import org.skyve.domain.messages.ValidationException;

import modules.admin.User.UserExtension;

@SuppressWarnings("static-method")
class GenerateUniqueUserNameStaticMethodTest {

	@Test
	void generateUniqueUserNameWithNullContactThrowsValidationException() {
		UserExtension user = new UserExtension();
		// contact is null by default
		assertThrows(ValidationException.class, () -> GenerateUniqueUserName.generateUniqueUserNameFromContactName(user));
	}
}
