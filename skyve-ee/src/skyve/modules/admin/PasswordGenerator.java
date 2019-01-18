package modules.admin;

import org.skyve.domain.messages.Message;
import org.skyve.domain.messages.ValidationException;

import modules.admin.domain.Configuration;
import modules.admin.domain.Configuration.PasswordComplexityModel;

public class PasswordGenerator {
	public static final String DIGITS = "0123456789";

	public static final String LOWERCASE_CHARACTERS = "abcdefghijklmnopqrstuvwxyz";

	public static final String UPPERCASE_CHARACTERS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

	public static final String PUNCTUATION = "~!@#$%^&*(){}[]-_+=:;<,>.?"; //excluded because they are hard to read - |/\\"'`

	public static final String PRINTABLE_CHARACTERS = DIGITS + LOWERCASE_CHARACTERS + UPPERCASE_CHARACTERS;

	public static String generate() throws Exception {

		Configuration config = Configuration.newInstance();
		PasswordComplexityModel m = config.getPasswordComplexityModel();

		if (m == null) {
			throw new ValidationException(new Message("Passwords cannot be generated - the Password complexity model has not been selected in the system configuration"));
		}

		// construct a reasonable password
		StringBuilder result = new StringBuilder(6);
		result.append(PUNCTUATION.charAt((int) (Math.random() * PUNCTUATION.length())));
		result.append(UPPERCASE_CHARACTERS.charAt((int) (Math.random() * UPPERCASE_CHARACTERS.length())));
		result.append(LOWERCASE_CHARACTERS.charAt((int) (Math.random() * LOWERCASE_CHARACTERS.length())));
		result.append(DIGITS.charAt((int) (Math.random() * DIGITS.length())));

		for (int i = 0; i < 6; i++) {
			int r = (int) (Math.random() * 4);
			// and randomize other characters
			if (r > 2) {
				result.append(PUNCTUATION.charAt((int) (Math.random() * PUNCTUATION.length())));
			} else if (r > 1) {
				result.append(UPPERCASE_CHARACTERS.charAt((int) (Math.random() * UPPERCASE_CHARACTERS.length())));
			} else if (r > 0) {
				result.append(LOWERCASE_CHARACTERS.charAt((int) (Math.random() * LOWERCASE_CHARACTERS.length())));
			} else {
				result.append(DIGITS.charAt((int) (Math.random() * DIGITS.length())));
			}
		}

		return result.toString();
	}
}
