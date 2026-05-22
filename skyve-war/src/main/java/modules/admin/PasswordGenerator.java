package modules.admin;

import java.security.SecureRandom;

import modules.admin.Configuration.ConfigurationExtension;
import modules.admin.domain.Configuration;

public class PasswordGenerator {
	private static final SecureRandom SECURE_RANDOM = new SecureRandom();

	private PasswordGenerator() {
		// utility class
	}

	public static final String DIGITS = "0123456789";

	public static final String LOWERCASE_CHARACTERS = "abcdefghijklmnopqrstuvwxyz";

	public static final String UPPERCASE_CHARACTERS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

	public static final String PUNCTUATION = "~!@#$%^&*(){}[]-_+=:;<,>.?"; //excluded because they are hard to read - |/\\"'`

	public static final String PRINTABLE_CHARACTERS = DIGITS + LOWERCASE_CHARACTERS + UPPERCASE_CHARACTERS;

	public static String generate() throws Exception {

		ConfigurationExtension config = Configuration.newInstance();

		// construct a reasonable password
		StringBuilder result = new StringBuilder(config.getPasswordMinLength().intValue());
		result.append(PUNCTUATION.charAt(SECURE_RANDOM.nextInt(PUNCTUATION.length())));
		result.append(UPPERCASE_CHARACTERS.charAt(SECURE_RANDOM.nextInt(UPPERCASE_CHARACTERS.length())));
		result.append(LOWERCASE_CHARACTERS.charAt(SECURE_RANDOM.nextInt(LOWERCASE_CHARACTERS.length())));
		result.append(DIGITS.charAt(SECURE_RANDOM.nextInt(DIGITS.length())));

		// generate at least a length 6 string, or the min length, whichever is greater
		int minLength = (config.getPasswordMinLength() == null || config.getPasswordMinLength().intValue() < 6) ? 6
				: config.getPasswordMinLength().intValue();

		for (int i = 0; i < minLength; i++) {
			int r = SECURE_RANDOM.nextInt(4);
			// and randomize other characters
			if (r > 2) {
				result.append(PUNCTUATION.charAt(SECURE_RANDOM.nextInt(PUNCTUATION.length())));
			} else if (r > 1) {
				result.append(UPPERCASE_CHARACTERS.charAt(SECURE_RANDOM.nextInt(UPPERCASE_CHARACTERS.length())));
			} else if (r > 0) {
				result.append(LOWERCASE_CHARACTERS.charAt(SECURE_RANDOM.nextInt(LOWERCASE_CHARACTERS.length())));
			} else {
				result.append(DIGITS.charAt(SECURE_RANDOM.nextInt(DIGITS.length())));
			}
		}

		return result.toString();
	}
}
