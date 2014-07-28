package modules.admin;

public class PasswordGenerator {
	public static final String DIGITS = "0123456789";

	public static final String LOCASE_CHARACTERS = "abcdefghijklmnopqrstuvwxyz";

	public static final String UPCASE_CHARACTERS = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";

	public static final String PRINTABLE_CHARACTERS = DIGITS + LOCASE_CHARACTERS + UPCASE_CHARACTERS;

	public static String generate() throws Exception {
		StringBuilder result = new StringBuilder(6);
		result.append(UPCASE_CHARACTERS.charAt((int) (Math.random() * 26)));
		result.append(LOCASE_CHARACTERS.charAt((int) (Math.random() * 26)));
		result.append(DIGITS.charAt((int) (Math.random() * 10)));
		result.append(UPCASE_CHARACTERS.charAt((int) (Math.random() * 26)));
		result.append(LOCASE_CHARACTERS.charAt((int) (Math.random() * 26)));
		result.append(DIGITS.charAt((int) (Math.random() * 10)));

		return result.toString();
	}
}
