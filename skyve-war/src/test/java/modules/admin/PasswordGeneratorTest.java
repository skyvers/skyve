package modules.admin;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.jupiter.api.Assertions.assertNotEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;

import org.junit.jupiter.api.Test;

import util.AbstractH2Test;

class PasswordGeneratorTest extends AbstractH2Test {

	@Test
	@SuppressWarnings("static-method")
	void testGenerateReturnsNonNull() {
		String password = PasswordGenerator.generate();
		assertThat(password, is(notNullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	void testGenerateReturnsMinimumLength() {
		String password = PasswordGenerator.generate();
		assertTrue(password.length() >= 6, "Generated password must be at least 6 characters");
	}

	@Test
	@SuppressWarnings("static-method")
	void testGenerateContainsDigit() {
		// Run several times to account for randomness
		boolean found = false;
		for (int i = 0; i < 20; i++) {
			String password = PasswordGenerator.generate();
			for (char c : password.toCharArray()) {
				if (PasswordGenerator.DIGITS.indexOf(c) >= 0) {
					found = true;
					break;
				}
			}
			if (found) {
				break;
			}
		}
		assertTrue(found, "Generated password must contain at least one digit across multiple generations");
	}

	@Test
	@SuppressWarnings("static-method")
	void testGenerateContainsUppercase() {
		boolean found = false;
		for (int i = 0; i < 20; i++) {
			String password = PasswordGenerator.generate();
			for (char c : password.toCharArray()) {
				if (PasswordGenerator.UPPERCASE_CHARACTERS.indexOf(c) >= 0) {
					found = true;
					break;
				}
			}
			if (found) {
				break;
			}
		}
		assertTrue(found, "Generated password must contain at least one uppercase character across multiple generations");
	}

	@Test
	@SuppressWarnings("static-method")
	void testGenerateContainsLowercase() {
		boolean found = false;
		for (int i = 0; i < 20; i++) {
			String password = PasswordGenerator.generate();
			for (char c : password.toCharArray()) {
				if (PasswordGenerator.LOWERCASE_CHARACTERS.indexOf(c) >= 0) {
					found = true;
					break;
				}
			}
			if (found) {
				break;
			}
		}
		assertTrue(found, "Generated password must contain at least one lowercase character across multiple generations");
	}

	@Test
	@SuppressWarnings("static-method")
	void testGenerateContainsPunctuation() {
		boolean found = false;
		for (int i = 0; i < 20; i++) {
			String password = PasswordGenerator.generate();
			for (char c : password.toCharArray()) {
				if (PasswordGenerator.PUNCTUATION.indexOf(c) >= 0) {
					found = true;
					break;
				}
			}
			if (found) {
				break;
			}
		}
		assertTrue(found, "Generated password must contain at least one punctuation character across multiple generations");
	}

	@Test
	@SuppressWarnings("static-method")
	void testGenerateProducesDifferentPasswords() {
		String first = PasswordGenerator.generate();
		String second = PasswordGenerator.generate();
		// Two consecutive calls should not deterministically produce the same result
		// (astronomically unlikely with a real SecureRandom)
		assertNotEquals(first, second, "Consecutive generated passwords should differ");
	}
}
