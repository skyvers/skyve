package modules.admin;

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertTrue;

import org.junit.Test;

import util.AbstractH2TestForJUnit4;

public class PasswordGeneratorTest extends AbstractH2TestForJUnit4 {

	@Test
	@SuppressWarnings("static-method")
	public void testGenerateReturnsNonNull() throws Exception {
		String password = PasswordGenerator.generate();
		assertThat(password, is(notNullValue()));
	}

	@Test
	@SuppressWarnings("static-method")
	public void testGenerateReturnsMinimumLength() throws Exception {
		String password = PasswordGenerator.generate();
		assertTrue("Generated password must be at least 6 characters", password.length() >= 6);
	}

	@Test
	@SuppressWarnings("static-method")
	public void testGenerateContainsDigit() throws Exception {
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
		assertTrue("Generated password must contain at least one digit across multiple generations", found);
	}

	@Test
	@SuppressWarnings("static-method")
	public void testGenerateContainsUppercase() throws Exception {
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
		assertTrue("Generated password must contain at least one uppercase character across multiple generations", found);
	}

	@Test
	@SuppressWarnings("static-method")
	public void testGenerateContainsLowercase() throws Exception {
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
		assertTrue("Generated password must contain at least one lowercase character across multiple generations", found);
	}

	@Test
	@SuppressWarnings("static-method")
	public void testGenerateContainsPunctuation() throws Exception {
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
		assertTrue("Generated password must contain at least one punctuation character across multiple generations", found);
	}

	@Test
	@SuppressWarnings("static-method")
	public void testGenerateProducesDifferentPasswords() throws Exception {
		String first = PasswordGenerator.generate();
		String second = PasswordGenerator.generate();
		// Two consecutive calls should not deterministically produce the same result
		// (astronomically unlikely with a real SecureRandom)
		assertNotEquals("Consecutive generated passwords should differ", first, second);
	}
}
