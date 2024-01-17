package org.skyve;

import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;

import java.util.Arrays;
import java.util.Collection;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameter;
import org.junit.runners.Parameterized.Parameters;
import org.skyve.impl.util.UtilImpl;
import org.skyve.util.Util;

@RunWith(value = Parameterized.class)
public class EXTParameterisedTest {

	private static String shortPassword = "password";
	private static String longPassword = "G^`Nyp&n1@rqsOll+?Q6m9w^Q<+N5+(ShbB$\\\"9Ns)/pc)fvv}`hj9*wL\\\\YH<"
			+ "6x?G^`Nyp&n1@rqsOll+?Q6m9w^Q<+N5+(ShbB$\\\"9Ns)/pc)fvv}`hj9*wL\\\\YH<6x?G^`Nyp&n1@rqsOll+?Q6m9"
			+ "w^Q<+N5+(ShbB$\"9Ns)/pc)fvv}`hj9*wLYH<6x?G^`Nyp&n1@rqsOll+?Q6m9w^Q<+N5+(ShbB$"
			+ "\"9Ns)/pc)fvv}`hj9*wLYH<6 x?";

	@Parameter(value = 0)
	public String algorithm;
	@Parameter(value = 1)
	public String clearText;

	@Parameters(name = "{index}: testParse({0}) = {1}")
	public static Collection<String[]> data() {
		return Arrays.asList(new String[][] {
				{ "MD5", shortPassword },
				{ "MD5", longPassword },
				{ "SHA1", shortPassword },
				{ "SHA1", longPassword },
				{ "bcrypt", shortPassword },
				{ "bcrypt", longPassword },
				{ "pbkdf2", shortPassword },
				{ "pbkdf2", longPassword },
//				{ "scrypt", shortPassword },
//				{ "scrypt", longPassword },
		});
	}

	/**
	 * Tests that each hashedPassword input cleartext in the parameters list.
	 */
	@Test
	@SuppressWarnings("boxing")
	public void testHashPassword() {
		// setup the test data
		UtilImpl.PASSWORD_HASHING_ALGORITHM = algorithm;

		// validate the test data
		assertThat(Util.getPasswordHashingAlgorithm(), is(algorithm));

		// call the method under test
		String result = EXT.hashPassword(clearText);

		// verify the result
		System.out.println(String.format("%s (%d): %s (%d)", algorithm, clearText.length(), result, result.length()));
		assertThat("Encoded length should be less than 100 chars", result.length() <= 255, is(true));
		assertThat(result, is(not(clearText)));
	}
}
